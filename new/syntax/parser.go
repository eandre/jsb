// Copyright 2016 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package syntax

import (
	"fmt"
	"io"
	"strings"

	"github.com/eandre/jsb/new/src"
)

const debug = false
const trace = false

type parser struct {
	base *src.PosBase
	errh ErrorHandler
	scanner

	first error // first error encountered
	prec  int

	fnest  int    // function nesting level (for error handling)
	xnest  int    // expression nesting level (for complit ambiguity resolution)
	indent []byte // tracing support
}

func (p *parser) init(base *src.PosBase, r io.Reader, errh ErrorHandler) {
	p.base = base
	p.errh = errh
	p.scanner.init(
		r,
		func(line, col uint, msg string) {
			p.error_at(p.pos_at(line, col), msg)
		},
	)

	p.first = nil

	p.fnest = 0
	p.xnest = 0
	p.indent = nil
}

const lineMax = 1<<24 - 1 // TODO(gri) this limit is defined for src.Pos - fix

func (p *parser) got(tok token) bool {
	if p.tok == tok {
		p.next()
		return true
	}
	return false
}

func (p *parser) want(tok token) {
	if !p.got(tok) {
		p.syntax_error("expecting " + tok.String())
		p.advance()
	}
}

// ----------------------------------------------------------------------------
// Error handling

// pos_at returns the Pos value for (line, col) and the current position base.
func (p *parser) pos_at(line, col uint) src.Pos {
	return src.MakePos(p.base, line, col)
}

// error reports an error at the given position.
func (p *parser) error_at(pos src.Pos, msg string) {
	err := Error{pos, msg}
	if p.first == nil {
		p.first = err
	}
	if p.errh == nil {
		panic(p.first)
	}
	p.errh(err)
}

// syntax_error_at reports a syntax error at the given position.
func (p *parser) syntax_error_at(pos src.Pos, msg string) {
	if trace {
		defer p.trace("syntax_error (" + msg + ")")()
	}

	if p.tok == _EOF && p.first != nil {
		return // avoid meaningless follow-up errors
	}

	// add punctuation etc. as needed to msg
	switch {
	case msg == "":
		// nothing to do
	case strings.HasPrefix(msg, "in"), strings.HasPrefix(msg, "at"), strings.HasPrefix(msg, "after"):
		msg = " " + msg
	case strings.HasPrefix(msg, "expecting"):
		msg = ", " + msg
	default:
		// plain error - we don't care about current token
		p.error_at(pos, "syntax error: "+msg)
		return
	}

	// determine token string
	var tok string
	switch p.tok {
	case _Name, _Semi:
		tok = p.lit
	case _Literal:
		tok = "literal " + p.lit
	case _Operator:
		tok = p.op.String()
	case _AssignOp:
		tok = p.op.String() + "="
	case _IncOp:
		tok = p.op.String()
		tok += tok
	default:
		tok = tokstring(p.tok)
	}

	p.error_at(pos, "syntax error: unexpected "+tok+msg)
}

// Convenience methods using the current token position.
func (p *parser) pos() src.Pos            { return p.pos_at(p.line, p.col) }
func (p *parser) error(msg string)        { p.error_at(p.pos(), msg) }
func (p *parser) syntax_error(msg string) { p.syntax_error_at(p.pos(), msg) }

// The stopset contains keywords that start a statement.
// They are good synchronization points in case of syntax
// errors and (usually) shouldn't be skipped over.
// TODO update
const stopset uint64 = 1<<_Break |
	1<<_Catch |
	1<<_Const |
	1<<_Continue |
	1<<_For |
	1<<_Function |
	1<<_If |
	1<<_Return |
	1<<_Switch |
	1<<_Var

// Advance consumes tokens until it finds a token of the stopset or followlist.
// The stopset is only considered if we are inside a function (p.fnest > 0).
// The followlist is the list of valid tokens that can follow a production;
// if it is empty, exactly one token is consumed to ensure progress.
func (p *parser) advance(followlist ...token) {
	if len(followlist) == 0 {
		p.next()
		return
	}

	// compute follow set
	// (not speed critical, advance is only called in error situations)
	var followset uint64 = 1 << _EOF // never skip over EOF
	for _, tok := range followlist {
		followset |= 1 << tok
	}

	for !(contains(followset, p.tok) || p.fnest > 0 && contains(stopset, p.tok)) {
		p.next()
	}
}

func tokstring(tok token) string {
	switch tok {
	case _EOF:
		return "EOF"
	case _Comma:
		return "comma"
	case _Semi:
		return "semicolon"
	}
	return tok.String()
}

// usage: defer p.trace(msg)()
func (p *parser) trace(msg string) func() {
	fmt.Printf("%5d: %s%s (\n", p.line, p.indent, msg)
	const tab = ". "
	p.indent = append(p.indent, tab...)
	return func() {
		p.indent = p.indent[:len(p.indent)-len(tab)]
		if x := recover(); x != nil {
			panic(x) // skip print_trace
		}
		fmt.Printf("%5d: %s)\n", p.line, p.indent)
	}
}

// ----------------------------------------------------------------------------
// Package files
//
// Parse methods are annotated with matching Go productions as appropriate.
// The annotations are intended as guidelines only since a single Go grammar
// rule may be covered by multiple parse methods and vice versa.
//
// Excluding methods returning slices, parse methods named xOrNil may return
// nil; all others are expected to return a valid non-nil node.

// SourceFile = PackageClause ";" { ImportDecl ";" } { TopLevelDecl ";" } .
func (p *parser) fileOrNil() *File {
	if trace {
		defer p.trace("file")()
	}

	f := new(File)
	f.pos = p.pos()

	// PackageClause
	if !p.got(_Package) {
		p.syntax_error("package statement must be first")
		return nil
	}
	f.PkgName = p.name()
	p.want(_Semi)

	// don't bother continuing if package clause has errors
	if p.first != nil {
		return nil
	}

	// { ImportDecl ";" }
	for p.got(_Import) {
		f.DeclList = append(f.DeclList, p.importDecl())
		p.want(_Semi)
	}

	// { TopLevelDecl ";" }
	for p.tok != _EOF {
		switch p.tok {
		case _Const:
			p.next()
			f.DeclList = append(f.DeclList, p.constDecl())

		case _Var:
			p.next()
			f.DeclList = append(f.DeclList, p.varDecl())

		case _Function:
			p.next()
			f.DeclList = append(f.DeclList, p.funcDecl())

		default:
			if p.tok == _Lbrace && len(f.DeclList) > 0 && isEmptyFuncDecl(f.DeclList[len(f.DeclList)-1]) {
				// opening { of function declaration on next line
				p.syntax_error("unexpected semicolon or newline before {")
			} else {
				p.syntax_error("non-declaration statement outside function body")
			}
			p.advance(_Const, _Var, _Function)
			continue
		}

		if p.tok != _EOF && !p.got(_Semi) {
			p.syntax_error("after top level declaration")
			p.advance(_Const, _Var, _Function)
		}
	}
	// p.tok == _EOF

	f.Lines = p.source.line

	return f
}

func isEmptyFuncDecl(dcl Decl) bool {
	f, ok := dcl.(*FuncDecl)
	return ok && f.Body == nil
}

// ----------------------------------------------------------------------------
// Declarations

// ImportSpec = ImportPath .
// ImportPath = string_lit .
func (p *parser) importDecl() Decl {
	if trace {
		defer p.trace("importDecl")()
	}

	d := new(ImportDecl)
	d.pos = p.pos()
	d.Path = p.oliteral()
	if d.Path == nil {
		p.syntax_error("missing import path")
		p.advance(_Semi, _Rparen)
		return nil
	}

	return d
}

// ConstSpec = IdentifierList [ [ Type ] "=" ExpressionList ] .
func (p *parser) constDecl() Decl {
	if trace {
		defer p.trace("constDecl")()
	}

	d := new(ConstDecl)
	d.pos = p.pos()

	for {
		name := p.name()
		p.want(_Assign)
		x := p.expr()
		d.Names = append(d.Names, name)
		d.Values = append(d.Values, x)
		if !p.got(_Comma) {
			break
		}
	}

	return d
}

// VarSpec = IdentifierList ( Type [ "=" ExpressionList ] | "=" ExpressionList ) .
func (p *parser) varDecl() Decl {
	if trace {
		defer p.trace("varDecl")()
	}

	d := new(VarDecl)
	d.pos = p.pos()

	for {
		name := p.name()
		d.Names = append(d.Names, name)
		if p.got(_Assign) {
			d.Values = append(d.Values, p.expr())
		} else {
			d.Values = append(d.Values, nil)
		}
		if !p.got(_Comma) {
			break
		}
	}

	return d
}

func (p *parser) funcDecl() *FuncDecl {
	if trace {
		defer p.trace("funcDecl")()
	}

	f := new(FuncDecl)
	f.pos = p.pos()

	if p.tok != _Name {
		p.syntax_error("expecting name")
		p.advance(_Lbrace, _Semi)
	}

	f.Name = p.name()
	f.Params = p.paramList()

	if p.tok == _Lbrace {
		f.Body = p.blockStmt("")
	}

	return f
}

// ----------------------------------------------------------------------------
// Expressions

func (p *parser) expr() Expr {
	if trace {
		defer p.trace("expr")()
	}

	return p.binaryExpr(0)
}

// Expression = UnaryExpr | Expression binary_op Expression .
func (p *parser) binaryExpr(prec int) Expr {
	// don't trace binaryExpr - only leads to overly nested trace output

	x := p.unaryExpr()
	for (p.tok == _Operator) && p.prec > prec {
		t := new(Operation)
		t.pos = p.pos()
		t.Op = p.op
		t.X = x
		tprec := p.prec
		p.next()
		t.Y = p.binaryExpr(tprec)
		x = t
	}
	return x
}

// UnaryExpr = PrimaryExpr | unary_op UnaryExpr .
func (p *parser) unaryExpr() Expr {
	if trace {
		defer p.trace("unaryExpr")()
	}

	switch p.tok {
	case _Operator:
		switch p.op {
		case Mul, Add, Sub, Not, Xor:
			x := new(Operation)
			x.pos = p.pos()
			x.Op = p.op
			p.next()
			x.X = p.unaryExpr()
			return x

		case And:
			x := new(Operation)
			x.pos = p.pos()
			x.Op = And
			p.next()
			// unaryExpr may have returned a parenthesized composite literal
			// (see comment in operand) - remove parentheses if any
			x.X = unparen(p.unaryExpr())
			return x
		}
	}

	// TODO(mdempsky): We need parens here so we can report an
	// error for "(x) := true". It should be possible to detect
	// and reject that more efficiently though.
	return p.pexpr(true)
}

// Operand     = Literal | OperandName | MethodExpr | "(" Expression ")" .
// Literal     = BasicLit | CompositeLit | FunctionLit .
// BasicLit    = int_lit | float_lit | imaginary_lit | rune_lit | string_lit .
// OperandName = identifier | QualifiedIdent.
func (p *parser) operand(keep_parens bool) Expr {
	if trace {
		defer p.trace("operand " + p.tok.String())()
	}

	switch p.tok {
	case _Name:
		return p.name()

	case _Literal:
		return p.oliteral()

	case _Lparen:
		pos := p.pos()
		p.next()
		p.xnest++
		x := p.expr()
		p.xnest--
		p.want(_Rparen)

		// Optimization: Record presence of ()'s only where needed
		// for error reporting. Don't bother in other cases; it is
		// just a waste of memory and time.

		// Parentheses are not permitted on lhs of := .
		// switch x.Op {
		// case ONAME, ONONAME, OPACK, OTYPE, OLITERAL, OTYPESW:
		// 	keep_parens = true
		// }

		// Parentheses are not permitted around T in a composite
		// literal T{}. If the next token is a {, assume x is a
		// composite literal type T (it may not be, { could be
		// the opening brace of a block, but we don't know yet).
		if p.tok == _Lbrace {
			keep_parens = true
		}

		// Parentheses are also not permitted around the expression
		// in a go/defer statement. In that case, operand is called
		// with keep_parens set.
		if keep_parens {
			px := new(ParenExpr)
			px.pos = pos
			px.X = x
			x = px
		}
		return x

	default:
		x := p.bad()
		p.syntax_error("expecting expression")
		p.advance()
		return x
	}

	// Syntactically, composite literals are operands. Because a complit
	// type may be a qualified identifier which is handled by pexpr
	// (together with selector expressions), complits are parsed there
	// as well (operand is only called from pexpr).
}

// PrimaryExpr =
// 	Operand |
// 	Conversion |
// 	PrimaryExpr Selector |
// 	PrimaryExpr Index |
// 	PrimaryExpr Slice |
// 	PrimaryExpr TypeAssertion |
// 	PrimaryExpr Arguments .
//
// Selector       = "." identifier .
// Index          = "[" Expression "]" .
// Slice          = "[" ( [ Expression ] ":" [ Expression ] ) |
//                      ( [ Expression ] ":" Expression ":" Expression )
//                  "]" .
// TypeAssertion  = "." "(" Type ")" .
// Arguments      = "(" [ ( ExpressionList | Type [ "," ExpressionList ] ) [ "..." ] [ "," ] ] ")" .
func (p *parser) pexpr(keep_parens bool) Expr {
	if trace {
		defer p.trace("pexpr")()
	}

	x := p.operand(keep_parens)

loop:
	for {
		pos := p.pos()
		switch p.tok {
		case _Dot:
			p.next()
			switch p.tok {
			case _Name:
				// pexpr '.' sym
				t := new(SelectorExpr)
				t.pos = pos
				t.X = x
				t.Sel = p.name()
				x = t

			default:
				p.syntax_error("expecting name")
				p.advance(_Semi, _Rparen)
			}

		case _Lbrack:
			p.next()
			p.xnest++

			i := p.expr()
			p.want(_Rbrack)
			// x[i]
			t := new(IndexExpr)
			t.pos = pos
			t.X = x
			t.Index = i
			x = t
			p.xnest--
			break

		case _Lparen:
			x = p.call(x)

		default:
			break loop
		}
	}

	return x
}

// ----------------------------------------------------------------------------
// Types

func newIndirect(pos src.Pos, typ Expr) Expr {
	o := new(Operation)
	o.pos = pos
	o.Op = Mul
	o.X = typ
	return o
}

func (p *parser) dotname(name *Name) Expr {
	if trace {
		defer p.trace("dotname")()
	}

	if p.tok == _Dot {
		s := new(SelectorExpr)
		s.pos = p.pos()
		p.next()
		s.X = name
		s.Sel = p.name()
		return s
	}
	return name
}

// FunctionBody = Block .
func (p *parser) funcBody() []Stmt {
	if trace {
		defer p.trace("funcBody")()
	}

	p.fnest++
	body := p.stmtList()
	p.fnest--

	if body == nil {
		body = []Stmt{new(EmptyStmt)}
	}
	return body
}

func (p *parser) oliteral() *BasicLit {
	if p.tok == _Literal {
		b := new(BasicLit)
		b.pos = p.pos()
		b.Value = p.lit
		b.Kind = p.kind
		p.next()
		return b
	}
	return nil
}

// Parameters    = "(" [ ParameterList [ "," ] ] ")" .
// ParameterList = ParameterDecl { "," ParameterDecl } .
func (p *parser) paramList() (list []*Name) {
	if trace {
		defer p.trace("paramList")()
	}

	p.want(_Lparen)

	for p.tok != _EOF && p.tok != _Rparen {
		list = append(list, p.name())
		if !p.ocomma(_Rparen) {
			break
		}
	}

	p.want(_Rparen)
	return
}

func (p *parser) bad() *BadExpr {
	b := new(BadExpr)
	b.pos = p.pos()
	return b
}

// ----------------------------------------------------------------------------
// Statements

// We represent x++, x-- as assignments x += ImplicitOne, x -= ImplicitOne.
// ImplicitOne should not be used elsewhere.
var ImplicitOne = &BasicLit{Value: "1"}

// SimpleStmt = EmptyStmt | ExpressionStmt | IncDecStmt | Assignment | ShortVarDecl .
func (p *parser) simpleStmt(lhs Expr, rangeOk bool) SimpleStmt {
	if trace {
		defer p.trace("simpleStmt")()
	}

	if lhs == nil {
		lhs = p.exprList()
	}

	// expr_list
	pos := p.pos()
	switch p.tok {
	case _Assign:
		p.next()

		// expr_list '=' expr_list
		return p.newAssignStmt(pos, 0, lhs, p.exprList())

	default:
		p.syntax_error("expecting = or comma")
		p.advance(_Semi, _Rbrace)
		// make the best of what we have
		s := new(ExprStmt)
		s.pos = lhs.Pos()
		s.X = lhs
		return s
	}
}

func (p *parser) newAssignStmt(pos src.Pos, op Operator, lhs, rhs Expr) *AssignStmt {
	a := new(AssignStmt)
	a.pos = pos
	a.Op = op
	a.Lhs = lhs
	a.Rhs = rhs
	return a
}

func (p *parser) blockStmt(context string) *BlockStmt {
	if trace {
		defer p.trace("blockStmt")()
	}

	s := new(BlockStmt)
	s.pos = p.pos()

	if !p.got(_Lbrace) {
		p.syntax_error("expecting { after " + context)
		p.advance(_Name, _Rbrace)
		// TODO(gri) may be better to return here than to continue (#19663)
	}

	s.List = p.stmtList()
	s.Rbrace = p.pos()
	p.want(_Rbrace)

	return s
}

func (p *parser) declStmt(f func() Decl) *DeclStmt {
	if trace {
		defer p.trace("declStmt")()
	}

	s := new(DeclStmt)
	s.pos = p.pos()

	p.next() // _Const, or _Var
	s.Decl = f()
	return s
}

func (p *parser) forStmt() Stmt {
	if trace {
		defer p.trace("forStmt")()
	}

	s := new(ForStmt)
	s.pos = p.pos()

	s.Init, s.Cond, s.Post = p.header(_For)
	s.Body = p.blockStmt("for clause")

	return s
}

// TODO(gri) This function is now so heavily influenced by the keyword that
//           it may not make sense anymore to combine all three cases. It
//           may be simpler to just split it up for each statement kind.
func (p *parser) header(keyword token) (init SimpleStmt, cond Expr, post SimpleStmt) {
	p.want(keyword)

	if p.tok == _Lbrace {
		if keyword == _If {
			p.syntax_error("missing condition in if statement")
		}
		return
	}
	// p.tok != _Lbrace

	outer := p.xnest
	p.xnest = -1

	if p.tok != _Semi {
		// accept potential varDecl but complain
		if p.got(_Var) {
			p.syntax_error(fmt.Sprintf("var declaration not allowed in %s initializer", keyword.String()))
		}
		init = p.simpleStmt(nil, keyword == _For)
	}

	var condStmt SimpleStmt
	var semi struct {
		pos src.Pos
		lit string // valid if pos.IsKnown()
	}
	if p.tok == _Semi {
		semi.pos = p.pos()
		semi.lit = p.lit
		p.next()
		if keyword == _For {
			if p.tok != _Semi {
				if p.tok == _Lbrace {
					p.syntax_error("expecting for loop condition")
					goto done
				}
				condStmt = p.simpleStmt(nil, false)
			}
			p.want(_Semi)
			if p.tok != _Lbrace {
				post = p.simpleStmt(nil, false)
			}
		} else if p.tok != _Lbrace {
			condStmt = p.simpleStmt(nil, false)
		}
	} else {
		condStmt = init
		init = nil
	}

done:
	// unpack condStmt
	switch s := condStmt.(type) {
	case nil:
		if keyword == _If && semi.pos.IsKnown() {
			if semi.lit != "semicolon" {
				p.syntax_error_at(semi.pos, fmt.Sprintf("unexpected %s, expecting { after if clause", semi.lit))
			} else {
				p.syntax_error_at(semi.pos, "missing condition in if statement")
			}
		}
	case *ExprStmt:
		cond = s.X
	default:
		p.syntax_error(fmt.Sprintf("%s used as value", String(s)))
	}

	p.xnest = outer
	return
}

func (p *parser) ifStmt() *IfStmt {
	if trace {
		defer p.trace("ifStmt")()
	}

	s := new(IfStmt)
	s.pos = p.pos()

	p.want(_If)
	s.Cond = p.expr()
	s.Then = p.blockStmt("if clause")

	if p.got(_Else) {
		switch p.tok {
		case _If:
			s.Else = p.ifStmt()
		case _Lbrace:
			s.Else = p.blockStmt("")
		default:
			p.syntax_error("else must be followed by if or statement block")
			p.advance(_Name, _Rbrace)
		}
	}

	return s
}

// Statement =
// 	Declaration | LabeledStmt | SimpleStmt |
// 	GoStmt | ReturnStmt | BreakStmt | ContinueStmt | GotoStmt |
// 	FallthroughStmt | Block | IfStmt | SwitchStmt | SelectStmt | ForStmt |
// 	DeferStmt .
func (p *parser) stmtOrNil() Stmt {
	if trace {
		defer p.trace("stmt " + p.tok.String())()
	}

	// Most statements (assignments) start with an identifier;
	// look for it first before doing anything more expensive.
	if p.tok == _Name {
		lhs := p.exprList()
		return p.simpleStmt(lhs, false)
	}

	switch p.tok {
	case _Lbrace:
		return p.blockStmt("")

	case _Var:
		return p.declStmt(p.varDecl)

	case _Const:
		return p.declStmt(p.constDecl)

	case _Operator:
		switch p.op {
		case Add, Sub, Mul, And, Xor, Not:
			return p.simpleStmt(nil, false) // unary operators
		}

	case _Literal, _Function, _Lparen, // operands
		_Lbrack: // composite types
		return p.simpleStmt(nil, false)

	case _For:
		return p.forStmt()

	case _If:
		return p.ifStmt()

	case _Break, _Continue:
		s := new(BranchStmt)
		s.pos = p.pos()
		s.Tok = p.tok
		p.next()
		return s

	case _Return:
		s := new(ReturnStmt)
		s.pos = p.pos()
		p.next()
		if p.tok != _Semi && p.tok != _Rbrace {
			s.Result = p.expr()
		}
		return s

	case _Semi:
		s := new(EmptyStmt)
		s.pos = p.pos()
		return s
	}

	return nil
}

// StatementList = { Statement ";" } .
func (p *parser) stmtList() (l []Stmt) {
	if trace {
		defer p.trace("stmtList")()
	}

	for p.tok != _EOF && p.tok != _Rbrace && p.tok != _Case && p.tok != _Default {
		s := p.stmtOrNil()
		if s == nil {
			break
		}
		l = append(l, s)
		// customized version of osemi:
		// ';' is optional before a closing ')' or '}'
		if p.tok == _Rparen || p.tok == _Rbrace {
			continue
		}
		if !p.got(_Semi) {
			p.syntax_error("at end of statement")
			p.advance(_Semi, _Rbrace)
		}
	}
	return
}

// Arguments = "(" [ ( ExpressionList | Type [ "," ExpressionList ] ) [ "..." ] [ "," ] ] ")" .
func (p *parser) call(fun Expr) *CallExpr {
	if trace {
		defer p.trace("call")()
	}

	// call or conversion
	// convtype '(' expr ocomma ')'
	c := new(CallExpr)
	c.pos = p.pos()
	c.Fun = fun

	p.want(_Lparen)
	p.xnest++

	for p.tok != _EOF && p.tok != _Rparen {
		c.ArgList = append(c.ArgList, p.expr())
		if !p.ocomma(_Rparen) {
			break
		}
	}

	p.xnest--
	p.want(_Rparen)

	return c
}

// ----------------------------------------------------------------------------
// Common productions

func (p *parser) newName(value string) *Name {
	n := new(Name)
	n.pos = p.pos()
	n.Value = value
	return n
}

func (p *parser) name() *Name {
	// no tracing to avoid overly verbose output

	if p.tok == _Name {
		n := p.newName(p.lit)
		p.next()
		return n
	}

	n := p.newName("_")
	p.syntax_error("expecting name")
	p.advance()
	return n
}

// IdentifierList = identifier { "," identifier } .
// The first name must be provided.
func (p *parser) nameList(first *Name) []*Name {
	if trace {
		defer p.trace("nameList")()
	}

	if debug && first == nil {
		panic("first name not provided")
	}

	l := []*Name{first}
	for p.got(_Comma) {
		l = append(l, p.name())
	}

	return l
}

// The first name may be provided, or nil.
func (p *parser) qualifiedName(name *Name) Expr {
	if trace {
		defer p.trace("qualifiedName")()
	}

	switch {
	case name != nil:
		// name is provided
	case p.tok == _Name:
		name = p.name()
	default:
		name = p.newName("_")
		p.syntax_error("expecting name")
		p.advance(_Dot, _Semi, _Rbrace)
	}

	return p.dotname(name)
}

// ExpressionList = Expression { "," Expression } .
func (p *parser) exprList() Expr {
	if trace {
		defer p.trace("exprList")()
	}

	x := p.expr()
	if p.got(_Comma) {
		list := []Expr{x, p.expr()}
		for p.got(_Comma) {
			list = append(list, p.expr())
		}
		t := new(ListExpr)
		t.pos = x.Pos()
		t.ElemList = list
		x = t
	}
	return x
}

// osemi parses an optional semicolon.
func (p *parser) osemi(follow token) bool {
	switch p.tok {
	case _Semi:
		p.next()
		return true

	case _Rparen, _Rbrace:
		// semicolon is optional before ) or }
		return true
	}

	p.syntax_error("expecting semicolon, newline, or " + tokstring(follow))
	p.advance(follow)
	return false
}

// ocomma parses an optional comma.
func (p *parser) ocomma(follow token) bool {
	switch p.tok {
	case _Comma:
		p.next()
		return true

	case _Rparen, _Rbrace:
		// comma is optional before ) or }
		return true
	}

	p.syntax_error("expecting comma or " + tokstring(follow))
	p.advance(follow)
	return false
}

// unparen removes all parentheses around an expression.
func unparen(x Expr) Expr {
	for {
		p, ok := x.(*ParenExpr)
		if !ok {
			break
		}
		x = p.X
	}
	return x
}
