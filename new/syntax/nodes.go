// Copyright 2016 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package syntax

import "github.com/eandre/jsb/new/src"

// ----------------------------------------------------------------------------
// Nodes

type Node interface {
	// Pos() returns the position associated with the node as follows:
	// 1) The position of a node representing a terminal syntax production
	//    (Name, BasicLit, etc.) is the position of the respective production
	//    in the source.
	// 2) The position of a node representing a non-terminal production
	//    (IndexExpr, IfStmt, etc.) is the position of a token uniquely
	//    associated with that production; usually the left-most one
	//    ('[' for IndexExpr, 'if' for IfStmt, etc.)
	Pos() src.Pos
	aNode()
}

type node struct {
	// commented out for now since not yet used
	// doc  *Comment // nil means no comment(s) attached
	pos src.Pos
}

func (n *node) Pos() src.Pos { return n.pos }
func (*node) aNode()         {}

// ----------------------------------------------------------------------------
// Files

// package PkgName; DeclList[0], DeclList[1], ...
type File struct {
	PkgName  *Name
	DeclList []Decl
	Lines    uint
	node
}

// ----------------------------------------------------------------------------
// Declarations

type (
	Decl interface {
		Node
		aDecl()
	}

	ImportDecl struct {
		Path *BasicLit
		decl
	}

	ConstDecl struct {
		Names  []*Name
		Values []Expr // len(Values) == len(Names)
		decl
	}

	VarDecl struct {
		Names  []*Name
		Values []Expr // len(Values) == len(Names)
		decl
	}

	// Name { Body }
	// Name extends Name { Body }
	ClassDecl struct {
		Name    *Name
		Extends *Name // Can be nil

		// TODO body

		decl
	}

	// Name { Body }
	FuncDecl struct {
		Name   *Name
		Params []*Name
		Body   *BlockStmt
		decl
	}
)

type decl struct{ node }

func (*decl) aDecl() {}

// ----------------------------------------------------------------------------
// Expressions

type (
	Expr interface {
		Node
		aExpr()
	}

	// Placeholder for an expression that failed to parse
	// correctly and where we can't provide a better node.
	BadExpr struct {
		expr
	}

	// Value
	Name struct {
		Value string
		expr
	}

	// Value
	BasicLit struct {
		Value string
		Kind  LitKind
		expr
	}

	// (X)
	ParenExpr struct {
		X Expr
		expr
	}

	// X.Sel
	SelectorExpr struct {
		X   Expr
		Sel *Name
		expr
	}

	// X[Index]
	IndexExpr struct {
		X     Expr
		Index Expr
		expr
	}

	Operation struct {
		Op   Operator
		X, Y Expr // Y == nil means unary expression
		expr
	}

	// Fun(ArgList[0], ArgList[1], ...)
	CallExpr struct {
		Fun     Expr
		ArgList []Expr
		expr
	}
)

type expr struct{ node }

func (*expr) aExpr() {}

// ----------------------------------------------------------------------------
// Statements

type (
	Stmt interface {
		Node
		aStmt()
	}

	SimpleStmt interface {
		Stmt
		aSimpleStmt()
	}

	EmptyStmt struct {
		simpleStmt
	}

	BlockStmt struct {
		List   []Stmt
		Rbrace src.Pos
		stmt
	}

	ExprStmt struct {
		X Expr
		simpleStmt
	}

	DeclStmt struct {
		Decl Decl
		stmt
	}

	// ElemList[0], ElemList[1], ...
	ListExpr struct {
		ElemList []Expr
		expr
	}

	AssignStmt struct {
		Op       Operator // 0 means no operation
		Lhs, Rhs Expr     // Rhs == ImplicitOne means Lhs++ (Op == Add) or Lhs-- (Op == Sub)
		simpleStmt
	}

	BranchStmt struct {
		Tok token // Break or Continue

		stmt
	}

	ReturnStmt struct {
		Result Expr // nil means naked return
		stmt
	}

	IfStmt struct {
		Cond Expr
		Then *BlockStmt
		Else Stmt // either *IfStmt or *BlockStmt
		stmt
	}

	ForStmt struct {
		Init SimpleStmt
		Cond Expr
		Post SimpleStmt
		Body *BlockStmt
		stmt
	}
)

type stmt struct{ node }

func (stmt) aStmt() {}

type simpleStmt struct {
	stmt
}

func (simpleStmt) aSimpleStmt() {}

// ----------------------------------------------------------------------------
// Comments

// TODO(gri) Consider renaming to CommentPos, CommentPlacement, etc.
//           Kind = Above doesn't make much sense.
type CommentKind uint

const (
	Above CommentKind = iota
	Below
	Left
	Right
)

type Comment struct {
	Kind CommentKind
	Text string
	Next *Comment
}
