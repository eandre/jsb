package syntax

import "fmt"

type token uint

const (
	_ token = iota
	_EOF

	// Names and literals
	_Name
	_Literal

	// Operators and operations
	_Assign
	_AssignOp
	_IncOp
	_Operator

	// Delimiters
	_Colon
	_Comma
	_Dot
	_Question
	_Semi
	_Lbrace
	_Lbrack
	_Lparen
	_Rbrace
	_Rbrack
	_Rparen

	// Keywords
	_Break
	_Case
	_Catch
	_Class
	_Const
	_Continue
	_Default
	_Delete
	_Do
	_Else
	_Enum
	_Export
	_Extends
	_Finally
	_For
	_Function
	_If
	_Implements
	_Import
	_In
	_InstanceOf
	_Interface
	_Let
	_New
	_Package
	_Private
	_Protected
	_Public
	_Return
	_Static
	_Super
	_Switch
	_This
	_Throw
	_Try
	_TypeOf
	_Var
	_Void
	_While
	_With

	tokenCount
)

var tokstrings = [...]string{
	// source control
	_EOF: "EOF",

	// names and literals
	_Name:    "name",
	_Literal: "literal",

	// operators and operations
	_Assign:   "=",
	_AssignOp: "op=",
	_IncOp:    "opop",
	_Operator: "op",

	// delimiters
	_Colon:    ":",
	_Comma:    ",",
	_Dot:      ".",
	_Question: "?",
	_Semi:     ";",
	_Lbrace:   "{",
	_Lbrack:   "[",
	_Lparen:   "(",
	_Rbrace:   "}",
	_Rbrack:   "]",
	_Rparen:   ")",

	// keywords
	_Break:      "break",
	_Case:       "case",
	_Catch:      "catch",
	_Class:      "class",
	_Const:      "const",
	_Continue:   "continue",
	_Default:    "default",
	_Delete:     "delete",
	_Do:         "do",
	_Else:       "else",
	_Enum:       "enum",
	_Export:     "export",
	_Extends:    "extends",
	_Finally:    "finally",
	_For:        "for",
	_Function:   "function",
	_If:         "if",
	_Implements: "implements",
	_Import:     "import",
	_In:         "in",
	_InstanceOf: "instanceof",
	_Interface:  "interface",
	_Let:        "let",
	_New:        "new",
	_Package:    "package",
	_Private:    "private",
	_Protected:  "protected",
	_Public:     "public",
	_Return:     "return",
	_Static:     "static",
	_Super:      "super",
	_Switch:     "switch",
	_This:       "this",
	_Throw:      "throw",
	_Try:        "try",
	_TypeOf:     "typeof",
	_Var:        "var",
	_Void:       "void",
	_While:      "while",
	_With:       "with",
}

func (tok token) String() string {
	var s string
	if 0 <= tok && int(tok) < len(tokstrings) {
		s = tokstrings[tok]
	}
	if s == "" {
		s = fmt.Sprintf("<tok-%d>", tok)
	}
	return s
}

// Make sure we have at most 64 tokens so we can use them in a set.
const _ uint64 = 1 << (tokenCount - 1)

// contains reports whether tok is in tokset.
func contains(tokset uint64, tok token) bool {
	return tokset&(1<<tok) != 0
}

type LitKind uint

const (
	NumLit LitKind = iota
	StringLit
)

type Operator uint

const (
	_      Operator = iota
	Not             // !
	OrOr            // ||
	AndAnd          // &&
	Eql             // ==
	Seql            // ===
	Neq             // !=
	Sneq            // !==
	Lss             // <
	Leq             // <=
	Gtr             // >
	Geq             // >=
	Add             // +
	Sub             // -
	Or              // |
	Xor             // ^
	Mul             // *
	Div             // /
	Rem             // %
	And             // &
	AndNot          // &^
	Shl             // <<
	Shr             // >>
	Ushr            // >>>
	Ushl            // <<<
)

var opstrings = [...]string{
	Not:    "!",
	OrOr:   "||",
	AndAnd: "&&",
	Eql:    "==",
	Seql:   "===",
	Neq:    "!=",
	Sneq:   "!==",
	Lss:    "<",
	Leq:    "<=",
	Gtr:    ">",
	Geq:    ">=",
	Add:    "+",
	Sub:    "-",
	Or:     "|",
	Xor:    "^",
	Mul:    "*",
	Div:    "/",
	Rem:    "%",
	And:    "&",
	AndNot: "&^",
	Shl:    "<<",
	Shr:    ">>",
	Ushr:   ">>>",
	Ushl:   "<<<",
}

func (op Operator) String() string {
	var s string
	if 0 <= op && int(op) < len(opstrings) {
		s = opstrings[op]
	}
	if s == "" {
		s = fmt.Sprintf("<op-%d>", op)
	}
	return s
}
