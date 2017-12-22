// Copyright 2016 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package syntax

import (
	"fmt"
	"log"
	"os"
	"strings"
	"testing"
)

func TestScanner(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping test in short mode")
	}

	src, err := os.Open("scanner.go")
	if err != nil {
		t.Fatal(err)
	}
	defer src.Close()

	var s scanner
	s.init(src, nil)
	for {
		s.next()
		if s.tok == _EOF {
			break
		}
		switch s.tok {
		case _Name:
			fmt.Println(s.line, s.tok, "=>", s.lit)
		case _Operator:
			fmt.Println(s.line, s.tok, "=>", s.op)
		default:
			fmt.Println(s.line, s.tok)
		}
	}
}

func TestTokens(t *testing.T) {
	// make source
	var buf []byte
	for i, s := range sampleTokens {
		buf = append(buf, "\t\t\t\t"[:i&3]...)     // leading indentation
		buf = append(buf, s.src...)                // token
		buf = append(buf, "        "[:i&7]...)     // trailing spaces
		buf = append(buf, "/* foo */ // bar\n"...) // comments
	}

	// scan source
	var got scanner
	got.init(&bytesReader{buf}, func(line, pos uint, msg string) {
		log.Println(line, pos, msg)
	})
	got.next()
	for i, want := range sampleTokens {
		nlsemi := false

		if got.line != uint(i+linebase) {
			t.Errorf("got line %d; want %d", got.line, i+linebase)
		}

		if got.tok != want.tok {
			t.Errorf("got tok = %s; want %s", got.tok, want.tok)
			continue
		}

		switch want.tok {
		case _Semi:
			if got.lit != "semicolon" {
				t.Errorf("got %s; want semicolon", got.lit)
			}

		case _Name, _Literal:
			if got.lit != want.src {
				t.Errorf("got lit = %q; want %q", got.lit, want.src)
				continue
			}
			nlsemi = true

		case _Operator, _AssignOp, _IncOp:
			if got.op != want.op {
				t.Errorf("got op = %s; want %s", got.op, want.op)
				continue
			}
			nlsemi = want.tok == _IncOp

		case _Rparen, _Rbrack, _Rbrace, _Break, _Continue, _Return:
			nlsemi = true
		}

		if nlsemi {
			got.next()
			if got.tok != _Semi {
				t.Errorf("got tok = %s; want ;", got.tok)
				continue
			}
			if got.lit != "newline" {
				t.Errorf("got %s; want newline", got.lit)
			}
		}

		got.next()
	}

	if got.tok != _EOF {
		t.Errorf("got %q; want _EOF", got.tok)
	}
}

var sampleTokens = [...]struct {
	tok token
	src string
	op  Operator
}{
	// name samples
	{_Name, "x", 0},
	{_Name, "X123", 0},
	{_Name, "foo", 0},
	{_Name, "Foo123", 0},
	{_Name, "foo_bar", 0},
	{_Name, "_", 0},
	{_Name, "_foobar", 0},
	{_Name, "a۰۱۸", 0},
	{_Name, "foo६४", 0},
	{_Name, "bar９８７６", 0},
	{_Name, "ŝ", 0},
	{_Name, "ŝfoo", 0},

	// literal samples
	{_Literal, "0", 0},
	{_Literal, "1", 0},
	{_Literal, "12345", 0},
	{_Literal, "123456789012345678890123456789012345678890", 0},
	{_Literal, "01234567", 0},
	{_Literal, "0x0", 0},
	{_Literal, "0xcafebabe", 0},
	{_Literal, "0.", 0},
	{_Literal, "0.e0", 0},
	{_Literal, "0.e-1", 0},
	{_Literal, "0.e+123", 0},
	{_Literal, ".0", 0},
	{_Literal, ".0E00", 0},
	{_Literal, ".0E-0123", 0},
	{_Literal, ".0E+12345678901234567890", 0},
	{_Literal, ".45e1", 0},
	{_Literal, "3.14159265", 0},
	{_Literal, "1e0", 0},
	{_Literal, "1e+100", 0},
	{_Literal, "1e-100", 0},
	{_Literal, "2.71828e-1000", 0},
	{_Literal, "'a'", 0},
	{_Literal, "'\\000'", 0},
	{_Literal, "'\\xFF'", 0},
	{_Literal, "'\\uff16'", 0},
	{_Literal, "'\\U0000ff16'", 0},

	// operators
	{_Operator, "||", OrOr},

	{_Operator, "&&", AndAnd},

	{_Operator, "==", Eql},
	{_Operator, "!=", Neq},
	{_Operator, "<", Lss},
	{_Operator, "<=", Leq},
	{_Operator, ">", Gtr},
	{_Operator, ">=", Geq},

	{_Operator, "+", Add},
	{_Operator, "-", Sub},
	{_Operator, "|", Or},
	{_Operator, "^", Xor},

	{_Operator, "*", Mul},
	{_Operator, "/", Div},
	{_Operator, "%", Rem},
	{_Operator, "&", And},
	{_Operator, "&^", AndNot},
	{_Operator, "<<", Shl},
	{_Operator, ">>", Shr},

	// assignment operations
	{_AssignOp, "+=", Add},
	{_AssignOp, "-=", Sub},
	{_AssignOp, "|=", Or},
	{_AssignOp, "^=", Xor},

	{_AssignOp, "*=", Mul},
	{_AssignOp, "/=", Div},
	{_AssignOp, "%=", Rem},
	{_AssignOp, "&=", And},
	{_AssignOp, "&^=", AndNot},
	{_AssignOp, "<<=", Shl},
	{_AssignOp, ">>=", Shr},

	// other operations
	{_IncOp, "++", Add},
	{_IncOp, "--", Sub},
	{_Assign, "=", 0},

	// delimiters
	{_Lparen, "(", 0},
	{_Lbrack, "[", 0},
	{_Lbrace, "{", 0},
	{_Rparen, ")", 0},
	{_Rbrack, "]", 0},
	{_Rbrace, "}", 0},
	{_Comma, ",", 0},
	{_Semi, ";", 0},
	{_Colon, ":", 0},
	{_Dot, ".", 0},

	// keywords
	{_Break, "break", 0},
	{_Case, "case", 0},
	{_Catch, "catch", 0},
	{_Const, "const", 0},
	{_Continue, "continue", 0},
	{_Default, "default", 0},
	{_Delete, "delete", 0},
	{_Do, "do", 0},
	{_Else, "else", 0},
	{_Enum, "enum", 0},
	{_Export, "export", 0},
	{_Extends, "extends", 0},
	{_Finally, "finally", 0},
	{_For, "for", 0},
	{_Function, "function", 0},
	{_If, "if", 0},
	{_Implements, "implements", 0},
	{_Import, "import", 0},
	{_In, "in", 0},
	{_InstanceOf, "instanceof", 0},
	{_Interface, "interface", 0},
	{_Let, "let", 0},
	{_Package, "package", 0},
	{_Private, "private", 0},
	{_Protected, "protected", 0},
	{_Public, "public", 0},
	{_Return, "return", 0},
	{_Static, "static", 0},
	{_Super, "super", 0},
	{_Switch, "switch", 0},
	{_This, "this", 0},
	{_Throw, "throw", 0},
	{_Try, "try", 0},
	{_TypeOf, "typeof", 0},
	{_Var, "var", 0},
	{_Void, "void", 0},
	{_While, "while", 0},
	{_With, "with", 0},
}

func TestScanErrors(t *testing.T) {
	for _, test := range []struct {
		src, msg  string
		line, col uint // 0-based
	}{
		// Note: Positions for lexical errors are the earliest position
		// where the error is apparent, not the beginning of the respective
		// token.

		// rune-level errors
		{"fo\x00o", "invalid NUL character", 0, 2},
		{"foo\n\ufeff bar", "invalid BOM in the middle of the file", 1, 0},
		{"foo\n\n\xff    ", "invalid UTF-8 encoding", 2, 0},

		// token-level errors
		{"\u00BD" /* ½ */, "invalid identifier character U+00BD '½'", 0, 0},
		{"\U0001d736\U0001d737\U0001d738_½" /* 𝜶𝜷𝜸_½ */, "invalid identifier character U+00BD '½'", 0, 13 /* byte offset */},
		{"\U0001d7d8" /* 𝟘 */, "identifier cannot begin with digit U+1D7D8 '𝟘'", 0, 0},
		{"foo\U0001d7d8_½" /* foo𝟘_½ */, "invalid identifier character U+00BD '½'", 0, 8 /* byte offset */},

		{"foo$bar = 0", "invalid character U+0024 '$'", 0, 3},
		{"const x = 0xyz", "malformed hex constant", 0, 12},
		{"0123456789", "malformed octal constant", 0, 10},
		{"0123456789. /* foobar", "comment not terminated", 0, 12},   // valid float constant
		{"0123456789e0 /*\nfoobar", "comment not terminated", 0, 13}, // valid float constant
		{"var a, b = 08, 07\n", "malformed octal constant", 0, 13},
		{"(x + 1.0e+x)", "malformed floating-point constant exponent", 0, 10},

		{"'\n", "newline in string", 0, 1},
		{`'\`, "string not terminated", 0, 0},
		{`'\'`, "string not terminated", 0, 0},
		{`'\x`, "string not terminated", 0, 0},
		{`'\x'`, "non-hex character in escape sequence: '", 0, 3},
		{`'\y'`, "unknown escape sequence", 0, 2},
		{`'\x0'`, "non-hex character in escape sequence: '", 0, 4},
		{`'\00'`, "non-octal character in escape sequence: '", 0, 4},
		{`'\377' /*`, "comment not terminated", 0, 7}, // valid octal escape
		{`'\400'`, "octal escape value > 255: 256", 0, 5},
		{`'xx`, "string not terminated", 0, 0},

		{"\"\n", "newline in string", 0, 1},
		{`"`, "string not terminated", 0, 0},
		{`"foo`, "string not terminated", 0, 0},
		{"`", "invalid character U+0060 '`'", 0, 0},
		{"`foo", "invalid character U+0060 '`'", 0, 0},
		{"/*/", "comment not terminated", 0, 0},
		{"/*\n\nfoo", "comment not terminated", 0, 0},
		{"/*\n\nfoo", "comment not terminated", 0, 0},
		{`"\`, "string not terminated", 0, 0},
		{`"\"`, "string not terminated", 0, 0},
		{`"\x`, "string not terminated", 0, 0},
		{`"\x"`, "non-hex character in escape sequence: \"", 0, 3},
		{`"\y"`, "unknown escape sequence", 0, 2},
		{`"\x0"`, "non-hex character in escape sequence: \"", 0, 4},
		{`"\00"`, "non-octal character in escape sequence: \"", 0, 4},
		{`"\377" /*`, "comment not terminated", 0, 7}, // valid octal escape
		{`"\378"`, "non-octal character in escape sequence: 8", 0, 4},
		{`"\400"`, "octal escape value > 255: 256", 0, 5},

		{`s := "foo\z"`, "unknown escape sequence", 0, 10},
		{`s := "foo\z00\nbar"`, "unknown escape sequence", 0, 10},
		{`"\x`, "string not terminated", 0, 0},
		{`"\x"`, "non-hex character in escape sequence: \"", 0, 3},
		{`var s string = "\x"`, "non-hex character in escape sequence: \"", 0, 18},
		{`return "\Uffffffff"`, "escape sequence is invalid Unicode code point", 0, 18},

		// former problem cases
		{"package p\n\n\xef", "invalid UTF-8 encoding", 2, 0},
	} {
		var s scanner
		nerrors := 0
		s.init(&bytesReader{[]byte(test.src)}, func(line, col uint, msg string) {
			nerrors++
			// only check the first error
			if nerrors == 1 {
				if msg != test.msg {
					t.Errorf("%q: got msg = %q; want %q", test.src, msg, test.msg)
				}
				if line != test.line+linebase {
					t.Errorf("%q: got line = %d; want %d", test.src, line, test.line+linebase)
				}
				if col != test.col+colbase {
					t.Errorf("%q: got col = %d; want %d", test.src, col, test.col+colbase)
				}
			} else if nerrors > 1 {
				// TODO(gri) make this use position info
				t.Errorf("%q: got unexpected %q at line = %d", test.src, msg, line)
			}
		})

		for {
			s.next()
			if s.tok == _EOF {
				break
			}
		}

		if nerrors == 0 {
			t.Errorf("%q: got no error; want %q", test.src, test.msg)
		}
	}
}

func TestIssue21938(t *testing.T) {
	s := "/*" + strings.Repeat(" ", 4089) + "*/ .5"

	var got scanner
	got.init(strings.NewReader(s), nil)
	got.next()

	if got.tok != _Literal || got.lit != ".5" {
		t.Errorf("got %s %q; want %s %q", got.tok, got.lit, _Literal, ".5")
	}
}
