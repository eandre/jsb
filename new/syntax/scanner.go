// Copyright 2016 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This file implements scanner, a lexical tokenizer for
// Go source. After initialization, consecutive calls of
// next advance the scanner one token at a time.
//
// This file, source.go, and tokens.go are self-contained
// (go tool compile scanner.go source.go tokens.go compiles)
// and thus could be made into its own package.

package syntax

import (
	"fmt"
	"io"
	"unicode"
	"unicode/utf8"
)

type scanner struct {
	source
	pragh  func(line, col uint, msg string)
	nlsemi bool // if set '\n' and EOF translate to ';'

	// current token, valid after calling next()
	line, col uint
	tok       token
	lit       string   // valid if tok is _Name, _Literal, or _Semi ("semicolon", "newline", or "EOF")
	kind      LitKind  // valid if tok is _Literal
	op        Operator // valid if tok is _Operator, _AssignOp, or _IncOp
}

func (s *scanner) init(src io.Reader, errh func(line, col uint, msg string)) {
	s.source.init(src, errh)
	s.nlsemi = false
}

// next advances the scanner by reading the next token.
//
// If a read, source encoding, or lexical error occurs, next
// calls the error handler installed with init. The handler
// must exist.
//
// If a //line or //go: directive is encountered at the start
// of a line, next calls the directive handler pragh installed
// with init, if not nil.
//
// The (line, col) position passed to the error and directive
// handler is always at or after the current source reading
// position.
func (s *scanner) next() {
	nlsemi := s.nlsemi
	s.nlsemi = false

redo:
	// skip white space
	c := s.getr()
	for c == ' ' || c == '\t' || c == '\n' && !nlsemi || c == '\r' {
		c = s.getr()
	}

	// token start
	s.line, s.col = s.source.line0, s.source.col0

	if isLetter(c) || c >= utf8.RuneSelf && s.isIdentRune(c, true) {
		s.ident()
		return
	}

	switch c {
	case -1:
		if nlsemi {
			s.lit = "EOF"
			s.tok = _Semi
			break
		}
		s.tok = _EOF

	case '\n':
		s.lit = "newline"
		s.tok = _Semi

	case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
		s.number(c)

	case '"':
		s.stdString(c)

	case '\'':
		s.stdString(c)

	case '(':
		s.tok = _Lparen

	case '[':
		s.tok = _Lbrack

	case '{':
		s.tok = _Lbrace

	case ',':
		s.tok = _Comma

	case ';':
		s.lit = "semicolon"
		s.tok = _Semi

	case ')':
		s.nlsemi = true
		s.tok = _Rparen

	case ']':
		s.nlsemi = true
		s.tok = _Rbrack

	case '}':
		s.nlsemi = true
		s.tok = _Rbrace

	case ':':
		s.tok = _Colon

	case '?':
		s.tok = _Question

	case '.':
		c = s.getr()
		if isDigit(c) {
			s.ungetr2()
			s.number('.')
			break
		}
		s.ungetr()
		s.tok = _Dot

	case '+':
		s.op = Add
		c = s.getr()
		if c != '+' {
			goto assignop
		}
		s.nlsemi = true
		s.tok = _IncOp

	case '-':
		s.op = Sub
		c = s.getr()
		if c != '-' {
			goto assignop
		}
		s.nlsemi = true
		s.tok = _IncOp

	case '*':
		s.op = Mul
		c = s.getr()
		goto assignop

	case '/':
		c = s.getr()
		if c == '/' {
			s.lineComment()
			goto redo
		}
		if c == '*' {
			s.fullComment()
			if s.source.line > s.line && nlsemi {
				// A multi-line comment acts like a newline;
				// it translates to a ';' if nlsemi is set.
				s.lit = "newline"
				s.tok = _Semi
				break
			}
			goto redo
		}
		s.op = Div
		goto assignop

	case '%':
		s.op = Rem
		c = s.getr()
		goto assignop

	case '&':
		c = s.getr()
		if c == '&' {
			s.op = AndAnd
			s.tok = _Operator
			break
		}
		s.op = And
		if c == '^' {
			s.op = AndNot
			c = s.getr()
		}
		goto assignop

	case '|':
		c = s.getr()
		if c == '|' {
			s.op = OrOr
			s.tok = _Operator
			break
		}
		s.op = Or
		goto assignop

	case '^':
		s.op = Xor
		c = s.getr()
		goto assignop

	case '<':
		c = s.getr()
		if c == '=' {
			s.op = Leq
			s.tok = _Operator
			break
		}
		if c == '<' {
			// TODO <<<
			s.op = Shl
			c = s.getr()
			goto assignop
		}
		s.ungetr()
		s.op = Lss
		s.tok = _Operator

	case '>':
		c = s.getr()
		if c == '=' {
			s.op = Geq
			s.tok = _Operator
			break
		}
		if c == '>' {
			// TODO <<<
			s.op = Shr
			c = s.getr()
			goto assignop
		}
		s.ungetr()
		s.op = Gtr
		s.tok = _Operator

	case '=':
		if s.getr() == '=' {
			// TODO ===
			s.op = Eql
			s.tok = _Operator
			break
		}
		s.ungetr()
		s.tok = _Assign

	case '!':
		if s.getr() == '=' {
			// TODO !==
			s.op = Neq
			s.tok = _Operator
			break
		}
		s.ungetr()
		s.op = Not
		s.tok = _Operator

	default:
		s.tok = 0
		s.error(fmt.Sprintf("invalid character %#U", c))
		goto redo
	}

	return

assignop:
	if c == '=' {
		s.tok = _AssignOp
		return
	}
	s.ungetr()
	s.tok = _Operator
}

func isLetter(c rune) bool {
	return 'a' <= c && c <= 'z' || 'A' <= c && c <= 'Z' || c == '_'
}

func isDigit(c rune) bool {
	return '0' <= c && c <= '9'
}

func (s *scanner) ident() {
	s.startLit()

	// accelerate common case (7bit ASCII)
	c := s.getr()
	for isLetter(c) || isDigit(c) {
		c = s.getr()
	}

	// general case
	if c >= utf8.RuneSelf {
		for s.isIdentRune(c, false) {
			c = s.getr()
		}
	}
	s.ungetr()

	lit := s.stopLit()

	// possibly a keyword
	if len(lit) >= 2 {
		if tok := keywordMap[hash(lit)]; tok != 0 && tokstrings[tok] == string(lit) {
			// TODO check semi insertion rules
			s.nlsemi = contains(1<<_Break|1<<_Continue|1<<_Return, tok)
			s.tok = tok
			return
		}
	}

	s.nlsemi = true
	s.lit = string(lit)
	s.tok = _Name
}

func (s *scanner) isIdentRune(c rune, first bool) bool {
	switch {
	case unicode.IsLetter(c) || c == '_':
		// ok
	case unicode.IsDigit(c):
		if first {
			s.error(fmt.Sprintf("identifier cannot begin with digit %#U", c))
		}
	case c >= utf8.RuneSelf:
		s.error(fmt.Sprintf("invalid identifier character %#U", c))
	default:
		return false
	}
	return true
}

// hash is a perfect hash function for keywords.
// It assumes that s has at least length 2.
func hash(s []byte) uint {
	return (uint(s[0])*31*31 + uint(s[1])*31 + uint(len(s))) % uint(len(keywordMap)-1)
	//return (uint(s[0])<<4 ^ uint(s[1]) + uint(len(s))) & uint(len(keywordMap)-1)
}

var keywordMap [1 << 12]token // size must be power of two

func init() {
	// populate keywordMap
	for tok := _Break; tok <= _With; tok++ {
		h := hash([]byte(tokstrings[tok]))
		if keywordMap[h] != 0 {
			panic("imperfect hash: " + keywordMap[h].String() + " == " + tok.String())
		}
		keywordMap[h] = tok
	}
}

func (s *scanner) number(c rune) {
	s.startLit()

	if c != '.' {
		s.kind = NumLit // until proven otherwise
		if c == '0' {
			c = s.getr()
			if c == 'x' || c == 'X' {
				// hex
				c = s.getr()
				hasDigit := false
				for isDigit(c) || 'a' <= c && c <= 'f' || 'A' <= c && c <= 'F' {
					c = s.getr()
					hasDigit = true
				}
				if !hasDigit {
					s.error("malformed hex constant")
				}
				goto done
			}

			// decimal 0, octal, or float
			has8or9 := false
			for isDigit(c) {
				if c > '7' {
					has8or9 = true
				}
				c = s.getr()
			}
			if c != '.' && c != 'e' && c != 'E' && c != 'i' {
				// octal
				if has8or9 {
					s.error("malformed octal constant")
				}
				goto done
			}

		} else {
			// decimal or float
			for isDigit(c) {
				c = s.getr()
			}
		}
	}

	// float
	if c == '.' {
		s.kind = NumLit
		c = s.getr()
		for isDigit(c) {
			c = s.getr()
		}
	}

	// exponent
	if c == 'e' || c == 'E' {
		s.kind = NumLit
		c = s.getr()
		if c == '-' || c == '+' {
			c = s.getr()
		}
		if !isDigit(c) {
			s.error("malformed floating-point constant exponent")
		}
		for isDigit(c) {
			c = s.getr()
		}
	}

done:
	s.ungetr()
	s.nlsemi = true
	s.lit = string(s.stopLit())
	s.tok = _Literal
}

func (s *scanner) rune() {
	s.startLit()

	ok := true // only report errors if we're ok so far
	n := 0
	for ; ; n++ {
		r := s.getr()
		if r == '\'' {
			break
		}
		if r == '\\' {
			if !s.escape('\'') {
				ok = false
			}
			continue
		}
		if r == '\n' {
			s.ungetr() // assume newline is not part of literal
			if ok {
				s.error("newline in character literal")
				ok = false
			}
			break
		}
		if r < 0 {
			if ok {
				s.errh(s.line, s.col, "invalid character literal (missing closing ')")
				ok = false
			}
			break
		}
	}

	if ok {
		if n == 0 {
			s.error("empty character literal or unescaped ' in character literal")
		} else if n != 1 {
			s.errh(s.line, s.col, "invalid character literal (more than one character)")
		}
	}

	s.nlsemi = true
	s.lit = string(s.stopLit())
	s.kind = StringLit
	s.tok = _Literal
}

func (s *scanner) stdString(start rune) {
	s.startLit()

	for {
		r := s.getr()
		if r == start {
			break
		}
		if r == '\\' {
			s.escape(start)
			continue
		}
		if r == '\n' {
			s.ungetr() // assume newline is not part of literal
			s.error("newline in string")
			break
		}
		if r < 0 {
			s.errh(s.line, s.col, "string not terminated")
			break
		}
	}

	s.nlsemi = true
	s.lit = string(s.stopLit())
	s.kind = StringLit
	s.tok = _Literal
}

func (s *scanner) skipLine(r rune) {
	for r >= 0 {
		if r == '\n' {
			s.ungetr() // don't consume '\n' - needed for nlsemi logic
			break
		}
		r = s.getr()
	}
}

func (s *scanner) lineComment() {
	r := s.getr()
	s.skipLine(r)
}

func (s *scanner) fullComment() {
	for {
		r := s.getr()
		for r == '*' {
			r = s.getr()
			if r == '/' {
				return
			}
		}
		if r < 0 {
			s.errh(s.line, s.col, "comment not terminated")
			return
		}
	}
}

func (s *scanner) escape(quote rune) bool {
	var n int
	var base, max uint32

	c := s.getr()
	switch c {
	case 'a', 'b', 'f', 'n', 'r', 't', 'v', '\\', quote:
		return true
	case '0', '1', '2', '3', '4', '5', '6', '7':
		n, base, max = 3, 8, 255
	case 'x':
		c = s.getr()
		n, base, max = 2, 16, 255
	case 'u':
		c = s.getr()
		n, base, max = 4, 16, unicode.MaxRune
	case 'U':
		c = s.getr()
		n, base, max = 8, 16, unicode.MaxRune
	default:
		if c < 0 {
			return true // complain in caller about EOF
		}
		s.error("unknown escape sequence")
		return false
	}

	var x uint32
	for i := n; i > 0; i-- {
		d := base
		switch {
		case isDigit(c):
			d = uint32(c) - '0'
		case 'a' <= c && c <= 'f':
			d = uint32(c) - ('a' - 10)
		case 'A' <= c && c <= 'F':
			d = uint32(c) - ('A' - 10)
		}
		if d >= base {
			if c < 0 {
				return true // complain in caller about EOF
			}
			kind := "hex"
			if base == 8 {
				kind = "octal"
			}
			s.error(fmt.Sprintf("non-%s character in escape sequence: %c", kind, c))
			s.ungetr()
			return false
		}
		// d < base
		x = x*base + d
		c = s.getr()
	}
	s.ungetr()

	if x > max && base == 8 {
		s.error(fmt.Sprintf("octal escape value > 255: %d", x))
		return false
	}

	if x > max || 0xD800 <= x && x < 0xE000 /* surrogate range */ {
		s.error("escape sequence is invalid Unicode code point")
		return false
	}

	return true
}
