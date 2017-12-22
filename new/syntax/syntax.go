// Copyright 2016 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package syntax

import (
	"fmt"
	"io"
	"os"

	"github.com/eandre/jsb/new/src"
)

// Error describes a syntax error. Error implements the error interface.
type Error struct {
	Pos src.Pos
	Msg string
}

func (err Error) Error() string {
	return fmt.Sprintf("%s: %s", err.Pos, err.Msg)
}

var _ error = Error{} // verify that Error implements error

// An ErrorHandler is called for each error encountered reading a .js file.
type ErrorHandler func(err error)

// Parse parses a single JavaScript source file from src and returns the corresponding
// syntax tree. If there are errors, Parse will return the first error found,
// and a possibly partially constructed syntax tree, or nil if no correct package
// clause was found. The base argument is only used for position information.
//
// If errh != nil, it is called with each error encountered, and Parse will
// process as much source as possible. If errh is nil, Parse will terminate
// immediately upon encountering an error.
func Parse(base *src.PosBase, src io.Reader, errh ErrorHandler) (_ *File, first error) {
	defer func() {
		if p := recover(); p != nil {
			if err, ok := p.(Error); ok {
				first = err
				return
			}
			panic(p)
		}
	}()

	var p parser
	p.init(base, src, errh)
	p.next()
	return p.fileOrNil(), p.first
}

// ParseBytes behaves like Parse but it reads the source from the []byte slice provided.
func ParseBytes(base *src.PosBase, src []byte, errh ErrorHandler) (*File, error) {
	return Parse(base, &bytesReader{src}, errh)
}

type bytesReader struct {
	data []byte
}

func (r *bytesReader) Read(p []byte) (int, error) {
	if len(r.data) > 0 {
		n := copy(p, r.data)
		r.data = r.data[n:]
		return n, nil
	}
	return 0, io.EOF
}

// ParseFile behaves like Parse but it reads the source from the named file.
func ParseFile(filename string, errh ErrorHandler) (*File, error) {
	f, err := os.Open(filename)
	if err != nil {
		if errh != nil {
			errh(err)
		}
		return nil, err
	}
	defer f.Close()
	return Parse(src.NewFileBase(filename, filename), f, errh)
}
