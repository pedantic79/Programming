package main

import (
	"io"
	"os"
	"strings"
)

type rot13Reader struct {
	r io.Reader
}

func rot13(b byte) byte {
	switch {
	case b >= 'A' && b <= 'M' || b >= 'a' && b <= 'm':
		b += 13
	case b >= 'N' && b <= 'Z' || b >= 'n' && b <= 'z':
		b -= 13
	}
	return b
}

func (r13 rot13Reader) Read(b []byte) (int, error) {
	n, err := r13.r.Read(b)
	if err != nil {
		return n, err
	}

	for i := 0; i < n; i++ {
		b[i] = rot13(b[i])
	}
	return n, nil
}

func main() {
	s := strings.NewReader("Lbh penpxrq gur pbqr!")
	r := rot13Reader{s}
	io.Copy(os.Stdout, &r)
}
