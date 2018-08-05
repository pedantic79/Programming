package main

import (
	"fmt"
	"golang.org/x/tour/tree"
)

// Walk walks the tree t sending all values
// from the tree to the channel ch.
func Walk(t *tree.Tree, ch chan int) {
	var walk func(*tree.Tree, chan int)

	walk = func(t *tree.Tree, ch chan int) {
		if t != nil {
			walk(t.Left, ch)
			ch <- t.Value
			walk(t.Right, ch)
		}
	}
	walk(t, ch)
	close(ch)
}

// Same determines whether the trees
// t1 and t2 contain the same values.
func Same(t1, t2 *tree.Tree) bool {
	c1 := make(chan int)
	c2 := make(chan int)
	go Walk(t1, c1)
	go Walk(t2, c2)

	for n1 := range c1 {
		if n1 != <-c2 {
			return false
		}
	}

	return true
}

func Test(x, y int, eq bool) {
	symb := "!="
	if eq {
		symb = "=="
	}

	same := Same(tree.New(x), tree.New(y))

	passed := "PASSED"
	if same != eq {
		passed = "FAILED"
	}

	fmt.Printf("tree.New(%d) %s tree.New(%d): %s\n", x, symb, y, passed)
}

func main() {
	c := make(chan int)
	go Walk(tree.New(1), c)

	for n := range c {
		fmt.Printf("%d ", n)
	}
	fmt.Printf("\n")

	Test(1, 1, true)
	Test(1, 2, false)
	Test(3, 2, false)
}
