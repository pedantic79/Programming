package main

import (
	"fmt"
	"math/rand"

	"golang.org/x/tour/tree"
)

func walkHelper(t *tree.Tree, ch chan int) {
	if t != nil {
		walkHelper(t.Left, ch)
		ch <- t.Value
		walkHelper(t.Right, ch)
	}
}

// Walk walks the tree t sending all values
// from the tree to the channel ch.
func Walk(t *tree.Tree, ch chan int) {
	walkHelper(t, ch)
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

	// check that the channels are both closed
	_, ok1 := <-c1
	_, ok2 := <-c2

	return !ok1 && !ok2
}

func Test(x, y int, a, b int, eq bool) {
	symb := "!="
	if eq {
		symb = "=="
	}

	same := Same(New(x, a), New(y, b))

	passed := "PASSED"
	if same != eq {
		passed = "FAILED"
	}

	fmt.Printf("tree.New(%d) %s tree.New(%d): %s\n", x, symb, y, passed)
}

func insert(t *tree.Tree, v int) *tree.Tree {
	if t == nil {
		return &tree.Tree{nil, v, nil}
	}
	if v < t.Value {
		t.Left = insert(t.Left, v)
	} else {
		t.Right = insert(t.Right, v)
	}
	return t
}

func new(k, len int) *tree.Tree {
	var t *tree.Tree
	for _, v := range rand.Perm(len) {
		t = insert(t, (1+v)*k)
	}
	return t
}

func main() {
	c := make(chan int)
	go Walk(tree.New(1), c)

	for n := range c {
		fmt.Printf("%d ", n)
	}
	fmt.Printf("\n")

	Test(1, 1, 10, 10, true)
	Test(1, 1, 10, 9, false)
	Test(1, 1, 9, 10, false)
	Test(1, 2, 10, 10, false)
	Test(3, 2, 10, 10, false)
}
