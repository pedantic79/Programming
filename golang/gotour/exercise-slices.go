package main

import (
	"golang.org/x/tour/pic"
	"math/rand"
	"time"
)

func Pic(fn func(int, int) int) func(int, int) [][]uint8 {
	return func(dx, dy int) [][]uint8 {
		img := make([][]uint8, dy)
		for y := 0; y < dy; y++ {
			img[y] = make([]uint8, dx)
			for x := 0; x < dx; x++ {
				img[y][x] = uint8(fn(x, y))
			}
		}
		return img
	}
}

func main() {
	rand.Seed(time.Now().UTC().UnixNano())
	fn := []func(int, int) int{
		func(x, y int) int { return x ^ y },
		func(x, y int) int { return x * y },
		func(x, y int) int { return (x + y) / 2 },
	}

	pic.Show(Pic(fn[rand.Intn(len(fn))]))
}
