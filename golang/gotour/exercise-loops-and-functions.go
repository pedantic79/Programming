package main

import (
	"fmt"
	"math"
)

func Sqrt(x float64) float64 {
	epsilon := math.Nextafter(1, 2) - 1
	zold, z := float64(-1), float64(1)
	count := 0

	for math.Abs(zold-z) > epsilon {
		count++
		zold, z = z, z-(z*z-x)/(2*z)
	}

	fmt.Printf("Iteration count: %d\n", count)
	return z
}

func main() {
	fmt.Println(Sqrt(346346))
}
