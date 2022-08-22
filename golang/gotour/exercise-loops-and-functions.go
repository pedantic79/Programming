package main

import (
	"fmt"
	"math"
)

func Sqrt(x float64) float64 {
	zold, z := -1.0, 1.0

	for math.Abs(zold-z) > math.SmallestNonzeroFloat64 {
		zold, z = z, z-(z*z-x)/(2*z)
	}

	return z
}

func main() {
	fmt.Println(Sqrt(346346))
}
