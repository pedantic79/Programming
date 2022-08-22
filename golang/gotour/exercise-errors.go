package main

import (
	"fmt"
	"math"
)

type ErrNegativeSqrt float64

func (e ErrNegativeSqrt) Error() string {
	return fmt.Sprintf("cannot Sqrt negative number: %f", float64(e))
}

func Sqrt(x float64) (float64, error) {
	if x < 0 {
		return 0, ErrNegativeSqrt(x)
	}

	zold, z := -1.0, 1.0

	for math.Abs(zold-z) > math.SmallestNonzeroFloat64 {
		zold, z = z, z-(z*z-x)/(2*z)
	}

	return z, nil
}

func PrintSqrt(x float64) {
	n, err := Sqrt(x)
	if err != nil {
		fmt.Println(err)
	} else {
		fmt.Println(n)
	}
}

func main() {
	PrintSqrt(2)
	PrintSqrt(-2)
}
