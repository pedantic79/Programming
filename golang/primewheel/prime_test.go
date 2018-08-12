package main

import (
	"math/big"
	"testing"
)

func TestPrimes(t *testing.T) {
	p := Prime{
		primes:      []int64{2, 3},
		wheel:       []int64{1, 5},
		kFactor:     6,
		next:        5,
		kMultiplier: 0,
	}

	p.NextPrime()
	for i := int64(3); i < 2E9; i += 2 {
		if big.NewInt(i).ProbablyPrime(0) {
			num := p.NextPrime()
			if num != i {
				t.Errorf("%d is Prime, but our next prime is %d", i, num)
				return
			}
		}
	}
}
