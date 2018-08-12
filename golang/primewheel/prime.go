package main

import (
	"fmt"
)

type Prime struct {
	primes  []int64
	rValues []int64
	rIndex  int

	factor     int64
	nextK      int64
	multiplier int64s

	loc int
}

func (p *Prime) checkCache(num int64) bool {
	for _, n := range p.primes {
		if n == num {
			return true
		}
	}
	return false
}

func (p *Prime) IsPrime(num int64) bool {
	if num <= p.primes[len(p.primes)-1] {
		return p.checkCache(num)
	}
	return checkCoPrime(num, p.primes)
}

func checkCoPrime(num int64, coprimes []int64) bool {
	for _, coprime := range coprimes {
		if coprime*coprime > num {
			return true
		}
		if num%coprime == 0 {
			return false
		}
	}
	return true
}

func (p *Prime) genCand() int64 {
	// Get the r value, if we are k*0+1, then get get the next value
	r := p.rValues[p.rIndex]
	if r == 1 && p.multiplier == 0 {
		r = p.rValues[p.rIndex]
		p.rIndex++
	}

	// Cand is k * n + r
	cand := p.factor*p.multiplier + r
	// fmt.Printf("cand = %d*%d+%d = %d\n", p.factor, p.multiplier, r, cand)

	// If rIndex is at the end, go back to the beginning
	// and reset the multiplier
	p.rIndex++
	if p.rIndex >= len(p.rValues) {
		p.rIndex = 0
		p.multiplier++
	}

	// fmt.Printf("%d > %d*%d=%d\n", cand, p.factor, p.nextK, p.factor*p.nextK)
	if cand > p.factor*p.nextK {
		p.growRValues()
		return p.genCand()
	}

	return cand
}

func (p *Prime) growRValues() {
	p.factor *= p.nextK

	// fmt.Println("GROW")
	p.rValues = nil
	p.rValues = append(p.rValues, 1)

	var multipliers []int64

	for _, prime := range p.primes {
		if prime > p.nextK {
			p.nextK = prime
			break
		}
	}

	for _, prime := range p.primes {
		if prime >= p.nextK {
			break
		}
		multipliers = append(multipliers, prime)
	}

	for prime := p.nextK; prime <= p.factor; prime += 2 {
		if checkCoPrime(prime, multipliers) {
			p.rValues = append(p.rValues, prime)
		}
	}

	p.multiplier = 1
	p.rIndex = 0

}

func (p *Prime) nextPrime() int64 {
	if p.loc < len(p.primes) {
		v := p.primes[p.loc]
		p.loc++
		return v
	}

	for {
		cand := p.genCand()
		if p.IsPrime(cand) {
			p.primes = append(p.primes, cand)
			p.loc++
			return cand
		}
	}
}

func main() {
	p := Prime{
		primes:     []int64{2, 3},
		rValues:    []int64{1, 5},
		factor:     6,
		nextK:      5,
		multiplier: 0,
	}

	for i := 0; i < 100; i++ {
		fmt.Printf("%d\n", p.nextPrime())
	}
}
