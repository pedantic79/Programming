package com.github.pedantic.app

/*
 * Computation of the n'th decimal digit of pi with very little memory.
 * Written by Fabrice Bellard on January 8, 1997.
 * Ported to C# by Chris Sells on May 5, 2002.
 *
 * We use a slightly modified version of the method described by Simon
 * Plouffe in "On the Computation of the n'th decimal digit of various
 * transcendental numbers" (November 1996). We have modified the algorithm
 * to get a running time of O(n^2) instead of O(n^3log(n)^3).
 *
 * This program uses mostly integer arithmetic. It may be slow on some
 * hardware where integer multiplications and division must be done
 * by software.
 */

import kotlin.math.log
import kotlin.math.log2
import kotlin.math.sqrt

fun mulMod(a: Int, b: Int, m: Int): Int = (a.toLong() * b.toLong() % m).toInt()

// return the inverse of x mod y
fun invMod(x: Int, y: Int): Int = x.toBigInteger().modInverse(y.toBigInteger()).toInt()

// return (a^b) mod m
fun powMod(aa: Int, bb: Int, m: Int): Int {
    var b = bb
    var r = 1
    var a = aa

    while (true) {
        if (b and 0x01 != 0) {
            r = mulMod(r, a, m)
        }

        b = b shr 1

        if (b == 0) {
            break
        }

        a = mulMod(a, a, m)
    }

    return r
}

// return true if n is prime
fun Int.isPrime(): Boolean {
    if (this % 2 == 0)
        return false

    val r = sqrt(toDouble()).toInt()
    (3..r step 2).forEach { i ->
        if (this % i == 0) {
            return false
        }
    }

    return true
}

// return the prime number immediately after n
tailrec fun nextPrime(n: Int): Int =
    when {
        n % 2 == 0 -> nextPrime(n + 1)
        !n.isPrime() -> nextPrime(n + 2)
        else -> n
    }

fun piStartingAt(n: Int): String {
    val en = ((n + 20) * log2(10.toDouble())).toInt()

    var t: Int
    var sum = 0.0
    var a = 3
    while (a <= 2 * en) {
        val vMax = log((2 * en).toDouble(), a.toDouble()).toInt()
        val av = (1..vMax).fold(1, {acc, _ -> acc * a})

        var s = 0
        var num = 1
        var den = 1
        var v = 0

        var kq = 1
        var kq2 = 1

        for (k in 1..en) {
            t = k
            if (kq >= a) {
                do {
                    t /= a
                    --v
                } while (t % a == 0)

                kq = 0
            }

            ++kq
            num = mulMod(num, t, av)

            t = 2 * k - 1
            if (kq2 >= a) {
                if (kq2 == a) {
                    do {
                        t /= a
                        ++v
                    } while (t % a == 0)
                }

                kq2 -= a
            }

            den = mulMod(den, t, av)
            kq2 += 2

            if (v > 0) {
                t = invMod(den, av)
                t = mulMod(t, num, av)
                t = mulMod(t, k, av)
                for (i in v until vMax) t = mulMod(t, a, av)
                s += t
                if (s >= av) s -= av
            }
        }

        t = powMod(10, n - 1, av)
        s = mulMod(s, t, av)
        sum = (sum + s.toDouble() / av.toDouble()) % 1.0
        a = nextPrime(a + 1)
    }

    return String.format("%09d", (sum * 1e9).toInt())
}