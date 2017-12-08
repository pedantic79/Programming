package com.github.pedantic

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

fun mulMod(a: Long, b: Long, m: Int): Int {
    return (a * b % m).toInt()
}

// return the inverse of x mod y
fun invMod(x: Int, y: Int): Int {
    var q: Int
    var u = x
    var v = y
    var a = 0
    var c = 1
    var t: Int

    do {
        q = v / u

        t = c
        c = a - q * c
        a = t

        t = u
        u = v - q * u
        v = t
    } while (u != 0)

    a %= y
    if (a < 0) a += y

    return a
}

// return (a^b) mod m
fun powMod(aa: Int, bb: Int, m: Int): Int {
    var b = bb
    var r = 1
    var a = aa

    while (true) {
        if (b and 0x01 != 0) {
            r = mulMod(r.toLong(), a.toLong(), m)
        }

        b = b shr 1

        if (b == 0) {
            break
        }

        a = mulMod(a.toLong(), a.toLong(), m)
    }

    return r
}

// return true if n is prime
fun Int.isPrime(): Boolean {
    if (this % 2 == 0)
        return false

    val r = sqrt(toDouble()).toInt()
    var i = 3

    while (i <= r) {
        if (this % i == 0) {
            return false
        }
        i += 2
    }

    return true
}

// return the prime number immediately after n
fun nextPrime(nn: Int): Int {
    var n = nn
    do {
        n++
    } while (!n.isPrime())

    return n
}

fun piStartingAt(n: Int): String {
    var av: Int
    var vmax: Int
    val en = ((n + 20) * log2(10.toDouble())).toInt()
    var num: Int
    var den: Int
    var kq: Int
    var kq2: Int
    var t: Int
    var v: Int
    var s: Int
    var sum = 0.0

    var a = 3
    while (a <= 2 * en) {
        vmax = log((2 * en).toDouble(), a.toDouble()).toInt()
        av = 1

        for (i in 0 until vmax) av *= a

        s = 0
        num = 1
        den = 1
        v = 0
        kq = 1
        kq2 = 1

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
            num = mulMod(num.toLong(), t.toLong(), av)

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

            den = mulMod(den.toLong(), t.toLong(), av)
            kq2 += 2

            if (v > 0) {
                t = invMod(den, av)
                t = mulMod(t.toLong(), num.toLong(), av)
                t = mulMod(t.toLong(), k.toLong(), av)
                for (i in v until vmax) t = mulMod(t.toLong(), a.toLong(), av)
                s += t
                if (s >= av) s -= av
            }
        }

        t = powMod(10, n - 1, av)
        s = mulMod(s.toLong(), t.toLong(), av)
        sum = (sum + s.toDouble() / av.toDouble()) % 1.0
        a = nextPrime(a)
    }

    return String.format("%09d", (sum * 1e9).toInt())
}