/**
 * Created by dnn on 9/5/16.
 */
@file:JvmName("IntegerPow")
package org.github.pedantic79

import java.math.BigInteger
import org.apache.commons.math3.fraction.BigFraction

private fun Long.isEven(): Boolean = this % 2L == 0L

fun doublePow(x: Long, y: Long): Double = fracPow(x, y).toDouble()

fun fracPow(x: Long, y: Long): BigFraction =
        if (y < 0) { BigFraction(BigInteger.ONE, numPow(x, -1 * y)) }
        else       { BigFraction(numPow(x, y), BigInteger.ONE) }

fun numPow(x: Long, y: Long): BigInteger  {
    if (y < 0) {
        throw IllegalArgumentException("Cannot raise to negative power")
    }
    return numPow(BigInteger.valueOf(x), y)
}

private fun numPow(x: BigInteger, y: Long): BigInteger =
        if (y == 0L) { BigInteger.ONE } else { f(x, y) }

private tailrec fun f(x: BigInteger, y: Long): BigInteger =
        if (y.isEven())   { f((x * x), (y / 2)) }
        else if (y == 1L) { x }
        else              { g((x * x), (y / 2), x) }

private tailrec fun g(x: BigInteger, y: Long, z: BigInteger): BigInteger =
        if (y.isEven()) { g((x * x), (y / 2), z) }
        else if (y == 1L) { x * z }
        else { g((x * x), (y / 2), x) }
