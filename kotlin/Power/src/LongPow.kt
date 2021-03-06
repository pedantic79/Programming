/**
 * Created by dnn on 9/5/16.
 */
@file:JvmName("Pow")
@file:JvmMultifileClass
package org.github.pedantic79

import org.apache.commons.math3.fraction.BigFraction

private fun Int.isEven(): Boolean = this % 2L == 0L

fun doublePowL(x: Long, y: Int): Double = fracPowL(x, y).toDouble()

fun fracPowL(x: Long, y: Int): BigFraction =
        if (y < 0) { BigFraction(1, numPowL(x, -1 * y)) }
        else       { BigFraction(numPowL(x, y), 1) }

fun numPowL(x: Long, y: Int): Long  {
    if (y < 0) {
        throw IllegalArgumentException("Cannot raise to negative power")
    }
    return doPowL(x, y)
}

private fun doPowL(x: Long, y: Int): Long =
        if (y == 0) { 1 }
        else { f(x, y) }

private tailrec fun f(x: Long, y: Int): Long =
        if (y.isEven())  { f((x * x), (y / 2)) }
        else if (y == 1) { x }
        else             { g((x * x), (y / 2), x) }

private tailrec fun g(x: Long, y: Int, z: Long): Long =
        if (y.isEven())  { g((x * x), (y / 2), z) }
        else if (y == 1) { x * z }
        else             { g((x * x), ((y - 1)/ 2), x * z) }
