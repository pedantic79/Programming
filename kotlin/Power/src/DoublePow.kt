/**
 * Created by dnn on 9/5/16.
 */
@file:JvmName("Pow")
@file:JvmMultifileClass
package org.github.pedantic79

private fun Int.isEven(): Boolean = this % 2L == 0L

fun doublePowD(x: Double, y: Int): Double = fracPowD(x, y)

fun fracPowD(x: Double, y: Int): Double =
        if (y < 0) { 1.0/numPowD(x, -1 * y) }
        else       { numPowD(x, y) }

fun numPowD(x: Double, y: Int): Double  {
    if (y < 0) {
        throw IllegalArgumentException("Cannot raise to negative power")
    }
    return doPowD(x, y)
}

private fun doPowD(x: Double, y: Int): Double =
        if (y == 0) { 1.0 }
        else        { f(x, y) }

private tailrec fun f(x: Double, y: Int): Double =
        if (x.isInfinite())  { x }
        else if (y.isEven()) { f((x * x), (y / 2)) }
        else if (y == 1)     { x }
        else                 { g((x * x), (y / 2), x) }

private tailrec fun g(x: Double, y: Int, z: Double): Double =
        if (x.isInfinite())   { x }
        else if (y.isEven())  { g((x * x), (y / 2), z) }
        else if (y == 1)      { x * z }
        else                  { g((x * x), ((y - 1)/ 2), x * z) }
