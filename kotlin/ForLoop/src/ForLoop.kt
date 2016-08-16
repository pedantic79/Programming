/**
 * Created by dnn on 8/15/16.
 */

class BreakException : Exception("This is a break exception.") {}

/**
 * Use "throw BreakException()" to perform a break, and return@cfor to perform a continue
 */
inline fun cfor(init: () -> Unit, cond: () -> Boolean, itr: () -> Unit, mainLoop: () -> Unit) {
    init()
    try {
        while (cond()) {
            mainLoop()
            itr()
        }
    }
    catch (e: BreakException) {
        // Do nothing. Just except the while loop
    }
}

fun main(args: Array<String>) {
    var i = 0
    cfor({i = 3}, {i < 10}, {i++}) {
        if (i > 7) {
            println("loop ${i}")
            throw BreakException()
        }
        println("Hello World ${i}")
    }

    println(i)
}
