/**
 * Created by dnn on 8/15/16.
 */


inline fun cfor(init: () -> Unit, cond: () -> Boolean, itr: () -> Unit, mainLoop: () -> Unit) {
    init()
    while (cond()) {
        mainLoop()
        itr()
    }
    return
}

fun main(args: Array<String>) {
    var i = 0
    cfor({i = 3}, {i < 10}, {i++}) {
        println("Hello World ${i}")
    }

    println(i)
}
