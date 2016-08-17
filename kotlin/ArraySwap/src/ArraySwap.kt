/**
 * Created by dnn on 8/16/16.
 */

class BreakException : Exception("Throw this exception to perform a cfor-break.") {}

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

fun <T> Array<T>.swap(n: Int, m: Int) {
    val tmp: T = get(n)
    set(n, get(m))
    set(m, tmp)
}

fun checkArray(arr: Array<Int>): Boolean {
    var i = 1
    cfor({}, {i < arr.size}, {i++}) {
        // Keep on looping until we are no longer equal or increasing
        if (arr[i - 1] <= arr[i]) {
            return@cfor
        }

        // find the left most index of the larger value
        var left = i - 1
        while (left - 1 >= 0 && arr[left - 1] == arr[left]) {
            left--
        }

        // find the right value to swap
        while (i < arr.size && arr[i] < arr[left]) {
            i++
        }
        val right = i - 1
        arr.swap(left, right)

        // Check to see if the rest of the array is sorted
        cfor({ i = if (left == 0) 1 else left }, { i < arr.size }, { i++ }) {
            if (arr[i - 1] > arr[i]) {
                arr.swap(left, right)
                return false
            }
        }
        arr.swap(left, right)
        return true
    }

    return true
}