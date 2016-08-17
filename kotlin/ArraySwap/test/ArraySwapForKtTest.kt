import org.junit.Before
import org.junit.Test

import org.junit.Assert.*

/**
 * Created by dnn on 8/16/16.
 */
class ArraySwapForKtTest {
    @Test
    fun cforTest() {
        var i = 0
        cfor({}, {i<10}, {i++}) {
            if (i > 5) {
                throw BreakException()
            }
        }

        assert(i == 6)
    }

    @Test
    fun swapTest() {
        var array = arrayOf(1,2,3)
        array.swap(0, 2)
        assertArrayEquals(array, arrayOf(3,2,1))
    }

}