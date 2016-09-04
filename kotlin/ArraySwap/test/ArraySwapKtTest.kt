import com.pholser.junit.quickcheck.Property
import com.pholser.junit.quickcheck.runner.JUnitQuickcheck
import org.junit.Assert.*
import org.junit.runner.RunWith

/**
 * Created by dnn on 8/16/16.
 */

fun checkSingleSwap(ar: Array<Int>): Boolean {
    var count = 0
    val cp = ar.copyOf()
    cp.sort()

    for (i in ar.indices) {
        if (ar[i] != cp[i]) count++
    }

    if (count > 2) return false
    return true
}

fun ae(ar: Array<Int>) {
    assertEquals(checkSingleSwap(ar), checkArray(ar))
}

class ArraySwapKtTest {
    @org.junit.Test
    fun checkArray() {
        ae(arrayOf(1, 2, 3))
        ae(arrayOf(1, 2, 3, 2, 3, 3))
        ae(arrayOf(2, 3, 1, 4, 5))
        ae(arrayOf(1, 2, 3, 2))
        ae(arrayOf(1, 2, 5, 4, 3))
        ae(arrayOf(1, 2, 3, 5, 4))
        ae(arrayOf(1, 4, 3, 2, 5))
        ae(arrayOf(1, 5, 4, 3, 2))
        ae(arrayOf(1, 5, 3, 3, 7))
        ae(arrayOf(2, 2, 1, 3, 7))
        ae(arrayOf(2, 3, 1, 3, 7))
        ae(arrayOf(1, 3, 1, 3, 7))
        ae(arrayOf(2, 1, 1, 3, 7))
        ae(arrayOf(1, 2, 3, 4))
        ae(arrayOf(4, 2, 3, 1))
        ae(arrayOf(1, 2, 3, 2, 2, 3))
        ae(arrayOf(1, 2, 3, 2, 1))
        ae(arrayOf(1, 2, 3, 2, 2, 3, 3, 4))
        ae(arrayOf(1, 2, 6, 2, 4, 6, 8))
        ae(arrayOf(1, 2, 3, 3, 2))
        ae(arrayOf(1, 3, 5, 5, 3))
        ae(arrayOf(5, 5, 5, 5, 3))
    }

}


@RunWith(JUnitQuickcheck::class)
class SymmetricKeyCryptographyProperties {
    @Property
    fun decryptReversesEncrypt(a: Array<Int>) {

        ae(a)
    }
}