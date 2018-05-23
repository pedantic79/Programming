import com.github.pedantic.app.piStartingAt
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

class PiTest {

    @Test
    fun pi8Test() {
        assertEquals(piStartingAt(1), "141592653")
    }
}