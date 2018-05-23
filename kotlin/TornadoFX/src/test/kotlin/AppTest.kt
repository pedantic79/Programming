import com.github.pedantic.app.MyApp
import javafx.stage.Stage
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.extension.ExtendWith
import org.testfx.api.FxAssert.verifyThat
import org.testfx.framework.junit5.ApplicationExtension
import org.testfx.framework.junit5.Start
import org.testfx.framework.junit5.Stop
import org.testfx.util.NodeQueryUtils.*

@ExtendWith(ApplicationExtension::class)
class AppTest {

    @Start
    fun start(stage: Stage) {
        MyApp().start(stage)
    }

    @Test
    fun buttonContains() {
        verifyThat(".button", hasText("Go!"))
    }

//    @Test
//    fun simpleTest(robot: Robot) {
//        verifyThat(".button", isVisible())
//

//        robot.clickOn(".button")

//        var b = robot.lookup(".button").query<Button>()
//        if (b != null) {
//            while (b.isDisabled) {
//                robot.sleep(2000);
//            }
//        }


    }



}
