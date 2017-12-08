package com.github.pedantic.app

import javafx.scene.text.FontWeight
import tornadofx.*

class Styles : Stylesheet() {

    init {
        textField {
            fontWeight = FontWeight.BOLD
            fontSize = 25.px
        }
        textArea {
            fontSize = 20.px
        }
    }
}