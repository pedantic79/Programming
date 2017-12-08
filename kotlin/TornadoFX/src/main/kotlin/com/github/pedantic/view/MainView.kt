package com.github.pedantic.view

import com.github.pedantic.app.EventController
import com.github.pedantic.piStartingAt
import com.github.thomasnield.rxkotlinfx.actionEvents
import io.reactivex.Emitter
import io.reactivex.Observable
import io.reactivex.rxkotlin.subscribeBy
import io.reactivex.rxkotlin.toObservable
import io.reactivex.subjects.PublishSubject
import io.reactivex.subjects.Subject
import javafx.beans.property.SimpleBooleanProperty
import javafx.beans.property.SimpleDoubleProperty
import javafx.beans.property.SimpleStringProperty
import javafx.geometry.Insets
import tornadofx.*

class MainView : View("Pi Calc TornadoFX + RxKotlin") {
    private val controller: EventController by inject()
    private val enableButton = SimpleBooleanProperty(true)
    private val number = SimpleStringProperty("100")
    private val percentage = SimpleDoubleProperty(0.0)

    override val root = vbox()

    init {
        with(root) {
            primaryStage.minWidth = 400.toDouble()

            textarea {
                minHeight = 800.toDouble()
                isWrapText = true
                isEditable = false


                controller.button.toObservable().subscribe {

                    runAsync(daemon = true) {
                        text = "3."
                        val digits = number.value.toInt()

                        val pipe = PublishSubject.create<String>().toSerialized()

                        pipe.scan(0, {x,_ -> x + 1}).subscribe {
                            percentage.set(it.toDouble()/digits.toDouble())
                        }

                        pipe.subscribeBy(
                                onNext = { s -> runAsync(daemon = true) {} success { appendText(s) } },
                                onComplete = {
                                    enableButton.set(true)
                                })
                        piDigits(digits, pipe)
                    }
                }
            }
            hbox {
                maxHeight = 100.toDouble()
                button("Go!") {
                    minHeight = 50.toDouble()
                    minWidth = 50.toDouble()

                    hboxConstraints { margin = Insets(5.0) }

                    controller.button += actionEvents().map { Unit }

                    enableWhen(enableButton)
                    controller.button.toObservable().subscribe {
                        enableButton.set(false)
                    }
                }
                textfield {
                    height
                    minHeight = 50.toDouble()
                    minWidth = 500.toDouble()

                    hboxConstraints { margin = Insets(5.0) }
                    bind(number)
                }
                progressindicator {
                    hboxConstraints { margin = Insets(5.0) }

                    setMaxSize(100.0, 100.0)
                    bind(percentage)
                }
            }
        }
    }
}

fun piDigits(max: Int, s: Subject<String>): Subject<String> {
    with(s) {
        val gen = { state: Int, emitter: Emitter<Int> ->
            emitter.onNext(state)
            state + 9
        }

        Observable.generate({1}, gen)
                .map { piStartingAt(it) }
                .flatMap { it.toCharArray().toTypedArray().toObservable() }
                .take(max.toLong())
                .buffer(1)
                .map { it.toCharArray().joinToString(separator = "") }
                .subscribe { onNext(it) }
        onComplete()
    }
    return s
}