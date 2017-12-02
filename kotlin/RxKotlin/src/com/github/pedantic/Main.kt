package com.github.pedantic

import io.reactivex.*
import io.reactivex.rxkotlin.subscribeBy
import io.reactivex.rxkotlin.toFlowable
import io.reactivex.schedulers.Schedulers
import io.reactivex.Flowable



fun main(args: Array<String>) {
    Flowable.just("Hello World").subscribe { println(it) }

    val list = listOf("Alpha", "Beta", "Gamma", "Delta", "Epsilon")

    list.toFlowable()
            .filter { it.length > 5 }
            .subscribeBy(
                    onNext = { println(it) },
                    onError = { it.printStackTrace() },
                    onComplete = { println("Done!") }
            )

    val obs: Observable<String> = Observable.create<String> { s ->
        s.onNext("Hello World")
        Thread.sleep(1000)
        s.onNext("Completed!")
        s.onComplete()
    }
    obs.subscribeOn(Schedulers.computation()).subscribe { v -> println("Observable.create: " + v) }



    val obs2 = Observable.fromArray(0, 1, 2, 3).map { x -> x + 1}
    obs2.subscribe { println("Observable.fromArray: " + it) }
    val v: Maybe<Int> = obs2.reduce { x, y -> x + y }
    println("Sum: " + v.blockingGet(0))


    Flowable.range(1,10)
            .flatMap {
                Flowable.just(it)
                        .subscribeOn(Schedulers.computation())
                        .map { x -> x * x }
            }
            .blockingSubscribe { println("Flatmap: " + it) }

    Flowable.range(1, 10)
            .parallel()
            .runOn(Schedulers.computation())
            .map { x -> x * x }
            .sequential()
            .blockingSubscribe { println("Parallel: " + it) }


    piDigits(20)
            .buffer(3)
            .map { it.toCharArray().joinToString(separator = "") }
            .scanWith({ "" }, { acc, value -> acc + value })
            .skip(1)
            .subscribe {println("pi: " + it)}
}

fun piDigits(max: Int): Flowable<Char> =
    Flowable.create({ f ->
        f.onNext('3')
        f.onNext('.')

        val gen = { state: Int, emitter: Emitter<Int> ->
            emitter.onNext(state)
            state + 9
        }

        Flowable.generate({ 1 }, gen)
                .map { piStartingAt(it) }
                .flatMap { it.toCharArray().toTypedArray().toFlowable() }
                .take(max.toLong())
                .subscribe { f.onNext(it) }

        f.onComplete()
    }, BackpressureStrategy.BUFFER)