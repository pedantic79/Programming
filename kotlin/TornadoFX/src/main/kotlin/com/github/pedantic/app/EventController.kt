package com.github.pedantic.app


import io.reactivex.Observable
import io.reactivex.disposables.Disposable
import io.reactivex.subjects.PublishSubject
import io.reactivex.subjects.Subject
import tornadofx.*


class EventController: Controller() {
    val button = SubjectObs<Unit>()
}

class SubjectObs<T> {
    private val subject: Subject<T> = PublishSubject.create<T>().toSerialized()

    fun toObservable(): Observable<T> = subject

    private fun add(observable: Observable<T>): Disposable =
            observable.subscribe({ subject.onNext(it) }, { subject.onError(it) }, { subject.onComplete() })

    operator fun plusAssign(map: Observable<T>) {
        add(map)
    }

}

