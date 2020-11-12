package com.example

// import io.reactivex.Observable
import io.reactivex.*
import io.reactivex.internal.schedulers.*
import io.reactivex.rxkotlin.*
import io.reactivex.subjects.*
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

fun reverseString(s: String, delay: Long = 1000): String {
    Thread.sleep(delay)
    return s.reversed()
}

fun displayString(s: String) {
    var formatter = DateTimeFormatter.ofPattern("ss.S")
    val now = LocalDateTime.now().format(formatter)
    val threadName = Thread.currentThread().name
    println("$now: $threadName, $s")
}

fun main(args: Array<String>) {
    val list = listOf("Alpha", "Beta", "Gamma", "Delta", "Epsilon")
    // val stream1 = list.toObservable()
    // val stream2 = stream1
    //     // .subscribeOn(IoScheduler())
    //     // .subscribeOn(NewThreadScheduler())
    //     .map { reverseString(it) }
    //     .doOnNext { displayString(it) }
    // val stream3 = stream2
    //     .map { reverseString(it, 2000) }
    //     .doOnNext { displayString(it) }

    // (1..10).toObservable()
    //     // .subscribeOn(ComputationScheduler())
    //     .subscribeOn(IoScheduler())
    //     .map { Thread.sleep(500); it }
    //     .doOnNext { displayString(Thread.currentThread().name); it }
    //     .map { Thread.sleep(500); it }
    //     .doOnNext { displayString(Thread.currentThread().name); it }
    //     .subscribeBy {
    //         println("Subscriber received $it on ${Thread.currentThread().name}")
    //     }

    // (1..10).toObservable()
    //     // .subscribeOn(ComputationScheduler())
    //     .subscribeOn(IoScheduler())
    //     .flatMap { Thread.sleep(500); Observable.just(it) }
    //     .doOnNext { displayString(Thread.currentThread().name); it }
    //     .flatMap { Thread.sleep(500); Observable.just(it) }
    //     .doOnNext { displayString(Thread.currentThread().name); it }
    //     .blockingSubscribe {
    //         println("Subscriber received $it on ${Thread.currentThread().name}")
    //     }

    // var hot_stream = (1..10).toObservable().publish()
    //
    // hot_stream
    //     .subscribeOn(IoScheduler())
    //     .doOnNext { displayString(Thread.currentThread().name); it }
    //     .blockingSubscribe {
    //         println("Subscriber received $it on ${Thread.currentThread().name}")
    //     }

    // var hot = AsyncSubject.create<String>()
    // var hot = ReplaySubject.create<String>()
    // var hot = BehaviorSubject.create<String>()
    // var hot = PublishSubject.create<String>()
    // var hot = UnicastSubject.create<String>()

    // stream2
        // .subscribeOn(IoScheduler())
        // .subscribeOn(NewThreadScheduler())
        // .subscribe(hot)

    // displayString("Sleep")
    // Thread.sleep(3000)
    // Thread.sleep(1000)

    // TODO make the subject execute on a separate thread to the other stream

    // .subscribeOn(<my taskthreadExecutor>)
    // .observeOn(<my taskthreadExecutor>)

    // hot
    //     // .subscribeOn(IoScheduler())
    //     // .subscribeOn(NewThreadScheduler())
    //     .map { reverseString(it, 3000) }
    //     .doOnNext { displayString(it) }
    //     .subscribeOn(NewThreadScheduler())
    //     .blockingSubscribeBy { displayString(it) }

    // TODO figure out how flatMap works with "asynchronous" operations...
    // TODO see what ? does
    // TODO see what async does
    // TODO flowables?
    // TODO lazy observable? (defer)


    list
        .toObservable()
        .subscribeOn(ComputationScheduler())
        .doOnNext { displayString("$it (1)") }
        .flatMap { Observable.just(it, reverseString(it)) }
        .doOnNext { displayString("$it (2)") }
        .map { reverseString(it) }
        .doOnNext { displayString("$it (3)") }
        .subscribeBy { displayString("$it (done)") }

    Thread.sleep(10000)
    println("")

    list
        .toObservable()
        .subscribeOn(ComputationScheduler())
        .doOnNext { displayString("$it (1)") }
        .map { reverseString(it) }
        .observeOn(NewThreadScheduler())
        .doOnNext { displayString("$it (2)") }
        .map { reverseString(it) }
        // .flatMap { Observable.just(it, reverseString(it)) }
        .observeOn(IoScheduler())
        .subscribeBy { displayString("$it (done)") }

    Thread.sleep(10000)

    // (1..10).toFlowable()
    //     // .subscribeOn(ComputationScheduler())
    //     .subscribeOn(IoScheduler())
    //     .flatMap { Thread.sleep(500); Flowable.just(it) }
    //     .doOnNext { displayString(Thread.currentThread().name); it }
    //     .flatMap { Thread.sleep(500); Flowable.just(it) }
    //     .doOnNext { displayString(Thread.currentThread().name); it }
    //     .blockingSubscribe {
    //         println("Subscriber received $it on ${Thread.currentThread().name}")
    //     }

    // Thread.sleep(10000)

    // println("Execute stream3")
    // stream3
    //     .subscribeBy(
    //         onNext = { println(it) },
    //         onError = { it.printStackTrace() },
    //         onComplete = { println("Done!") }
    //     )
    //
    // println("\nExecute stream2")
    // stream2
    //     .subscribeBy(
    //         onNext = { println(it) },
    //         onError = { it.printStackTrace() },
    //         onComplete = { println("Done!") }
    //     )
    //
    // println("\nExecute stream1")
    // stream1
    //     .subscribeBy(
    //         onNext = { println(it) },
    //         onError = { it.printStackTrace() },
    //         onComplete = { println("Done!") }
    //     )

    // stream
    //     .subscribeBy(
    //         onNext = { println(it); throw Exception() },
    //         onError = { it.printStackTrace() },
    //         onComplete = { println("Done!") }
    //     )
    //
    // stream
    //     .filter { it.length >= 5 }
    //     .subscribeBy(
    //         onNext = { println(it) },
    //         onError = { it.printStackTrace() },
    //         onComplete = { println("Done!") }
    //     )
}
