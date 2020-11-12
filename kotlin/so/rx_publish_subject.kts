//DEPS io.reactivex.rxjava3:rxjava:3.0.5

import java.util.concurrent.CountDownLatch
import io.reactivex.rxjava3.core.*
import io.reactivex.rxjava3.schedulers.*
import io.reactivex.rxjava3.subjects.*

fun main() {
    val subject: PublishSubject<Int> = PublishSubject.create()
    val countDownLatch = CountDownLatch(1)
    val isSubscribedLatch = CountDownLatch(1)

    subject
        .subscribeOn(Schedulers.computation())
        .doOnSubscribe { isSubscribedLatch.countDown() }
        .map { it + 1 }
        .subscribe {
            countDownLatch.countDown()
            println(Thread.currentThread().name)
        }

    isSubscribedLatch.await()
    subject.onNext(1)
    countDownLatch.await()
}

main()
