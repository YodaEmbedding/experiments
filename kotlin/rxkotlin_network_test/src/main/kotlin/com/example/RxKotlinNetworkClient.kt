package com.example.rxkotlin_network

import io.reactivex.*
import io.reactivex.internal.schedulers.*
import io.reactivex.rxkotlin.*
import io.reactivex.subjects.*
import java.io.BufferedReader
import java.io.DataOutputStream
import java.io.InputStreamReader
import java.net.Socket
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.util.concurrent.CountDownLatch
import java.util.concurrent.TimeUnit

fun displayString(s: String) {
    var formatter = DateTimeFormatter.ofPattern("ss.S")
    val now = LocalDateTime.now().format(formatter)
    val threadName = Thread.currentThread().name
    println("$now: $threadName, $s")
}

fun main(args: Array<String>) {
    val socket = Socket("localhost", 5678)
    val outputStream = DataOutputStream(socket.outputStream)
    val networkWriteDone = CountDownLatch(1)
    val networkReadThread = NetworkReadThread(socket.inputStream)
    networkReadThread.start()

    Observable.just("Hahn", "Heifetz", "Paganini", "Perlman", "Oistrakh")
        .zipWith(Observable.interval(1000, TimeUnit.MILLISECONDS))
        .map { (x, _) -> x }
        // TODO Handle java.net.SocketException?
        .doOnNext { outputStream.writeUTF("$it\n") }
        .doFinally { networkWriteDone.countDown() }
        .subscribeOn(IoScheduler())
        .subscribeBy(
            { error ->
                error.printStackTrace()
                networkReadThread.interrupt()
            },
            { },
            { item ->
                displayString("Wrote $item")
            })

    networkReadThread.outputStream
        .subscribeOn(IoScheduler())
        .subscribeBy { displayString("Read $it") }

    networkReadThread.join()
    networkWriteDone.await()
    println("\nEnded gracefully")
}
