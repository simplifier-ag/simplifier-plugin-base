package io.simplifier.pluginbase

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.concurrent.duration._
import scala.concurrent.{Await, Future, Promise}
import scala.util.Success

class StatefulProcessTest extends AnyWordSpec with Matchers {

  import scala.concurrent.ExecutionContext.Implicits.global

  val timeout: FiniteDuration = 5.seconds

  "A stateful process with only start function" when {
    "started with successful future" should {
      "yield returned value" in new SingleStartFixture {

        val startFuture: Future[String] = proc.startUp()
        startFuture.isCompleted mustBe false

        promise.success("resolved")
        Await.result(startFuture, timeout) mustBe "resolved"
      }
      "be stopped directly when started" in new SingleStartFixture {

        proc.startUp()
        promise.success("resolved")

        val stopFuture: Future[Unit] = proc.shutDown()
        Await.result(stopFuture, timeout)
      }
      "be stopped immediately if not running" in new SingleStartFixture {
        val stopFuture: Future[Unit] = proc.shutDown()
        stopFuture.isCompleted mustBe true
        stopFuture.value mustBe Some(Success(()))
        Await.result(stopFuture, timeout)
      }
      "be stopped directly if already stopped" in new SingleStartFixture {
        proc.startUp()
        promise.success("resolved")

        val stopFuture1: Future[Unit] = proc.shutDown()
        Await.ready(stopFuture1, timeout)

        val stopFuture2: Future[Unit] = proc.shutDown()
        Await.result(stopFuture2, timeout)
      }
    }

    "started with failed future" should {
      "yield failure when started" in new SingleStartFixture {

        val startFuture: Future[String] = proc.startUp()
        startFuture.isCompleted mustBe false

        promise.failure(new MyException)

        intercept[MyException] {
          Await.result(startFuture, timeout)
        }
      }
      "be stopped successfully" in new SingleStartFixture {
        val startFuture: Future[String] = proc.startUp()
        promise.failure(new MyException)

        val stopFuture: Future[Unit] = proc.shutDown()
        val expected: Unit = ()
        Await.result(stopFuture, timeout) mustBe expected
      }
    }
  }

  "A stateful process with start and stop function" when {
    "started with successful future" should {
      "pass running state from startup to stop function" in new StartStopFixture {

        val startFuture: Future[String] = proc.startUp()
        startFuture.isCompleted mustBe false

        startPromise.success(magicWord)
        Await.result(startFuture, timeout) mustBe magicWord

        val stopFuture: Future[Unit] = proc.shutDown()
        stopPromise.success(())
        Await.result(stopFuture, timeout)
      }
      "be stopped directly when not running, without calling stop function" in new StartStopFixture {
        val stopFuture: Future[Unit] = proc.shutDown()
        Await.result(stopFuture, timeout)
      }
      "be stopped even when stop function throws an exception" in new StartStopFixture {

        val startFuture: Future[String] = proc.startUp()
        startPromise.success("this is not the magic word")
        Await.result(startFuture, timeout) must not be magicWord

        val stopFuture: Future[Unit] = proc.shutDown()
        stopPromise.success(())
        intercept[MyException] {
          Await.result(stopFuture, timeout)
        }
      }
    }
  }


  trait SingleStartFixture {

    val promise: Promise[String] = Promise[String]()
    val startFunc: () => Future[String] = () => promise.future

    val proc: StatefulProcess[String, Unit] = StatefulProcess(startFunc)

  }

  trait StartStopFixture {

    val magicWord = "resolved"
    val startPromise: Promise[String] = Promise[String]()
    val stopPromise: Promise[Unit] = Promise[Unit]()
    val startFunc: () => Future[String] = () => startPromise.future
    val stopFunc: String => Future[Unit] = (str: String) => Future {
      if (str != magicWord) throw new MyException
    } flatMap {
      _ => stopPromise.future
    }

    val proc: StatefulProcess[String, Unit] = StatefulProcess(startFunc, stopFunc)
  }

  class MyException extends Exception

}
