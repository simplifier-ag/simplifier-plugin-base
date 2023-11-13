package io.simplifier.pluginbase

import akka.http.scaladsl.util.FastFuture

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}
import io.simplifier.pluginbase

/**
  * Service initialization entry point, encapsulating the service startup, shutdown methods
  * and containing the internal state of the process.
  * @param startFunc function to call when the service is started
  * @param stopFunc function to call then the service is stopped
  * @param ec execution context
  * @tparam A type of the internal state
  * @tparam I type of the init param
  */
class StatefulProcess[A, I](startFunc: I => Future[A], stopFunc: A => Future[Unit])(implicit ec: ExecutionContext) {

  import StatefulProcess._

  @volatile
  private var internalState: State[A] = NotRunning

  @volatile
  private var startUpPromise: Promise[Unit] = Promise()

  /**
    * Start service up.
    * @return future containing the startup result
    */
  def startUp()(implicit ev: Unit =:= I): Future[A] = {
    startUp(ev(Unit))
  }

  /**
    * Start service up.
    * @param init initialization parameter
    * @return future containing the startup result
    */
  def startUp(init: => I): Future[A] = this.synchronized {
    internalState match {
      case NotRunning =>
        val startFuture = startFunc(init) andThen afterStart
        internalState = Starting(startFuture)
        startFuture
      case Starting(future) =>
        future
      case Running(state) =>
        Future.successful(state)
      case Stopping(_) =>
        Future.failed(new IllegalStateException("Cannot start process, while shutting down"))
    }
  }

  /**
    * Return future to be completed when the service startup finished.
    * @return future to await the startup of the service
    */
  def awaitStartup(): Future[Unit] = this.synchronized {
    startUpPromise.future
  }

  /**
    * Internal Callback after startup finished.
    */
  private def afterStart: PartialFunction[Try[A], Unit] = this.synchronized {
    case Success(state) =>
      internalState = Running(state)
      startUpPromise.success(())
    case Failure(e) =>
      internalState = NotRunning
      startUpPromise.failure(e)
  }

  /**
    * Service shut down.
    * @return future containing the shut down result
    */
  def shutDown(): Future[Unit] = this.synchronized {
    internalState match {
      case NotRunning =>
        Future.successful(())
      case Stopping(future) =>
        future
      case Starting(startFuture) =>
        val stopPromise = Promise[Unit]()
        startFuture.andThen {
          case Success(_) => stopPromise.completeWith(shutDown())
          case Failure(_) => stopPromise.success(())
        }
        stopPromise.future
      case Running(state) =>
        val stopFuture = stopFunc(state) andThen afterShutDown
        internalState = Stopping(stopFuture)
        stopFuture
    }
  }

  /**
    * Internal Callback after shutdown finished.
    */
  private def afterShutDown: PartialFunction[Try[Unit], Unit] = this.synchronized {
    case _ =>
      internalState = NotRunning
      startUpPromise = Promise()
  }

  /**
    * Build a new StatefulProcess by appending the given delegate function after a successful process start.
    * The original stateful processes are not modified, but a new process is returned.
    * @param delegate delegate function to call after the start process finished successfull
    * @return new StatefulProcess
    */
  def propagateStartTo(delegate: A => Unit): StatefulProcess[A, I] = {
    val newStartFunc: I => Future[A] = init => startFunc(init) map {
      successfulResult =>
        delegate(successfulResult)
        successfulResult
    }
    new StatefulProcess[A, I](newStartFunc, stopFunc)
  }

  /**
    * Build a new StatefulProcess which starts another StatefulProcess after this service was started successful.
    * The original stateful processes are not modified, but a new process is returned.
    * @param otherProcess other process
    * @tparam B other process's internal state
    * @return new StatefulProcess
    */
  def propagateStartToProcess[B](otherProcess: => StatefulProcess[B, A]): StatefulProcess[A, I] = {
    propagateStartTo(response => otherProcess.startUp(response))
  }

  /**
    * Build a new StatefulProcess by prepending the given delegate function before process shutdown.
    * It doesn't matter whether the delegated shutdown process was successful for continuation of this shutdown,
    * only that it is finished.
    * The original stateful processes are not modified, but a new process is returned.
    * @param delegate delegate function to be finished before shutdown is started
    * @return new  StatefulProcess
    */
  def propagateStopTo(delegate: () => Future[Unit]): StatefulProcess[A, I] = {
    val newStopFunc: A => Future[Unit] = state => delegate() flatMap {
      _ => stopFunc(state)
    } recoverWith {
      case NonFatal(_) => stopFunc(state)
    }
    new StatefulProcess[A, I](startFunc, newStopFunc)
  }

  /**
    * Build a new StatefulProcess by stopping another StatefulProcess before this service is stopped.
    * The original stateful processes are not modified, but a new process is returned.
    * @param otherProcess other process
    * @tparam B other process's internal state
    * @return new StatefulProcess
    */
  def propagateStopToProcess[B](otherProcess: => StatefulProcess[B, _]): StatefulProcess[A, I] = {
    propagateStopTo(() => otherProcess.shutDown())
  }

  /**
    * Propagate start and stop to other child process.
    * @see propagateStartTo
    * @see propagateStopTo
    * @param childProcess child process toi delegate to
    * @tparam B child process's internal state
    * @return new StatefulProcess
    */
  def withChildProcess[B](childProcess: => StatefulProcess[B, A]): StatefulProcess[A, I] = {
    propagateStartToProcess(childProcess).propagateStopToProcess(childProcess)
  }

}

object StatefulProcess {

  private[StatefulProcess] sealed trait State[+A]

  private[StatefulProcess] case object NotRunning extends State[Nothing]
  private[StatefulProcess] case class Starting[A](onReady: Future[A]) extends State[A]
  private[StatefulProcess] case class Running[A](state: A) extends State[A]
  private[StatefulProcess] case class Stopping(onReady: Future[Unit]) extends State[Nothing]

  /** Default, empty stop function. */
  def emptyStopFunc[A]: A => Future[Unit] = (_: A) => FastFuture.successful(())

  /**
    * Create new stateful process.
    * @param startFunc start function to call on service startup
    * @param stopFunc stop function to call on service shutdown
    * @param ec execution context
    * @tparam A internal state type
    * @return atomic initializer
    */
  def apply[A](startFunc: () => Future[A], stopFunc: A => Future[Unit] = emptyStopFunc)(implicit ec: ExecutionContext): StatefulProcess[A, Unit] = {
    new StatefulProcess[A, Unit](_ => startFunc(), stopFunc)
  }

  object ChildProcess {

    /**
      * Create new stateful child process.
      * @param startFunc start function to call on service startup
      * @param stopFunc stop function to call on service shutdown
      * @param ec execution context
      * @tparam A internal state type
      * @tparam I init parameter type
      * @return atomic initializer
      */
    def apply[A, I](startFunc: I => Future[A], stopFunc: A => Future[Unit] = emptyStopFunc)(implicit ec: ExecutionContext) = {
      new StatefulProcess(startFunc, stopFunc)
    }

  }

}
