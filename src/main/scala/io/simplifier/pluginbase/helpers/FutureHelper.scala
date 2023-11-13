package io.simplifier.pluginbase.helpers

import io.simplifier.pluginbase.util.logging.Logging

import scala.concurrent.{ExecutionContext, Future}

class FutureHelper extends Logging {

  def serialiseFutures[A](futures: Iterable[() => Future[A]], chunkSize: Integer)(implicit ec: ExecutionContext): Future[List[A]] = {
    val emptyFuture = Future(List.empty[A])
    val futureGroups = futures.grouped(chunkSize)
    logger.debug(s"Processing ${futures.size} futures in groups of $chunkSize.")
    futureGroups.foldLeft(emptyFuture) { (accFutures, nextGroup) =>
      accFutures.flatMap { accFutureResults =>
        val futuresToProcess = nextGroup.map(nextFuture => nextFuture())
        Future.sequence(futuresToProcess).map { processedFuture =>
          logger.debug(s"Future group with ${futuresToProcess.size} futures completed.")
          accFutureResults ++ processedFuture
        }
      }
    }
  }
}
