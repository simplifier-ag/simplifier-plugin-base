package io.simplifier.pluginbase.model

import scala.util.Try

/**
  * Transaction management abstraction, so mocking is possible
  */
class UserTransaction {

  def inSingleTransaction[A](block: => A): Try[A] = {
    import org.squeryl.PrimitiveTypeMode._
    Try(inTransaction {
      block
    })
  }

}
