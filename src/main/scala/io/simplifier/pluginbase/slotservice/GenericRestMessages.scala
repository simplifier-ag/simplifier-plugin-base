package io.simplifier.pluginbase.slotservice

import io.swagger.annotations.ApiModelProperty

trait GenericRestMessages {

  import GenericRestMessages._

  /** Generator function for REST messages. */
  protected def mkRestMessagePair(dataObject: String, op: String, plural: Boolean = false): (RestMessage, String => RestMessage) = {
    val haveHas = if (plural) "have" else "has"
    val successMessage = RestMessage(typeSuccess, s"$dataObject $haveHas been $op successfully.")
    val errorMessage: String => RestMessage = { reason =>
      RestMessage(typeError, s"$dataObject cannot be $op due to the following reason: $reason.")
    }
    (successMessage, errorMessage)
  }

}

object GenericRestMessages {

  val typeError = "E"
  private val typeSuccess = "S"

  /** Rest message for JSON output */
  case class RestMessage(
                          @ApiModelProperty(value="the response message type") msgType: String,
                          @ApiModelProperty(value="the response message text") msgText: String)

}
