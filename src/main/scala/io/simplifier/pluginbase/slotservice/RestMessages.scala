package io.simplifier.pluginbase.slotservice

import Constants._

object RestMessages extends GenericRestMessages {

  val (uploadPermissionFileSuccess, uploadPermissionFileError) = mkRestMessagePair(PERMISSION_FILE, ACTION_UPDATED)
  val (downloadPermissionFileSuccess, downloadPermissionFileError) = mkRestMessagePair(PERMISSION_FILE, ACTION_GET)

}
