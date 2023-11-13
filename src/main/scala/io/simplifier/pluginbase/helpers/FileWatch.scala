package io.simplifier.pluginbase.helpers

import io.simplifier.pluginbase.util.logging.Logging
import org.apache.commons.io.monitor.{FileAlterationListener, FileAlterationListenerAdaptor, FileAlterationMonitor, FileAlterationObserver}

import java.io.File

trait FileWatch extends Logging {

  def onChange(file: File): Unit

  def watchFile(fileToWatch: File) {
    val parentDirectory: File = fileToWatch match {
      case file if file.isFile => file.getParentFile
      case directory if directory.isDirectory => directory
    }
    val observer: FileAlterationObserver = new FileAlterationObserver(parentDirectory)
    val monitor: FileAlterationMonitor = new FileAlterationMonitor()
    val listener: FileAlterationListener = new FileAlterationListenerAdaptor {
      override def onStart(observer: FileAlterationObserver): Unit = {
        super.onStart(observer)
      }

      override def onFileChange(file: File): Unit = {
        super.onFileChange(file)
        if (fileToWatch.isFile && file.getPath.equals(fileToWatch.getPath))
          onChange(file)
        else if (fileToWatch.isDirectory)
          onChange(file)
      }
    }

    observer.addListener(listener)
    monitor.addObserver(observer)
    monitor.start()
  }

}