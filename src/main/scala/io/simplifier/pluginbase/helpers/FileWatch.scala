package io.simplifier.pluginbase.helpers

import io.simplifier.pluginbase.util.logging.Logging
import org.apache.commons.io.filefilter.{FileFilterUtils, IOFileFilter}
import org.apache.commons.io.monitor.{FileAlterationListener, FileAlterationListenerAdaptor, FileAlterationMonitor, FileAlterationObserver}

import java.io.File

trait FileWatch extends Logging {

  def onChange(file: File): Unit

  def watchFile(fileToWatch: File): Unit = {
    val (directory, fileFilter): (File, IOFileFilter) = fileToWatch match {
      case file if file.isFile =>
        // For a specific file: watch parent directory, but filter for only this file
        val filter = FileFilterUtils.nameFileFilter(file.getName)
        (file.getParentFile, filter)

      case directory if directory.isDirectory =>
        // For a directory: watch it and accept all files in it
        (directory, FileFilterUtils.trueFileFilter())
    }

    // Create observer with file filter - only monitors files matching the filter
    val observer: FileAlterationObserver = new FileAlterationObserver(directory, fileFilter)
    val monitor: FileAlterationMonitor = new FileAlterationMonitor()

    val listener: FileAlterationListener = new FileAlterationListenerAdaptor {
      override def onFileChange(file: File): Unit = {
        super.onFileChange(file)
        // No filtering needed here anymore - the observer already filtered for us
        onChange(file)
      }
    }

    observer.addListener(listener)
    monitor.addObserver(observer)
    monitor.start()
  }

}