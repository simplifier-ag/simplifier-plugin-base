import sbt.*

object SimplifierPluginDependencies {

  val pluginBase = "io.simplifier" %% "simplifier-plugin-base" % "0.5.0" withSources() exclude("org.scala-lang.modules", "scala-xml")

  val mysql = "mysql" % "mysql-connector-java" % "5.1.47"

}
