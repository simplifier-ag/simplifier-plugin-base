ThisBuild / version := "1.0.2"

ThisBuild / scalaVersion := "2.12.15"

ThisBuild / organization := "io.simplifier"

ThisBuild / useCoursier := true

lazy val compileSettings = Seq(
  scalacOptions := Seq(
    "-unchecked", "-deprecation", "-feature", "-encoding", "utf8",
    "-Xmax-classfile-name", "100", "-Ypatmat-exhaust-depth", "off"),
  javacOptions ++= Seq("-source", "1.8", "-target", "1.8")
)

lazy val root = (project in file("."))
  .settings(
    name := "simplifier-plugin-base",
    PublishToMavenCentral.settings,
    compileSettings,
    licenses := Seq(
      ("MIT", url("http://opensource.org/licenses/MIT"))
    ),
    pomExtra := (
      <developers>
        <developer>
          <id>C-Schwemin</id>
          <name>Christoph Schwemin</name>
        </developer>
      </developers>
    ),

    libraryDependencies ++= Seq(
      "com.typesafe.akka" %% "akka-actor" % akkaV withSources() withJavadoc(),
      "com.typesafe.akka" %% "akka-slf4j" % akkaV withSources() withJavadoc(),
      "com.typesafe.akka" %% "akka-stream" % akkaV withSources() withJavadoc(),
      "com.typesafe.akka" %% "akka-http-core" % akkaHttpV withSources() withJavadoc(),
      "com.typesafe.akka" %% "akka-parsing" % akkaHttpV withSources() withJavadoc(),
      "com.typesafe.akka" %% "akka-http" % akkaHttpV withSources() withJavadoc(),
      "com.typesafe.akka" %% "akka-http-xml" % akkaHttpV withSources() withJavadoc(),
      "com.typesafe.akka" %% "akka-http-testkit" % akkaHttpV % "test" withSources() withJavadoc(),
      "com.typesafe.akka" %% "akka-testkit" % akkaV % "test" withSources() withJavadoc(),
      "com.github.swagger-akka-http" %% "swagger-akka-http" % "0.13.0" withSources() withJavadoc() exclude("com.google.guava", "guava"),
      "joda-time" % "joda-time" % jodaV withSources() withJavadoc(),
      "com.google.guava" % "guava" % guavaV withSources() withJavadoc(),
      "com.google.jimfs" % "jimfs" % jimFsV withSources() withJavadoc() exclude("com.google.guava", "guava"),
      "org.apache.commons" % "commons-dbcp2" % commonsDbcpV withSources() withJavadoc(),
      "org.squeryl" %% "squeryl" % squerylV withSources() withJavadoc(),
      "org.flywaydb" % "flyway-core" % flywayV withSources() withJavadoc(),
      "ch.qos.logback" % "logback-classic" % "1.2.3" withSources() withJavadoc(),
      "io.github.simplifier-ag" % "simplifier-plugin-api_2.12" % "0.6.0" withSources(),
      "org.scalatest" %% "scalatest" % "3.1.4" % "test" withSources() withJavadoc(),
      "ch.qos.logback" % "logback-classic" % "1.2.3" withSources() withJavadoc(),
      "org.mockito" %% "mockito-scala" % "1.17.7" % Test,
      "com.github.swagger-akka-http" %% "swagger-akka-http" % "0.13.0" withSources() withJavadoc() exclude("com.google.guava", "guava")
    ),

    dependencyOverrides ++= akkaOverrides ++ netty3Overrides,

    assembly / test := {},
    assembly / aggregate := false,

    assembly / assemblyJarName := s"simplifier-plugin-base_${scalaBinaryVersion.value}-${version.value}.jar",
  )

lazy val jodaV = "2.10"
lazy val guavaV = "32.1.1-jre"
lazy val jimFsV = "1.1"
lazy val squerylV = "0.9.5-7"
lazy val flywayV = "7.15.0"
lazy val commonsDbcpV = "2.1.1"

lazy val configV = "1.4.2"
lazy val akkaV = "2.6.20"
lazy val akkaHttpV = "10.2.10"

lazy val akkaOverrides = Seq(
  "com.typesafe.akka" %% "akka-actor" % akkaV,
  "com.typesafe.akka" %% "akka-stream" % akkaV
)
lazy val netty3Overrides = Seq(
  "io.netty" % "netty" % "3.10.6.Final"
)
