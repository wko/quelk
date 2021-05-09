name := "TemporalRewriter"
organization := "de.tu-dresden.lat"

version := "0.1"

scalaVersion := "2.12.6"



resolvers += Classpaths.typesafeReleases
resolvers += "jitpack" at "https://jitpack.io"

val ScalatraVersion = "2.7.0"

libraryDependencies ++= Seq(
  "org.scalatra" %% "scalatra" % ScalatraVersion,
  "org.scalatra" %% "scalatra-scalate" % ScalatraVersion,
  "org.scalatra" %% "scalatra-scalatest" % ScalatraVersion % "test",
  "org.scalatra"            %% "scalatra-forms"    % ScalatraVersion,
  "org.scalatra"            %% "scalatra-specs2"   % ScalatraVersion    % Test,
  "org.eclipse.jetty" % "jetty-webapp" % "9.4.27.v20200227" % "container",
  "javax.servlet" % "javax.servlet-api" % "3.1.0" % "provided",
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.0",
  "org.postgresql" % "postgresql" % "9.3-1100-jdbc4",
  "org.scalikejdbc" %% "scalikejdbc"       % "3.3.2",
  "com.typesafe.slick" %% "slick" % "3.2.0",
  "com.thoughtbot" % "monocats_2.12" % "0.1.1",
  "com.github.scopt" %% "scopt" % "3.7.0",
  "org.scala-lang.modules" %% "scala-xml" % "1.1.0",
  "com.github.matfax" % "pb-scala" % "v0.3.2",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.0",
  "org.scala-graph" %% "graph-core" % "1.12.5",
  "org.phenoscape" %% "scowl" % "1.3",
  "net.sourceforge.owlapi" %  "owlapi-distribution"    % "4.2.7",
  "org.semanticweb.elk" %  "elk-reasoner"    % "0.4.3",
  "org.semanticweb.elk" %  "elk-owlapi"    % "0.4.3",
  "org.scalatest" %% "scalatest" % "3.0.5" % Test,
  //"org.jgrapht" % "jgrapht-core" % "1.5.0"
)


enablePlugins(ScalatraPlugin)
