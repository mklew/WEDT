import AssemblyKeys._

assemblySettings

test in assembly := {}

mainClass in assembly := Some("wedt.Boot")

name := "sentiment-analyzer"

val sprayV = "1.3.2"

val akkaV = "2.3.6"

val luceneV = "4.10.3"

scalaVersion := "2.11.4"

version := "0.0.1-SNAPSHOT"

libraryDependencies ++= Seq(  
  "org.mockito"             % "mockito-core"            % "1.9.5" % "test",
  "com.softwaremill.macwire" %% "macros"                % "0.7",
  "io.spray"                   %% "spray-testkit"          % sprayV  % "test",
  "ch.qos.logback"              % "logback-classic"        % "1.1.2",
  "ch.qos.logback"              % "logback-core"           % "1.1.2",
  "com.typesafe.akka"          %% "akka-actor"             % akkaV,
  "com.typesafe.akka"          %% "akka-remote"            % akkaV,
  "com.typesafe.akka"          %% "akka-slf4j"             % akkaV,
  "com.typesafe.akka"          %% "akka-testkit"           % akkaV   % "test",
  "com.typesafe.scala-logging" %% "scala-logging-slf4j"    % "2.1.2",
  "io.spray"                   %% "spray-can"              % sprayV,
  "io.spray"                   %% "spray-client"           % sprayV,
  "io.spray"                   %% "spray-routing"          % sprayV,
  "io.spray"                   %% "spray-caching"          % sprayV,
  "io.spray"                   %% "spray-json"             % "1.3.1", //another repo, has independent versioning
  "io.spray"                   %% "spray-testkit"          % sprayV  % "test",
  "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test",
  "org.slf4j"                   % "slf4j-api"              % "1.7.7",
  "com.typesafe"                % "config"                 % "1.2.1",
  "org.scala-lang.modules"     %% "scala-xml"              % "1.0.2",
  "net.ceedubs"                %% "ficus"                  % "1.1.1",
  "org.jsoup"                   % "jsoup"                  % "1.8.1",
  "org.apache.lucene"           % "lucene-core"            % luceneV,
  "org.apache.lucene"           % "lucene-analyzers-common" % luceneV,
  "org.apache.lucene"           % "lucene-queryparser"     % luceneV
)

fork := true

fork in Test := true

javaOptions in Test := Seq("-Xmx4G", "-XX:+UseConcMarkSweepGC", "-XX:MaxPermSize=2G", "-Xss4M")

scalacOptions ++= Seq("-feature")
