name := "fundoku"

scalaVersion := "2.11.8"

enablePlugins(ScalaJSPlugin)

scalaJSUseRhino in Global := false

scalacOptions ++= Seq("-deprecation", "-feature")

libraryDependencies ++= Seq(
  "org.scalatest" %%% "scalatest" % "3.0.0-M14" % "test",
  // "org.scalaz" %% "scalaz-core" % "7.2.4",
  "org.typelevel" %%% "cats" % "0.6.0",
  "org.scala-js" %%% "scalajs-dom" % "0.9.0",
  "com.lihaoyi" %%% "scalatags" % "0.5.5",
  "com.chuusai" %%% "shapeless" % "2.3.1"
)
