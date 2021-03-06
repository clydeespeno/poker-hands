lazy val root = (project in file(".")).
  settings(
    name := "hello",
    version := "1.0",
    scalaVersion := "2.11.4",
    scalacOptions ++= Seq("-unchecked", "-feature", "-language:postfixOps")
  )

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"

