ThisBuild / scalaVersion := "2.13.12"
libraryDependencies += "com.google.ortools" % "ortools-java" % "9.9.3963"

lazy val root = (project in file("."))
  .settings(
    name := "advent-of-code-scala",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.18" % Test
    )

  )
