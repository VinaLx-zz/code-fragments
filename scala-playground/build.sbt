lazy val root = (project in file("."))
  .settings(
    inThisBuild(List(
      scalaVersion := "2.12.1"
    )),
    name := "playground"
  )
