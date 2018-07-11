lazy val root =
  project
    .in(file("."))
    .settings(name := "tapl")
    .settings(Common.settings)
    .aggregate(utl, tl)

lazy val utl =
  module("untyped")
    .settings(mainClass in Compile := Some("edu.dsfn.Main"))
    .settings(libraryDependencies ++= Common.dependencies)
    .settings(libraryDependencies += Dependencies.parsec)
    .settings(libraryDependencies += Dependencies.fastparse)

lazy val tl =
  module("typed")
    .settings(mainClass in Compile := Some("edu.dsfn.Main"))
    .settings(libraryDependencies ++= Common.dependencies)
    .settings(libraryDependencies += Dependencies.parsec)
    .settings(libraryDependencies += Dependencies.fastparse)

def module(modName: String) =
  Project(modName, file(s"modules/$modName"))
    .settings(name := modName)
    .settings(Common.settings)
