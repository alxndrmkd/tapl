import sbt._

object Dependencies {

  lazy val cats = {
    "org.typelevel" %% "cats-core" % Version.cats
  }

  lazy val free = {
    "org.typelevel" %% "cats-free" % Version.cats
  }

  lazy val mtl = {
    "org.typelevel" %% "cats-mtl-core" % Version.mtl
  }

  lazy val effect = {
    "org.typelevel" %% "cats-effect" % Version.effect
  }

  lazy val fs2 = {
    "co.fs2" %% "fs2-core" % Version.fs2
  }

  lazy val parsec = {
    "org.scala-lang.modules" %% "scala-parser-combinators" % Version.parsec
  }

  lazy val fastparse = {
    "com.lihaoyi" %% "fastparse" % "1.0.0"
  }

}
