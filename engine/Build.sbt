
resolvers += "Sonatype snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"

name := "engine"

version := "1.0"

scalaVersion := "2.10.4"

EclipseKeys.withSource := true

unmanagedClasspath in Runtime <+= (baseDirectory) map { bd => Attributed.blank(bd / "src/main/resources") }

libraryDependencies += "org.scala-lang" % "scala-compiler" % "2.10.4"

