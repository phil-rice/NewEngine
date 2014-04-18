
resolvers += "Sonatype snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"


name := "jUnit"

version := "1.0"

scalaVersion := "2.10.4"

EclipseKeys.withSource := true

EclipseKeys.createSrc := EclipseCreateSrc.Default + EclipseCreateSrc.Resource

unmanagedClasspath in Runtime <+= (baseDirectory) map { bd => Attributed.blank(bd / "src/main/resources") }

libraryDependencies += "com.novocode" % "junit-interface" % "0.9" % "test"

libraryDependencies += "org.scala-lang" % "scala-compiler" % "2.10.4"
            