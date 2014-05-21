
resolvers += "Sonatype snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"


name := "cddjunit"

version := "1.0"

scalaVersion := "2.10.4"

EclipseKeys.withSource := true

EclipseKeys.createSrc := EclipseCreateSrc.Default + EclipseCreateSrc.Resource

unmanagedClasspath in Runtime <+= (baseDirectory) map { bd => Attributed.blank(bd / "src/main/resources") }

libraryDependencies += "com.novocode" % "junit-interface" % "0.9" % "test"

libraryDependencies += "junit" % "junit" % "4.8.2"

libraryDependencies += "org.joda" % "joda-convert" % "1.2"

libraryDependencies += "joda-time" % "joda-time" % "2.2"

            