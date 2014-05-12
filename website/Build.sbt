
resolvers += "Sonatype snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"


name := "website"

version := "1.0"

scalaVersion := "2.10.4"

EclipseKeys.withSource := true

EclipseKeys.createSrc := EclipseCreateSrc.Default + EclipseCreateSrc.Resource

unmanagedClasspath in Runtime <+= (baseDirectory) map { bd => Attributed.blank(bd / "src/main/resources") }

libraryDependencies += "org.eclipse.jetty" % "jetty-server" % "8.0.0.M0"

libraryDependencies += "org.eclipse.jetty" % "jetty-servlet" % "8.0.0.M0"

            