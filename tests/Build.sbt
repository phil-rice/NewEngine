
resolvers += "Sonatype snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"


name := "tests"

version := "1.0"

scalaVersion := "2.10.4"

EclipseKeys.withSource := true

EclipseKeys.createSrc := EclipseCreateSrc.Default + EclipseCreateSrc.Resource

unmanagedClasspath in Runtime <+= (baseDirectory) map { bd => Attributed.blank(bd / "src/main/resources") }

libraryDependencies += "com.novocode" % "junit-interface" % "0.9" % "test"

libraryDependencies += "junit" % "junit" % "4.8.2"

libraryDependencies += "org.scalatest" % "scalatest_2.10" %  "2.0.M6" % "test->*" excludeAll( ExclusionRule(organization = "org.eclipse.jetty") )

libraryDependencies += "org.seleniumhq.selenium" % "selenium-java" % "2.28.0" % "test" excludeAll( ExclusionRule(organization = "org.eclipse.jetty") )

libraryDependencies += "org.seleniumhq.selenium" % "selenium-chrome-driver" % "2.35.0" % "test" excludeAll( ExclusionRule(organization = "org.eclipse.jetty") )

libraryDependencies += "org.seleniumhq.selenium" % "selenium-htmlunit-driver" % "2.35.0" % "test" excludeAll( ExclusionRule(organization = "org.eclipse.jetty") )

            