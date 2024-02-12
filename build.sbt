ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.12"

lazy val root = (project in file("."))
  .settings(
    name := "hackrf_receive"
  )

libraryDependencies += "org.apache.commons" % "commons-collections4" % "4.4"
libraryDependencies += "org.apache.commons" % "commons-math3" % "3.6.1"
libraryDependencies += "org.apache.commons" % "commons-lang3" % "3.6"

// https://mvnrepository.com/artifact/org.scalafx/scalafx
libraryDependencies += "org.scalafx" %% "scalafx" % "20.0.0-R31"

// https://mvnrepository.com/artifact/javax.xml.bind/jaxb-api
libraryDependencies += "javax.xml.bind" % "jaxb-api" % "2.4.0-b180830.0359"

libraryDependencies += "net.java.dev.jna" % "jna" % "5.14.0"
