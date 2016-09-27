name := "first-class-derivatives"

version := "1.0"

scalaVersion := "2.11.7"

scalacOptions ++= Seq("-feature", "-deprecation")

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test"

resolvers += "Sonatype OSS Snapshots" at
  "https://oss.sonatype.org/content/repositories/releases"

libraryDependencies += "com.storm-enroute" %% "scalameter" % "0.7"

val scalaMeterFramework = new TestFramework("org.scalameter.ScalaMeterFramework")

testFrameworks in ThisBuild += scalaMeterFramework

testOptions in ThisBuild += Tests.Argument(scalaMeterFramework, "-silent")

parallelExecution in Test := true

libraryDependencies += "org.scala-lang.modules" % "scala-xml_2.11" % "1.0.5"

initialCommands in console := """import fcd._; import fcd.DerivativeParsers._"""

// For VM users on windows systems, please uncomment the following line:
// target := file("/home/vagrant/target/")
