name := "first-class-derivatives"

version := "3.0.2"

scalaVersion := "3.7.4"

scalacOptions ++= Seq("-feature", "-deprecation")

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % "test"

resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/releases"

Test / parallelExecution := true

libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "2.4.0"

console / initialCommands := """import fcd._; import fcd.DerivativeParsers._"""

// For VM users on windows systems, please uncomment the following line:
// target := file("/home/vagrant/target/")
