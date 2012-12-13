
scalaVersion := "2.10.0-RC5"

resolvers += "Typesafe Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"
 
libraryDependencies ++= Seq(
   "org.scalaz" % "scalaz-core" % "7.0-SNAPSHOT" cross CrossVersion.full
)
 
scalacOptions += "-feature"
 
initialCommands in console := "import scalaz._, Scalaz._"

initialCommands in console in Test := "import scalaz._, Scalaz._, scalacheck.ScalazProperties._, scalacheck.ScalazArbitrary._,scalacheck.ScalaCheckBinding._"

