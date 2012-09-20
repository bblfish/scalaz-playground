scalaVersion := "2.10.0-M7"

resolvers += "Typesafe Snapshots" at "http://repo.typesafe.com/typesafe/snapshots/"
 
libraryDependencies ++= Seq(
  "org.scalaz" % "scalaz-core" % "7.0.0-M3" cross CrossVersion.full
)
 
scalacOptions += "-feature"
 
initialCommands in console := "import scalaz._, Scalaz._"

initialCommands in console in Test := "import scalaz._, Scalaz._, scalacheck.ScalazProperties._, scalacheck.ScalazArbitrary._,scalacheck.ScalaCheckBinding._"


