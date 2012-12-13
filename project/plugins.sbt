//import sbt._

//import Defaults._

//requires local publishing of sbt-idea ( make sure it uses correct sbt version )

//resolvers += Resolver.file("local-repo", file("~/.sbt")) transactional()

//addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "1.3.0-SNAPSHOT")

//resolvers += "Sonatype snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"

//http://stackoverflow.com/questions/11768730/how-to-inform-sbt-to-consume-specific-scala-version-for-plugins
//libraryDependencies += sbtPluginExtra(
//    m = "com.github.mpeltonen" % "sbt-idea" % "1.3.0-SNAPSHOT", // Plugin module name and version
//    sbtV = "0.12",    // SBT version
//    scalaV = "2.9.2"    // Scala version compiled the plugin
//)

