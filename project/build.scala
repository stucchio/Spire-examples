import sbt._
import Defaults._
import Keys._

object ApplicationBuild extends Build {

  lazy val commonSettings = Defaults.defaultSettings ++ Seq(
    organization := "com.chrisstucchio",
    credentials += Credentials(Path.userHome / ".ivy2" / ".credentials"),
    scalaVersion := "2.10.0",
    version := "0.16rc5",
    resolvers ++= myResolvers,
    name := "spire examples",
    //fork := true,
    libraryDependencies ++= Seq("org.spire-math" %% "spire" % "0.6.0", "org.scalacheck" %% "scalacheck" % "1.10.0" % "test",
      "org.scalaz" %% "scalaz-core" % "7.0.1"
    )
  )

  val myResolvers = Seq("Sonatatype Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
			"Sonatatype Releases" at "http://oss.sonatype.org/content/repositories/releases",
			"Typesafe Releases" at "http://repo.typesafe.com/typesafe/releases",
			"Typesafe Snapshots" at "http://repo.typesafe.com/typesafe/snapshots",
			"Coda Hale" at "http://repo.codahale.com"
		      )

  lazy val spireExamples = Project("spire_examples", file("."), settings = commonSettings)
}
