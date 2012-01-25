
import sbt._
import Keys._


object Buildz extends Build {


  def buildSettings = Defaults.defaultSettings ++ Seq(
    organization := "com.strong-links",
    version := "0.2-SNAPSHOT",
    scalaVersion := "2.9.1",
    publishMavenStyle := true,
    publishArtifact in packageDoc := false,
    pomIncludeRepository := { x => false },
    publishTo <<= version { (v: String) =>
      val nexus = "https://oss.sonatype.org/"
      if (v.trim.endsWith("SNAPSHOT")) Some("snapshots" at nexus + "content/repositories/snapshots") 
      else                             Some("releases"  at nexus + "service/local/staging/deploy/maven2")
    },
    credentials += Credentials(Path.userHome / ".ivy2" / "credentials.oss"),
    pomExtra := (
      <url>http://github.com/strong-links/core</url>
      <licenses>
        <license>
          <name>Apache 2</name>
          <url>http://www.apache.org/licenses/LICENSE-2.0.txt</url>
          <distribution>repo</distribution>
        </license>
      </licenses>
      <scm>
        <url>git@github.com:strong-links/core.git</url>
        <connection>scm:git:git@github.com:strong-links/core.git</connection>
      </scm>
      <developers>
        <developer>
          <id>jrlemieux</id>
          <name>Jacques Lemieux</name>
        </developer>
        <developer>
          <id>max-l</id>
          <name>Maxime Lévesque</name>
        </developer>
      </developers>
    )
  )


  lazy val core = Project(
    id = "core",
    base = file("core"),
    settings = buildSettings ++Seq(
      libraryDependencies ++= Seq(
         "org.slf4j" % "slf4j-api" % "1.6.1"
      )
    )
  )

  lazy val root = Project(
    id = "root",
    base = file("."),
    settings = buildSettings ++ Seq(
      publish := (),
      publishLocal := ()
    )
  ) aggregate(core)    
}
