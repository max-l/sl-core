
import sbt._
import Keys._

object Buildz extends Build {

  def buildSettings = Defaults.defaultSettings ++ Seq(
    organization := "com.strong-links",
    version := "0.2",
    scalaVersion := "2.9.1"
  )
  
  lazy val core = Project(
    id = "core",
    base = file("."),
    settings = buildSettings 
  )
}
