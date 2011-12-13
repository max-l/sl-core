
import sbt._
import Keys._


object Buildz extends Build {
				   
  def buildSettings = Defaults.defaultSettings ++ Seq(
    organization := "net.strong_links",
    version := "0.2",
    logLevel in Global := Level.Warn,
    scalaVersion := "2.9.1"
  )
    
  lazy val buildProject = Project(
    id = "project",
    base = file("project"),    
    settings = buildSettings ++ Seq(    
      sbtPlugin := true
    )
  )  
  
  lazy val core = Project(
    id = "core",
    base = file("core"),
    settings = buildSettings 
  )

  lazy val root = Project(
    id = "root",
    base = file("."),
    settings = buildSettings 
  ) aggregate(core)    
}
