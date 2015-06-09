package io.github.lampetia

import sbt.Keys._
import sbt._
import com.typesafe.sbt.SbtGit._
import scala.Console._


object Build extends sbt.Build {

  val buildPrompt: State => String = { state =>
        val prompt = GitCommand.prompt(state).trim
        val regex = """(.*)\((.*)\)>""".r
        prompt match {
          case regex(pname, "master") => 
            s"""$BLUE$pname $RED[master]$RESET $$ """
          case regex(pname, bname) => 
            s"""$BLUE$pname $YELLOW[$bname]$RESET $$ """
          case pname => 
            s"""$BLUE$pname$RESET $$ """
        }
        
      }
  
  override lazy val settings = super.settings ++
    Seq(
      organization := Common.organization,
      version := Common.version,
      scalaVersion := Common.Versions.scalaVersion,
      scalacOptions ++= Common.Settings.scalacOptions,
      scalacOptions in Test ++= Seq("-Yrangepos"), // review this
      resolvers ++= Common.Resolvers.all,
      shellPrompt := buildPrompt,
      fork in Test := true)


  lazy val `lampetia-model` =
    Project("lampetia-model", file("lampetia-model"))

  lazy val `lampetia-core` =
    Project("lampetia-core", file("lampetia-core"))
      .dependsOn(`lampetia-model`)

  lazy val `lampetia-security-model` =
    Project("lampetia-security-model", file("lampetia-security-model"))
      .dependsOn(`lampetia-model`, `lampetia-core`)

  lazy val `lampetia-security` =
    Project("lampetia-security", file("lampetia-security"))
      .dependsOn(`lampetia-model`, `lampetia-core`, `lampetia-security-model`)

  lazy val `lampetia` =
    Project("lampetia", file("."))
      .aggregate(
        `lampetia-model`,
        `lampetia-core`,
        `lampetia-security-model`,
        `lampetia-security`)


}


