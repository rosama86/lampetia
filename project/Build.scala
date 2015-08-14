package lampetia

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
            s"""$BLUE$pname $GREEN[master]$RESET $$ """
          case regex(pname, bname) =>
            s"""$BLUE$pname $YELLOW[$bname]$RESET $$ """
          case pname =>
            s"""$BLUE$pname$RESET $$ """
        }

      }

  val dependencies = Common.Dependencies

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

  lazy val `lampetia-spray` =
    Project("lampetia-spray", file("lampetia-spray"))
      .dependsOn(`lampetia-core`)

  lazy val `lampetia-sql` =
    Project("lampetia-sql", file("lampetia-sql"))
      .dependsOn(`lampetia-core`)

  lazy val `lampetia-postgresql` =
    Project("lampetia-postgresql", file("lampetia-postgresql"))
      .dependsOn(`lampetia-sql`)

  lazy val `lampetia-mysql` =
    Project("lampetia-mysql", file("lampetia-mysql"))
      .dependsOn(`lampetia-sql`)

  lazy val `lampetia-security-model` =
    Project("lampetia-security-model", file("lampetia-security-model"))
      .dependsOn(`lampetia-model`, `lampetia-core`, `lampetia-postgresql`)

  lazy val `lampetia-security-service` =
    Project("lampetia-security-service", file("lampetia-security-service"))
      .dependsOn(`lampetia-model`, `lampetia-core`, `lampetia-postgresql`, `lampetia-security-model`)

  lazy val `lampetia-security-spray` =
    Project("lampetia-security-spray", file("lampetia-security-spray"))
      .dependsOn(`lampetia-spray`, `lampetia-security-service`)

  lazy val `lampetia` =
    Project("lampetia", file("."))
      .aggregate(
        `lampetia-model`,
        `lampetia-core`,
        `lampetia-sql`,
        `lampetia-spray`,
        `lampetia-postgresql`,
        `lampetia-mysql`,
        `lampetia-security-model`,
        `lampetia-security-service`,
        `lampetia-security-spray`)


}
