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

  lazy val `lampetia-message-pack` =
    Project("lampetia-message-pack", file("lampetia-message-pack"))
      .dependsOn(`lampetia-core`)

  lazy val `lampetia-postgresql` =
    Project("lampetia-postgresql", file("lampetia-postgresql"))
      .dependsOn(`lampetia-sql`)

  lazy val `lampetia-mysql` =
    Project("lampetia-mysql", file("lampetia-mysql"))
      .dependsOn(`lampetia-sql`)

  lazy val `lampetia-example` =
    Project("lampetia-example", file("lampetia-example"))
      .dependsOn(`lampetia-postgresql`, `lampetia-spray`)


  lazy val `lampetia` =
    Project("lampetia", file("."))
      .aggregate(
        `lampetia-model`,
        `lampetia-message-pack`,
        `lampetia-core`,
        `lampetia-sql`,
        `lampetia-spray`,
        `lampetia-postgresql`,
        `lampetia-mysql`,
        `lampetia-example`)


}
