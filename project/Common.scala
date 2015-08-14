package lampetia

import sbt._

object Common {

  val organization = "io.lampetia"
  val version = "0.1-SNAPSHOT"

  object Versions {
    val scalaVersion = "2.11.7"
    val jodaTimeVersion = "2.8"
    val jodaConvertVersion = "1.7"
    val ficusVersion = "1.1.1"
    val shapelessVersion = "2.2.0"
    val akkaVersion = "2.3.12"
    val sprayVersion = "1.3.3"
    val playVersion = "2.4.0"
    val logbackVersion = "1.1.2"
    val postgresqlJdbcVersion = "9.4-1201-jdbc41"
    val mysqlJdbcVersion = "5.1.36"
    val h2Version = "1.4.187"
    val hikariVersion = "2.3.7"
    val scalaTestVersion = "2.2.4"
  }

  object Resolvers {
    val typesafe = "Typesafe Repo" at "http://repo.typesafe.com/typesafe/releases/"
    val sonatype = Resolver.sonatypeRepo("releases")
    val all = Seq(typesafe,sonatype)
  }

  object Settings {
    //val scalacOptions = Seq("-unchecked", "-deprecation", "-feature", "-Xlint")
    val scalacOptions = Seq("-unchecked", "-deprecation", "-feature")
  }

  object Dependencies {
    import Versions._

    val ficus = "net.ceedubs" %% "ficus" % ficusVersion
    val jodaTime = "joda-time" % "joda-time" % jodaTimeVersion
    val jodaConvert = "org.joda" % "joda-convert" % jodaConvertVersion
    val shapeless = "com.chuusai" %% "shapeless" % shapelessVersion
    val logback = "ch.qos.logback" % "logback-classic" % logbackVersion
    val akkaActor = "com.typesafe.akka" %% "akka-actor" % akkaVersion
    val sprayCan = "io.spray" %% "spray-can" % sprayVersion
    val sprayRoute = "io.spray" %% "spray-routing" % sprayVersion
    val playFunctional = "com.typesafe.play" %% "play-functional" % playVersion
    val playJson = "com.typesafe.play" %% "play-json" % playVersion
    val postgresql = "org.postgresql" % "postgresql" % postgresqlJdbcVersion
    val mysql = "mysql" % "mysql-connector-java" % mysqlJdbcVersion
    val h2 = "com.h2database" % "h2" % h2Version
    val hikari = "com.zaxxer" % "HikariCP" % hikariVersion
    val scalaTest = "org.scalatest" % "scalatest_2.11" % scalaTestVersion % "test"

  }

}
