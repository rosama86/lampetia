import io.github.lampetia.Common.Dependencies._

name := "lampetia-security"

libraryDependencies ++= Seq(
  ficus,
  jodaConvert,
  jodaTime,
  logback,
  akkaActor,
  playFunctional,
  postgresql,
  h2,
  hikari,
  scalaTest)


