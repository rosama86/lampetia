import io.github.lampetia.Common.Dependencies._

name := "lampetia-core"

libraryDependencies ++= Seq(
  ficus,
  jodaConvert,
  jodaTime,
  logback,
  akkaActor,
  playFunctional,
  postgresql,
  hikari)


