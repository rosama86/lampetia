import io.github.lampetia.Common.Dependencies._

name := "lampetia-core"

libraryDependencies ++= Seq(
  ficus,
  jodaConvert,
  jodaTime,
  logback,
  akkaActor,
  playFunctional,
  playJson,
  postgresql,
  h2,
  hikari)


