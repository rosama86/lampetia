import dependencies._

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
