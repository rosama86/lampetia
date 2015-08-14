import dependencies._

name := "lampetia-postgresql"

libraryDependencies ++= Seq(
  ficus,
  jodaConvert,
  jodaTime,
  logback,
  akkaActor,
  playFunctional,
  playJson,
  postgresql,
  hikari)
