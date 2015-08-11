import dependencies._

name := "lampetia-mysql"

libraryDependencies ++= Seq(
  ficus,
  jodaConvert,
  jodaTime,
  logback,
  akkaActor,
  playFunctional,
  playJson,
  mysql,
  hikari)
