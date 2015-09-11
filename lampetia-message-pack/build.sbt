import dependencies._

name := "lampetia-message-pack"

libraryDependencies ++= Seq(
  jodaConvert,
  jodaTime,
  logback,
  messagePack,
  scalaTest)