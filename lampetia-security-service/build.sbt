import dependencies._

name := "lampetia-security-service"

//enablePlugins(JavaServerAppPackaging)

//mainClass in Compile := Some("lampetia.security.service.SecurityService")

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
