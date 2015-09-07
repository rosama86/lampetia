import dependencies._

name := "lampetia-security-api"

val bootable = "lampetia.security.spray.service.SecurityHttpService"

mainClass in Compile := Some(bootable)

lazy val service = taskKey[Unit](s"Start HTTP Service $bootable")

fullRunTask(service, Runtime, bootable)

fork in service := true

libraryDependencies ++= Seq(
  ficus,
  jodaConvert,
  jodaTime,
  logback,
  akkaActor,
  sprayCan,
  sprayRoute,
  jjwt,
  scalaTest,
  sprayTest)