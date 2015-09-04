import dependencies._

name := "lampetia-code-gen"

libraryDependencies ++= Seq(logback,ficus,scalate, jodaTime, commonsio)

libraryDependencies += "com.danieltrinh" % "scalariform_2.11" % "0.1.5"