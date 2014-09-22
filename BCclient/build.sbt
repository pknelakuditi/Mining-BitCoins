name := "Hello Test #1"
 
version := "1.0"
 
scalaVersion := "2.11.2"
 
resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor"  % "2.3.6",
  "com.typesafe.akka" %% "akka-remote" % "2.3.6"
)

addSbtPlugin("com.typesafe.sbteclipse" % "sbteclipse-plugin" % "2.5.0")