lazy val root = (project in file(".")).
  settings(
    name := "BetaCompiler",
    version := "1.0",
    scalaVersion := "2.12.1",
    mainClass in assembly := Some("Main")
  )

libraryDependencies ++= Seq(
   "com.lihaoyi" %% "fastparse" % "0.4.2"
)

// META-INF discarding
assemblyMergeStrategy in assembly := {
  {
    case PathList("META-INF", xs @ _*) => MergeStrategy.discard
    case x => MergeStrategy.first
  }
}




