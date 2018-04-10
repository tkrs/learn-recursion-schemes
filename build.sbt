name := "learn-recursion-schemes"
version := "0.1"
scalaVersion := "2.12.5"
addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.6")
libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-free" % "1.1.0"
)
scalafmtVersion in ThisBuild := "1.3.0"
scalafmtOnCompile in Compile in ThisBuild := true
scalafmtTestOnCompile in Compile in ThisBuild := true