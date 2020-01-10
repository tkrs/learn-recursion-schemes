name := "learn-recursion-schemes"
version := "0.1"
scalaVersion := "2.12.10"
addCompilerPlugin(("org.typelevel" %% "kind-projector" % "0.11.0").cross(CrossVersion.full))
libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-free" % "2.0.0"
)
ThisBuild / scalafmtOnCompile := true
