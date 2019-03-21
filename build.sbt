version := "0.0.1-SNAPSHOT"

organization := "devinsideyou"

scalaVersion := "2.12.8"

triggeredMessage := Watched.clearWhenTriggered

autoStartServer := false

scalacOptions ++= Seq(
  "-feature",
  "-deprecation",
  "-language:implicitConversions",
  "-language:higherKinds",
  "-Ypartial-unification"
)

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.9")
