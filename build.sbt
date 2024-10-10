scalaVersion := "2.13.13"
crossScalaVersions := Seq("2.13.12", "2.13.13")

name := "riscy"
version := "0.1.0"
organization := "io.riscy"

addCompilerPlugin("org.chipsalliance" %% "chisel-plugin" % "6.5.0" cross CrossVersion.full)

libraryDependencies += "org.chipsalliance" %% "chisel" % "6.5.0"
libraryDependencies += "edu.berkeley.cs" %% "chiseltest" % "6.0.0"
