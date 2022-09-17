import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

name := "Roguelike"

version := "1.1"

ThisBuild / scalaVersion := "3.2.0"

ThisBuild / scalafixDependencies += "com.github.liancheng" %% "organize-imports" % "0.6.0"
ThisBuild / scalafmtOnCompile := true
ThisBuild / semanticdbEnabled := true
ThisBuild / semanticdbVersion := scalafixSemanticdb.revision
ThisBuild / scalafixOnCompile := true

ThisBuild / scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-language:higherKinds",
  "-unchecked"
)

lazy val root =
  crossProject(JVMPlatform, JSPlatform, NativePlatform)
    .in(file("."))
    .settings(
      resolvers += "Sonatype OSS Snapshots" at "https://s01.oss.sonatype.org/content/repositories/snapshots",
      Seq(
        libraryDependencies ++= List(
          "eu.joaocosta" %%% "minart" % "0.4.3",
          "io.circe" %%% "circe-core" % "0.14.3",
          "io.circe" %%% "circe-generic" % "0.14.3",
          "io.circe" %%% "circe-parser" % "0.14.3"
        )
      )
    )
    .jsSettings(
      Seq(
        scalaJSUseMainModuleInitializer := true
      )
    )
    .nativeSettings(
      Seq(
        nativeLinkStubs := true,
        nativeMode      := "release",
        nativeLTO       := "thin",
        nativeGC        := "immix",
        nativeConfig ~= {
          _.withEmbedResources(true)
        }
      )
    )
    .settings(name := "Roguelike Root")
