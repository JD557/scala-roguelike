import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

name := "Roguelike"

version := "1.0"

ThisBuild / scalaVersion := "3.1.2"

ThisBuild / scalafixDependencies += "com.github.liancheng" %% "organize-imports" % "0.6.0"
ThisBuild / scalafmtOnCompile := true
ThisBuild / semanticdbEnabled := true
ThisBuild / semanticdbVersion := scalafixSemanticdb.revision
ThisBuild / scalafixOnCompile := true

lazy val root =
  crossProject(JVMPlatform, JSPlatform, NativePlatform)
    .in(file("."))
    .settings(
      resolvers += "Sonatype OSS Snapshots" at "https://s01.oss.sonatype.org/content/repositories/snapshots",
      Seq(
        libraryDependencies ++= List(
          "eu.joaocosta" %%% "minart" % "0.4.1",
          "com.armanbilge" %%% "circe-core" % "0.14.2-87-a48d472-SNAPSHOT",
          "com.armanbilge" %%% "circe-generic" % "0.14.2-87-a48d472-SNAPSHOT",
          "com.armanbilge" %%% "circe-parser" % "0.14.2-87-a48d472-SNAPSHOT"
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
