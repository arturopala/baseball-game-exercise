import Dependencies._

ThisBuild / scalaVersion     := "2.12.8"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.github.arturopala"
ThisBuild / organizationName := "arturopala"

lazy val root = (project in file("."))
  .settings(
    name := "baseball-game-exercise",
    libraryDependencies += scalaTest % Test
  )