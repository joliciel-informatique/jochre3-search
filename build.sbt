import BuildHelper._
import Libraries._
import com.typesafe.sbt.packager.docker.Cmd
import sbt.Keys.libraryDependencies

ThisBuild / scalaVersion := "2.13.14"
ThisBuild / organization := "com.joli-ciel"
ThisBuild / homepage := Some(url("https://www.joli-ciel.com/"))
ThisBuild / licenses := List("AGPL-v3" -> url("https://www.gnu.org/licenses/agpl-3.0.en.html"))

val cloakroomVersion = "0.5.15"
val luceneVersion = "9.11.1"
val jochre3OcrVersion = "0.3.15"
val catsRetryVersion = "3.1.3"
val jakartaMailVersion = "2.0.1"

lazy val jochre3SearchVersion = sys.env
  .get("JOCHRE3_SEARCH_VERSION")
  .getOrElse {
    ConsoleLogger().warn("JOCHRE3_SEARCH_VERSION env var not found")
    "0.0.1-SNAPSHOT"
  }

val projectSettings = commonSettings ++ Seq(
  version := jochre3SearchVersion
)

testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")

lazy val root =
  Project(id = "jochre3-search", base = file("."))
    .settings(noDoc: _*)
    .settings(noPublishSettings: _*)
    .aggregate(core, api, yiddish)

lazy val core = project
  .in(file("modules/core"))
  .settings(projectSettings: _*)
  .settings(
    libraryDependencies ++= commonDeps ++ databaseDeps ++ httpClientDeps ++ Seq(
      "org.apache.lucene" % "lucene-core" % luceneVersion,
      "org.apache.lucene" % "lucene-sandbox" % luceneVersion,
      "org.apache.lucene" % "lucene-join" % luceneVersion,
      "org.apache.lucene" % "lucene-facet" % luceneVersion,
      "org.apache.lucene" % "lucene-queryparser" % luceneVersion,
      "org.apache.lucene" % "lucene-analysis-common" % luceneVersion,
      "org.apache.lucene" % "lucene-highlighter" % luceneVersion,
      "org.apache.lucene" % "lucene-backward-codecs" % luceneVersion,
      "org.apache.lucene" % "lucene-test-framework" % luceneVersion % "test",
      "com.joliciel" %% "jochre3-ocr-core" % jochre3OcrVersion,
      "com.sun.mail" % "jakarta.mail" % jakartaMailVersion
    ),
    Compile / packageDoc / mappings := Seq(),
    fork := true,
    publish / skip := true
  )

lazy val yiddish = project
  .in(file("modules/yiddish"))
  .settings(projectSettings: _*)
  .settings(
    libraryDependencies ++= commonDeps ++ httpClientDeps ++ Seq(
      "com.joliciel" %% "jochre3-ocr-yiddish" % jochre3OcrVersion
    ),
    Compile / packageDoc / mappings := Seq(),
    fork := true,
    publish / skip := true
  )
  .dependsOn(core % "compile->compile;test->test")

lazy val api = project
  .in(file("modules/api"))
  .enablePlugins(JavaServerAppPackaging, DockerPlugin)
  .settings(projectSettings: _*)
  .settings(
    libraryDependencies ++= commonDeps ++ databaseDeps ++ apiDeps ++ Seq(
      "com.safety-data" %% "cloakroom-scala" % cloakroomVersion,
      "com.safety-data" %% "cloakroom-test-util-scala" % cloakroomVersion % Test,
      "com.safety-data" % "cloakroom" % cloakroomVersion,
      "com.github.cb372" %% "cats-retry" % catsRetryVersion
    ),
    Docker / packageName := "jochre/jochre3-search",
    Docker / maintainer := "Joliciel Informatique SARL",
    Docker / daemonUserUid := Some("1001"),
    dockerBaseImage := "openjdk:17.0.2-bullseye",
    Docker / dockerRepository := sys.env.get("JOCHRE3_DOCKER_REGISTRY"),
    Docker / version := version.value,
    dockerExposedPorts := Seq(3232),
    dockerExposedVolumes := Seq("/opt/docker/index"),
    // Add docker commands before changing user
    Docker / dockerCommands := dockerCommands.value.flatMap {
      case Cmd("USER", args @ _*) if args.contains("1001:0") =>
        Seq(
          // Add unattended security upgrades to docker image
          Cmd("RUN", "apt update && apt install -y unattended-upgrades"),
          Cmd("RUN", "unattended-upgrade -d --dry-run"),
          Cmd("RUN", "unattended-upgrade -d"),
          Cmd("USER", args: _*)
        )
      case cmd => Seq(cmd)
    },
    // do not package scaladoc
    Compile / packageDoc / mappings := Seq(),
    Compile / mainClass := Some("com.joliciel.jochre.search.api.MainApp"),
    fork := true
  )
  .dependsOn(core % "compile->compile;test->test", yiddish % "compile->compile;test->test")
