import sbt._

object Libraries {
  val slf4jVersion = "2.0.12"
  val zioVersion = "2.1.5"
  val zioJsonVersion = "0.7.1"
  val zioNioVersion = "2.0.2"
  val http4sVersion = "0.23.27"
  val zioConfigVersion = "4.0.2"
  val zioInteropCatsVersion = "23.1.0.2"
  val tapirVersion = "1.10.10"
  val scalaTestVersion = "3.2.19"
  val enumeratumVersion = "1.7.3"
  val enumeratumDoobieVersion = "1.7.5"
  val doobieVersion = "1.0.0-RC5"
  val logbackVersion = "1.5.6"
  val flywayVersion = "10.15.0"
  val catsVersion = "2.12.0"
  val sttpVersion = "3.9.7"
  val circeVersion = "0.14.9"

  val typeDeps = Seq(
    "com.beachape" %% "enumeratum" % enumeratumVersion,
    "io.circe" %% "circe-literal" % circeVersion,
    "io.circe" %% "circe-generic" % circeVersion
  )

  val loggingDeps = Seq(
    "ch.qos.logback" % "logback-classic" % logbackVersion,
    "org.slf4j" % "jul-to-slf4j" % slf4jVersion,
    "org.slf4j" % "log4j-over-slf4j" % slf4jVersion,
    "org.slf4j" % "jcl-over-slf4j" % slf4jVersion
  )

  val effectDeps = Seq(
    "dev.zio" %% "zio" % zioVersion,
    "dev.zio" %% "zio-json" % zioJsonVersion,
    "dev.zio" %% "zio-streams" % zioVersion,
    "dev.zio" %% "zio-nio" % zioNioVersion,
    "dev.zio" %% "zio-interop-cats" % zioInteropCatsVersion,
    "org.typelevel" %% "cats-core" % catsVersion
  )

  val configDeps = Seq(
    "dev.zio" %% "zio-config" % zioConfigVersion,
    "dev.zio" %% "zio-config-magnolia" % zioConfigVersion,
    "dev.zio" %% "zio-config-typesafe" % zioConfigVersion
  )

  val databaseDeps = Seq(
    "org.tpolecat" %% "doobie-core" % doobieVersion,
    "org.tpolecat" %% "doobie-hikari" % doobieVersion, // HikariCP transactor.
    "org.tpolecat" %% "doobie-postgres" % doobieVersion, // Postgres driver 42.3.1 + type mappings.
    "org.tpolecat" %% "doobie-postgres-circe" % doobieVersion,
    "org.tpolecat" %% "doobie-specs2" % doobieVersion % Test, // Specs2 support for typechecking statements.
    "org.tpolecat" %% "doobie-scalatest" % doobieVersion % Test, // ScalaTest support for typechecking statements.
    "com.beachape" %% "enumeratum-doobie" % enumeratumDoobieVersion,
    "org.flywaydb" % "flyway-core" % flywayVersion,
    "org.flywaydb" % "flyway-database-postgresql" % flywayVersion
  )

  val httpClientDeps = Seq(
    "com.softwaremill.sttp.client3" %% "core" % sttpVersion,
    "com.softwaremill.sttp.client3" %% "zio" % sttpVersion,
    "com.softwaremill.sttp.client3" %% "circe" % sttpVersion
  )

  val apiDeps = Seq(
    "org.http4s" %% "http4s-server" % http4sVersion,
    "org.http4s" %% "http4s-client" % http4sVersion,
    "org.http4s" %% "http4s-ember-server" % http4sVersion,
    "org.http4s" %% "http4s-dsl" % http4sVersion,
    "org.http4s" %% "http4s-circe" % http4sVersion,
    "com.softwaremill.sttp.tapir" %% "tapir-core" % tapirVersion,
    "com.softwaremill.sttp.tapir" %% "tapir-enumeratum" % tapirVersion,
    "com.softwaremill.sttp.tapir" %% "tapir-openapi-docs" % tapirVersion,
    "com.softwaremill.sttp.tapir" %% "tapir-swagger-ui-bundle" % tapirVersion,
    "com.softwaremill.sttp.tapir" %% "tapir-http4s-server-zio" % tapirVersion,
    "com.softwaremill.sttp.tapir" %% "tapir-json-zio" % tapirVersion,
    "com.softwaremill.sttp.tapir" %% "tapir-json-circe" % tapirVersion,
    "com.softwaremill.sttp.tapir" %% "tapir-sttp-stub-server" % tapirVersion % Test
  )

  val testDeps = Seq(
    "org.scalactic" %% "scalactic" % scalaTestVersion % Test,
    "org.scalatest" %% "scalatest" % scalaTestVersion % Test,
    "dev.zio" %% "zio-test" % zioVersion % Test,
    "dev.zio" %% "zio-test-sbt" % zioVersion % Test,
    "dev.zio" %% "zio-test-magnolia" % zioVersion % Test,
    "dev.zio" %% "zio-test-junit" % zioVersion % Test
  )

  val commonDeps = typeDeps ++ loggingDeps ++ effectDeps ++ configDeps ++ testDeps
}
