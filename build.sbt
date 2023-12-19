
/*
 build.sbt adapted from https://github.com/pbassiner/sbt-multi-project-example/blob/master/build.sbt
*/


name := "mtb-query-service"
ThisBuild / organization := "de.dnpm.dip"
ThisBuild / scalaVersion := "2.13.12"
ThisBuild / version      := "1.0-SNAPSHOT"


//-----------------------------------------------------------------------------
// PROJECTS
//-----------------------------------------------------------------------------

lazy val global = project
  .in(file("."))
  .settings(
    settings,
    publish / skip := true
  )
  .aggregate(
     api,
     impl
  )


lazy val api = project
  .settings(
    name := "mtb-query-service-api",
    settings,
    libraryDependencies ++= Seq(
      dependencies.scalatest,
      dependencies.mtb_model,
      dependencies.service_base
    )
  )


lazy val impl = project
  .settings(
    name := "mtb-query-service-impl",
    settings,
    libraryDependencies ++= Seq(
      dependencies.scalatest,
      dependencies.mtb_generators,
      dependencies.connector_base,
      dependencies.icd10gm,
      dependencies.icdo3,
      dependencies.icd_catalogs,
      dependencies.atc_impl,
      dependencies.atc_catalogs,
      dependencies.hgnc_geneset
    )
  )
  .dependsOn(
    api
  )



//-----------------------------------------------------------------------------
// DEPENDENCIES
//-----------------------------------------------------------------------------

lazy val dependencies =
  new {
    val scalatest      = "org.scalatest"  %% "scalatest"          % "3.2.17" % Test
    val mtb_model      = "de.dnpm.dip"    %% "mtb-dto-model"      % "1.0-SNAPSHOT"
    val mtb_generators = "de.dnpm.dip"    %% "mtb-dto-generators" % "1.0-SNAPSHOT"
    val service_base   = "de.dnpm.dip"    %% "service-base"       % "1.0-SNAPSHOT"
    val connector_base = "de.dnpm.dip"    %% "connector-base"     % "1.0-SNAPSHOT"
//    val atc            = "de.dnpm.dip"    %% "atc-impl"           % "1.0-SNAPSHOT" % Test
//    val hgnc           = "de.dnpm.dip"    %% "hgnc-gene-set-impl" % "1.0-SNAPSHOT" % Test
//    val hgnc           = "de.dnpm.dip"    %% "hgnc-gene-set-impl"i % "1.0-SNAPSHOT" % Test
    val icd10gm      = "de.dnpm.dip"    %% "icd10gm-impl"           % "1.0-SNAPSHOT" % Test
    val icdo3        = "de.dnpm.dip"    %% "icdo3-impl"             % "1.0-SNAPSHOT" % Test
    val icd_catalogs = "de.dnpm.dip"    %% "icd-claml-packaged"     % "1.0-SNAPSHOT" % Test
    val atc_impl     = "de.dnpm.dip"    %% "atc-impl"               % "1.0-SNAPSHOT" % Test
    val atc_catalogs = "de.dnpm.dip"    %% "atc-catalogs-packaged"  % "1.0-SNAPSHOT" % Test
    val hgnc_geneset = "de.dnpm.dip"    %% "hgnc-gene-set-impl"     % "1.0-SNAPSHOT" % Test
  }


//-----------------------------------------------------------------------------
// SETTINGS
//-----------------------------------------------------------------------------

lazy val settings = commonSettings


lazy val compilerOptions = Seq(
  "-encoding", "utf8",
  "-unchecked",
  "-feature",
  "-language:postfixOps",
  "-Xfatal-warnings",
  "-deprecation",
)

lazy val commonSettings = Seq(
  scalacOptions ++= compilerOptions,
  resolvers ++= Seq("Local Maven Repository" at "file://" + Path.userHome.absolutePath + "/.m2/repository") ++
    Resolver.sonatypeOssRepos("releases") ++
    Resolver.sonatypeOssRepos("snapshots")
)

