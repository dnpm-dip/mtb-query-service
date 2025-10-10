// build.sbt adapted from https://github.com/pbassiner/sbt-multi-project-example/blob/master/build.sbt

import scala.util.Properties.envOrElse


name := "mtb-query-service"
ThisBuild / organization := "de.dnpm.dip"
ThisBuild / scalaVersion := "2.13.16"
ThisBuild / version      := envOrElse("VERSION","1.1.0")

val ownerRepo  = envOrElse("REPOSITORY","dnpm-dip/mtb-query-service").split("/")
ThisBuild / githubOwner      := ownerRepo(0)
ThisBuild / githubRepository := ownerRepo(1)


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
    val scalatest      = "org.scalatest"  %% "scalatest"              % "3.2.17" % Test
    val mtb_model      = "de.dnpm.dip"    %% "mtb-dto-model"          % "1.1.1"
    val mtb_generators = "de.dnpm.dip"    %% "mtb-dto-generators"     % "1.1.1"
    val service_base   = "de.dnpm.dip"    %% "service-base"           % "1.1.0"
    val connector_base = "de.dnpm.dip"    %% "connector-base"         % "1.1.0"
    val icd10gm        = "de.dnpm.dip"    %% "icd10gm-impl"           % "1.1.1" % Test
    val icdo3          = "de.dnpm.dip"    %% "icdo3-impl"             % "1.1.1" % Test
    val icd_catalogs   = "de.dnpm.dip"    %% "icd-claml-packaged"     % "1.1.1" % Test
    val atc_impl       = "de.dnpm.dip"    %% "atc-impl"               % "1.1.0" % Test
    val atc_catalogs   = "de.dnpm.dip"    %% "atc-catalogs-packaged"  % "1.1.0" % Test
    val hgnc_geneset   = "de.dnpm.dip"    %% "hgnc-gene-set-impl"     % "1.1.0" % Test
  }


//-----------------------------------------------------------------------------
// SETTINGS
//-----------------------------------------------------------------------------

lazy val settings = commonSettings


// Compiler options from: https://alexn.org/blog/2020/05/26/scala-fatal-warnings/
lazy val compilerOptions = Seq(
  // Feature options
  "-encoding", "utf-8",
  "-explaintypes",
  "-feature",
  "-language:existentials",
  "-language:experimental.macros",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-language:postfixOps",
  "-Ymacro-annotations",

  // Warnings as errors!
  "-Xfatal-warnings",

  // Linting options
  "-unchecked",
  "-Xcheckinit",
  "-Xlint:adapted-args",
  "-Xlint:constant",
  "-Xlint:delayedinit-select",
  "-Xlint:deprecation",
  "-Xlint:doc-detached",
  "-Xlint:inaccessible",
  "-Xlint:infer-any",
  "-Xlint:missing-interpolator",
  "-Xlint:nullary-unit",
  "-Xlint:option-implicit",
  "-Xlint:package-object-classes",
  "-Xlint:poly-implicit-overload",
  "-Xlint:private-shadow",
  "-Xlint:stars-align",
  "-Xlint:type-parameter-shadow",
  "-Wdead-code",
  "-Wextra-implicit",
  "-Wnumeric-widen",
  "-Wunused:imports",
  "-Wunused:locals",
  "-Wunused:patvars",
  "-Wunused:privates",
  "-Wunused:implicits",
  "-Wvalue-discard",
)


lazy val commonSettings = Seq(
  scalacOptions ++= compilerOptions,
  resolvers ++= Seq(
    "Local Maven Repository" at "file://" + Path.userHome.absolutePath + "/.m2/repository",
    Resolver.githubPackages("dnpm-dip"),
    Resolver.githubPackages("KohlbacherLab"),
    Resolver.sonatypeCentralSnapshots
  )
)

