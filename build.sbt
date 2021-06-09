import sbtrelease._
import com.typesafe.tools.mima.core._
import sbtcrossproject.CrossPlugin.autoImport.crossProject
import sbtcrossproject.CrossType
import ReleaseTransformations._

val Scala211 = "2.11.12"
val NativeCond = s"matrix.scala == '$Scala211'"

ThisBuild / crossScalaVersions := Seq(Scala211, "2.12.13", "2.13.5")
ThisBuild / scalaVersion := Scala211

ThisBuild / githubWorkflowPublishTargetBranches := Seq()

ThisBuild / githubWorkflowBuildPreamble +=
  WorkflowStep.Run(
    List("sudo apt install clang libunwind-dev libgc-dev libre2-dev"),
    name = Some("Setup scala native dependencies"),
    cond = Some(NativeCond))

ThisBuild / githubWorkflowBuild := Seq(
  WorkflowStep.Sbt(
    List("test", "test:doc", "mimaReportBinaryIssues"),
    name = Some("Run main build")),

  WorkflowStep.Sbt(
    List("coreNative/test", "examplesNative/test"),
    name = Some("Run native build"),
    cond = Some(NativeCond)))

val scalatestVersion = "3.2.9"

lazy val nativeCommonSettings = Def.settings(
  scalaVersion := Scala211,
  crossScalaVersions := Seq(Scala211),
  nativeLinkStubs := true
)

lazy val commonSettings = Seq(
  organization := "org.typelevel",
  scalacOptions ++= Seq(
    "-deprecation",
    "-feature",
    "-language:higherKinds",
    "-language:implicitConversions"
  ),
  scalacOptions ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, v)) if v >= 13 =>
        Seq("-Ymacro-annotations")
      case _ =>
        Nil
    }
  },
  Compile / doc / scalacOptions ~= { _ filterNot { o => o == "-Ywarn-unused-import" || o == "-Xfatal-warnings" } },
  Compile / console / scalacOptions ~= { _ filterNot { o => o == "-Ywarn-unused-import" || o == "-Xfatal-warnings" } },
  Test / console / scalacOptions := (Compile / console / scalacOptions).value,
  resolvers ++= Seq(
    Resolver.sonatypeRepo("releases"),
    Resolver.sonatypeRepo("snapshots")
  ),
  libraryDependencies ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, v)) if v <= 12 =>
        Seq(
          compilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full)
        )
      case _ =>
        // if scala 2.13.0-M4 or later, macro annotations merged into scala-reflect
        // https://github.com/scala/scala/pull/6606
        Nil
    }
  },
  licenses += ("Three-clause BSD-style", url("https://github.com/mpilquist/simulacrum/blob/master/LICENSE")),
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (version.value.trim.endsWith("SNAPSHOT"))
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases" at nexus + "service/local/staging/deploy/maven2")
  },
  publishMavenStyle := true,
  Test / publishArtifact := false,
  pomIncludeRepository := { x => false },
  pomExtra := (
    <url>http://github.com/mpilquist/simulacrum</url>
    <scm>
      <url>git@github.com:mpilquist/simulacrum.git</url>
      <connection>scm:git:git@github.com:mpilquist/simulacrum.git</connection>
    </scm>
    <developers>
      <developer>
        <id>mpilquist</id>
        <name>Michael Pilquist</name>
        <url>http://github.com/mpilquist</url>
      </developer>
    </developers>
  ),
  pomPostProcess := { node =>
    import scala.xml._
    import scala.xml.transform._
    def stripIf(f: Node => Boolean) = new RewriteRule {
      override def transform(n: Node) =
        if (f(n)) NodeSeq.Empty else n
    }
    val stripTestScope = stripIf { n => n.label == "dependency" && (n \ "scope").text == "test" }
    new RuleTransformer(stripTestScope).transform(node)(0)
  },
  releaseCrossBuild := true,
  releaseProcess := Seq[ReleaseStep](
    checkSnapshotDependencies,
    inquireVersions,
    runClean,
    runTest,
    releaseStepCommandAndRemaining("test:doc"),
    setReleaseVersion,
    commitReleaseVersion,
    tagRelease,
    publishArtifacts,
    releaseStepCommandAndRemaining(s";++${Scala211}!;coreNative/publish"),
    setNextVersion,
    commitNextVersion,
    pushChanges
  ),
  Test / compile / wartremoverErrors ++= Seq(
    Wart.ExplicitImplicitTypes,
    Wart.ImplicitConversion)
)

lazy val root = project.in(file("."))
  .settings(commonSettings: _*)
  .settings(crossScalaVersions := Seq(Scala211))
  .settings(noPublishSettings: _*)
  .aggregate(coreJVM, examplesJVM, coreJS, examplesJS)

ThisBuild / mimaFailOnNoPrevious := false

def previousVersion(scalaVersion: String, currentVersion: String): Option[String] = {
  if (scalaVersion == "2.13.0")
    None
  else {
    val Version = """(\d+)\.(\d+)\.(\d+).*""".r
    val Version(x, y, z) = currentVersion
    if (z == "0") None
    else Some(s"$x.$y.${z.toInt - 1}")
  }
}

lazy val core = crossProject(JSPlatform, JVMPlatform, NativePlatform).crossType(CrossType.Pure)
  .settings(commonSettings: _*)
  .settings(
    moduleName := "simulacrum",
    Test / scalacOptions += "-Yno-imports"
  )
  .settings(
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided",
      "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided",
      "org.scalatest" %%% "scalatest" % scalatestVersion % "test"
    )
  )
  .nativeSettings(
    nativeCommonSettings
  )
  .platformsSettings(JSPlatform, NativePlatform)(
    Test / unmanagedSources / excludeFilter := "jvm.scala"
  )
  .jvmSettings(
    mimaPreviousArtifacts := previousVersion(scalaVersion.value, version.value).map { pv =>
      organization.value % ("simulacrum" + "_" + scalaBinaryVersion.value) % pv
    }.toSet
  )

lazy val coreJVM = core.jvm
lazy val coreJS = core.js
lazy val coreNative = core.native

lazy val examples = crossProject(JSPlatform, JVMPlatform, NativePlatform).crossType(CrossType.Pure)
  .dependsOn(core % "provided")
  .settings(commonSettings: _*)
  .settings(moduleName := "simulacrum-examples")
  .settings(
    libraryDependencies += "org.scalatest" %%% "scalatest" % scalatestVersion % "test"
  )
  .settings(noPublishSettings: _*)
  .platformsSettings(JVMPlatform, JSPlatform)(
    libraryDependencies += "com.chuusai" %%% "shapeless" % "2.3.7" % "test"
  )
  .nativeSettings(
    nativeCommonSettings
  )

lazy val examplesJVM = examples.jvm
lazy val examplesJS = examples.js
lazy val examplesNative = examples.native

lazy val noPublishSettings = Seq(
  publish := {},
  publishLocal := {},
  publishArtifact := false
)
