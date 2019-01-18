import sbtrelease._
import com.typesafe.tools.mima.core._
import sbtcrossproject.{crossProject, CrossType}
import ReleaseTransformations._

val Scala211 = "2.11.12"

def ifAtLeast(scalaBinaryVersion: String, atLeastVersion: String)(options: String*): Seq[String] = {
  case class ScalaBinaryVersion(major: Int, minor: Int) extends Ordered[ScalaBinaryVersion] {
    def compare(that: ScalaBinaryVersion) = Ordering[(Int, Int)].compare((this.major, this.minor), (that.major, that.minor))
  }
  val Pattern = """(\d+)\.(\d+).*""".r
  def getScalaBinaryVersion(v: String) = v match { case Pattern(major, minor) => ScalaBinaryVersion(major.toInt, minor.toInt) }
  if (getScalaBinaryVersion(scalaBinaryVersion) >= getScalaBinaryVersion(atLeastVersion)) options
  else Seq.empty
}

lazy val scalatestSetting = Def.setting(
  if (scalaVersion.value == "2.13.0-M5") {
    Seq("org.scalatest" %%% "scalatest" % "3.0.6-SNAP4" % "test")
  } else {
    Seq("org.scalatest" %%% "scalatest" % "3.0.6-SNAP5" % "test")
  }
)

lazy val nativeCommonSettings = Def.settings(
  // https://github.com/scalatest/scalatest/issues/1112#issuecomment-366856502
  libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.0-SNAP10" % "test",
  scalaVersion := Scala211,
  crossScalaVersions := Seq(Scala211),
  nativeLinkStubs := true
)

lazy val commonSettings = Seq(
  organization := "com.github.mpilquist",
  scalacOptions ++= Seq(
    "-deprecation",
    "-feature",
    "-language:higherKinds",
    "-language:implicitConversions"
  ),
  scalacOptions in (Compile, doc) ~= { _ filterNot { o => o == "-Ywarn-unused-import" || o == "-Xfatal-warnings" } },
  scalacOptions in (Compile, console) ~= { _ filterNot { o => o == "-Ywarn-unused-import" || o == "-Xfatal-warnings" } },
  scalacOptions in (Test, console) := (scalacOptions in (Compile, console)).value,
  scalaVersion := Scala211,
  crossScalaVersions := Seq("2.10.7", Scala211, "2.12.7", "2.13.0-M5"),
  resolvers ++= Seq(
    Resolver.sonatypeRepo("releases"),
    Resolver.sonatypeRepo("snapshots")
  ),
  libraryDependencies ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, v)) if v <= 12 =>
        Seq(
          compilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
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
  publishArtifact in Test := false,
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
  pomPostProcess := { (node) =>
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
  wartremoverErrors in (Test, compile) ++= Seq(
    Wart.ExplicitImplicitTypes,
    Wart.ImplicitConversion)
) ++ Seq(Compile, Test).map{ scope =>
  scalacOptions in (scope, compile) := {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, v)) if v >= 13 =>
        (scalacOptions in (scope, compile)).value :+ "-Ymacro-annotations"
      case _ =>
        (scalacOptions in (scope, compile)).value
    }
  }
}

lazy val root = project.in(file("."))
  .settings(commonSettings: _*)
  .settings(noPublishSettings: _*)
  .aggregate(coreJVM, examplesJVM, coreJS, examplesJS)

def previousVersion(currentVersion: String): Option[String] = {
  val Version = """(\d+)\.(\d+)\.(\d+).*""".r
  val Version(x, y, z) = currentVersion
  if (z == "0") None
  else Some(s"$x.$y.${z.toInt - 1}")
}

lazy val core = crossProject(JSPlatform, JVMPlatform, NativePlatform).crossType(CrossType.Pure)
  .settings(commonSettings: _*)
  .settings(
    moduleName := "simulacrum",
    libraryDependencies += "org.typelevel" %% "macro-compat" % "1.1.1",
    scalacOptions in (Test) += "-Yno-imports"
  )
  .settings(scalaMacroDependencies:_*)
  .nativeSettings(
    nativeCommonSettings
  )
  .platformsSettings(JVMPlatform, JSPlatform)(
    libraryDependencies ++= scalatestSetting.value
  )
  .platformsSettings(JSPlatform, NativePlatform)(
    excludeFilter in (Test, unmanagedSources) := "jvm.scala"
  )
  .jvmSettings(
    mimaPreviousArtifacts := previousVersion(version.value).map { pv =>
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
  .settings(noPublishSettings: _*)
  .platformsSettings(JVMPlatform, JSPlatform)(
    libraryDependencies ++= scalatestSetting.value
  )
  .nativeSettings(
    nativeCommonSettings
  )

lazy val examplesJVM = examples.jvm
lazy val examplesJS = examples.js
lazy val examplesNative = examples.native

// Base Build Settings
lazy val scalaMacroDependencies: Seq[Setting[_]] = Seq(
  libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided",
    "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided"
  ),
  libraryDependencies ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      // if scala 2.11+ is used, quasiquotes are merged into scala-reflect
      case Some((2, scalaMajor)) if scalaMajor >= 11 => Seq()
      // in Scala 2.10, quasiquotes are provided by macro paradise
      case Some((2, 10)) =>
        Seq(
          compilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full),
              "org.scalamacros" %% "quasiquotes" % "2.1.0" cross CrossVersion.binary
        )
    }
  }
)

lazy val noPublishSettings = Seq(
  publish := {},
  publishLocal := {},
  publishArtifact := false
)

