import sbt._
import sbt.Keys._
import play.Project._
import com.typesafe.sbteclipse.plugin.EclipsePlugin._
import bintray.Plugin._

object QBBuild extends Build {

  val QBVersion = "0.3"

  val QBRepositories = Seq(
    "Typesafe repository"     at "http://repo.typesafe.com/typesafe/releases/",
    "mandubian maven bintray" at "http://dl.bintray.com/mandubian/maven",
    "teamon.eu repo"          at "http://repo.teamon.eu",
    "Sonatype OSS Snapshots"  at "https://oss.sonatype.org/content/repositories/snapshots",
    "Mandubian repository releases" at "https://github.com/mandubian/mandubian-mvn/tree/master/releases"
  )

  val buildSettings = Project.defaultSettings ++
    Seq(bintrayPublishSettings:_*) ++
    Seq(ScoverageSbtPlugin.instrumentSettings:_*) ++
    Seq(
    organization := "org.qbproject",
    version := QBVersion,
    scalaVersion := "2.10.2",
    licenses := Seq("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
    EclipseKeys.projectFlavor := EclipseProjectFlavor.Scala,
    EclipseKeys.skipParents in ThisBuild := false,
    EclipseKeys.executionEnvironment := Some(EclipseExecutionEnvironment.JavaSE16),
    EclipseKeys.withSource := true
  )

  lazy val root = Project("qbroot", file("."),
    settings =  buildSettings
      ++ Seq(
      unmanagedSourceDirectories in Compile <+= baseDirectory(new File(_, "src/main/scala")),
      unmanagedSourceDirectories in Test    <+= baseDirectory(new File(_, "src/test/scala")),
      retrieveManaged := true
    )).aggregate(schemaProject, playProject, csvProject)

  lazy val schemaProject = Project("qbschema", file("qbschema"))
    .settings(buildSettings: _*)
    .settings(
      resolvers ++= QBRepositories,
      retrieveManaged := true,
      libraryDependencies ++= Seq(
        "com.typesafe.play" %% "play-json"         % "2.2.3",
        "org.specs2"        %% "specs2"            % "2.3.7"  % "test",
        "org.scalaz"        %% "scalaz-core"       % "7.0.5",
        "com.mandubian"     %% "play-json-zipper"  % "1.1"
      )
    )

  lazy val playProject = Project("qbplay", file("qbplay"))
    .settings(buildSettings: _*)
    .settings(
      resolvers ++= QBRepositories,
      retrieveManaged := true,
      libraryDependencies ++= Seq(
        "com.typesafe.play" %% "play"                % "2.2.3",
        "org.reactivemongo" %% "play2-reactivemongo" % "0.10.2",
        "eu.teamon"         %% "play-navigator"      % "0.5.0",
        "org.specs2"        %% "specs2" % "2.3.7"    % "test",
        "com.mandubian"     %% "play-json-zipper"    % "1.1",
        "com.github.axel22" %% "scalameter"          % "0.4"
      ),
      testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework"),
      Keys.fork in Test := false,
      Keys.parallelExecution in Test := false
    ).dependsOn(schemaProject)

  lazy val csvProject = Project("qb-csv", file("qb-csv"))
    .settings(buildSettings: _*)
    .settings(
      resolvers ++= QBRepositories,
      retrieveManaged := true,
      libraryDependencies ++= Seq(
        "com.typesafe.play" %% "play-json"           % "2.2.3",
        "net.sf.opencsv"    % "opencsv"              % "2.1",
        "org.specs2"        %% "specs2" % "2.3.7"    % "test",
        "org.scalaz"        %% "scalaz-core"         % "7.0.5"
    ),
      Keys.fork in Test := false,
      Keys.parallelExecution in Test := false
    ).dependsOn(schemaProject)

  lazy val playSampleProject = play.Project("qbplay-sample", QBVersion, path = file("qbplay-sample"))
    .settings(buildSettings: _*)
    .settings(playScalaSettings : _*)
    .settings(
      resolvers ++= QBRepositories,
      libraryDependencies ++= Seq(
        "org.specs2" %% "specs2"      % "1.13" % "test",
        "junit"      %  "junit"       % "4.8"  % "test",
        "org.scalaz" %% "scalaz-core" % "7.0.5"
      ),
      Keys.fork in Test := false
    )
    .dependsOn(schemaProject,playProject)
    .aggregate(schemaProject,playProject)

}