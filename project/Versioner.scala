import java.io.File
import java.time.Instant
import java.time.ZoneOffset
import java.time.ZonedDateTime

import sbt.IO

import scala.util.Try

protected class Versioner(
  readVersions: Seq[String] => Seq[(String, Versioner.Version)],
  codeVersions: (String, Seq[(String, Versioner.Version)]) => Seq[File]
) {

  def version(namespace: String, files: Seq[String]): Seq[File] = {
    val versions = readVersions("HEAD" +: files)

    codeVersions(namespace, versions)
  }
}

object Versioner {

  protected case class Version(version: Option[(String, ZonedDateTime)]) {

    def code: String = {
      val text = version match {
        case Some((hash: String, date: ZonedDateTime)) => s"""Some(Version("$hash", ZonedDateTime.parse("$date")))"""
        case None => "None"
      }

      text
    }
  }

  protected object Version {

    // In this version of the constructor, the date is in ISO 8601 format.
    def apply(hash: Option[String], date: Option[String]): Version = {
      if (hash.isDefined && date.isDefined) {
        Try {
          Version(Some(hash.get, ZonedDateTime.parse(date.get)))
        }.getOrElse(Version(None))
      }
      else
        Version(None)
    }
  }

  protected case class Versions(versions: Seq[(String, Version)]) {

    def code: String = {
      val text = versions.map { case (file, version) =>
        s""""$file" -> ${version.code}"""
      }.mkString(",\n    ")

      text
    }
  }

  protected def readVersionsBase(gitRunner: com.typesafe.sbt.git.GitRunner, gitCurrentBranch: String, baseDirectory: File)
      (files: Seq[String]): Seq[(String, Version)] = {
    val versions = files.map { file =>
      val gitArgs = Seq("rev-list", "--timestamp", "-1", gitCurrentBranch, file)
      // val gitArgs = Seq("log", """--format="%at %H"""", "--max-count=1", gitCurrentBranch, file)

      try {
        val output = gitRunner(gitArgs: _*)(baseDirectory, com.typesafe.sbt.git.NullLogger)
        val Array(timestamp, hash) = output.split(' ')
        val integerTime = Integer.parseInt(timestamp)
        val instant = Instant.ofEpochSecond(integerTime)
        val zonedDateTime = ZonedDateTime.ofInstant(instant, ZoneOffset.UTC)

        (file, Version(Some((hash, zonedDateTime))))
      }
      catch {
        case throwable: Throwable =>
          println(s"Warning: Couldn't get version for $file.")
          throwable.printStackTrace()
          (file, Version(None))
      }
    }

    versions
  }

  protected def codeVersionsBase(codebase: File)(namespace: String, versions: Seq[(String, Version)]): Seq[File] = {
    val filename = codebase.getCanonicalPath + "/" + namespace.replace('.', '/') + "/Versions.scala"
    val file = new File(filename)
    val versionCode = versions.head._2.code // This should be the HEAD
    val versionsCode = Versions(versions.tail).code

    val code = s"""
      |/* This code is automatically generated during project compilation. */
      |
      |package $namespace
      |
      |import java.time.ZonedDateTime
      |
      |case class Version(commit: String, date: ZonedDateTime)
      |
      |object Versions {
      |  // This first value applies to the entire repository.
      |  val version: Option[Version] = $versionCode
      |
      |  // These values are for individual files.
      |  val versions: Map[String, Option[Version]] = Map(
      |    $versionsCode
      |  )
      |}
      |""".stripMargin.trim + "\n"

    IO.write(file, code)
    Seq(file)
  }

  def apply(gitRunner: com.typesafe.sbt.git.GitRunner, gitCurrentBranch: String, baseDirectory: File, codebase: File): Versioner = {

    val readVersions = readVersionsBase(gitRunner, gitCurrentBranch, baseDirectory) _
    val codeVersions = codeVersionsBase(codebase) _

    new Versioner(readVersions, codeVersions)
  }
}
