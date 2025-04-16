import mill._
import mill.scalalib._
import mill.scalalib.publish._
import $file.common

object v {
  val scala = "2.13.12"
  val chiselCrossVersions = Map(
    "5.1.0" -> (ivy"org.chipsalliance::chisel:5.1.0", ivy"org.chipsalliance:::chisel-plugin:5.1.0"),
  )
  val scalatest = ivy"org.scalatest::scalatest:3.2.0"
  val scalapar = ivy"org.scala-lang.modules::scala-parallel-collections:1.0.4"
}

object nrshgl extends Cross[Nrshgl](v.chiselCrossVersions.keys.toSeq)

trait Nrshgl
  extends common.NrshglModule
    with Cross.Module[String] {

  override def scalaVersion = T(v.scala)

  override def millSourcePath = os.pwd

  def chiselModule = None

  def chiselPluginJar = None

  def chiselIvy = Some(v.chiselCrossVersions(crossValue)._1)

  def chiselPluginIvy = Some(v.chiselCrossVersions(crossValue)._2)
}

object nrshgldut extends Cross[NrshglDut](v.chiselCrossVersions.keys.toSeq)

trait nrshglDut
  extends common.NrshglTestModule
    with Cross.Module[String] {

  override def scalaVersion = T(v.scala)

  override def millSourcePath = os.pwd / "tests"

  def nrshglModule = nrshgl(crossValue)

  def chiselModule = None

  def chiselPluginJar = None

  def chiselIvy = Some(v.chiselCrossVersions(crossValue)._1)

  def chiselPluginIvy = Some(v.chiselCrossVersions(crossValue)._2)

  def scalatestIvy = v.scalatest

  def scalaparIvy = v.scalapar
}
