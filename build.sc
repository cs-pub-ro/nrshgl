import mill._
import mill.scalalib._
import mill.scalalib.publish._
import $file.common

object v {
  val scala = "2.13.12"
  val chiselCrossVersions = Map(
    "5.1.0" -> (ivy"org.chipsalliance::chisel:5.1.0", ivy"org.chipsalliance:::chisel-plugin:5.1.0"),
  )
  val defaultChiselVersion = "5.1.0"
  val scalatest = ivy"org.scalatest::scalatest:3.2.0"
  val scalapar = ivy"org.scala-lang.modules::scala-parallel-collections:1.0.4"

  val scalatestplus = ivy"org.scalatestplus::scalacheck-1-14::3.2.2.0"

  val chiseltest = ivy"edu.berkeley.cs::chiseltest:5.0.2"

  val sl = ivy"ro.upb.nrs::sl:1.0.0"
}

object fixedpoint extends Cross[FixedPoint](v.chiselCrossVersions.keys.toSeq) {
  def defaultCrossValue = v.defaultChiselVersion
  def default() : FixedPoint = apply(defaultCrossValue)
}

trait FixedPoint
  extends common.HasChisel
    with Cross.Module[String] {
  override def scalaVersion = T(v.scala)

  override def millSourcePath = os.pwd / "fixedpoint"

  override def sources = T.sources {
    Seq(PathRef(millSourcePath / "src" / "main" / "scala"))
  }

  def chiselModule = None

  def chiselPluginJar = None

  def chiselIvy = Some(v.chiselCrossVersions(crossValue)._1)

  def chiselPluginIvy = Some(v.chiselCrossVersions(crossValue)._2)

  def scalatestIvy = v.scalatest

  def scalatestplusIvy = v.scalatestplus

  override def ivyDeps = T(
    super.ivyDeps() ++ Agg(
      scalatestIvy,
      scalatestplusIvy
    )
  )
}

object nrshgl extends Cross[Nrshgl](v.chiselCrossVersions.keys.toSeq) {
  def defaultCrossValue = v.defaultChiselVersion
  def default() : Nrshgl = apply(defaultCrossValue)
}

trait Nrshgl
  extends common.NrshglModule
    with Cross.Module[String] {

  override def scalaVersion = T(v.scala)

  override def millSourcePath = os.pwd / "nrshgl"

  def fixedpointModule: ScalaModule = fixedpoint(crossValue)

  def chiselModule = None

  def chiselPluginJar = None

  def chiselIvy = Some(v.chiselCrossVersions(crossValue)._1)

  def chiselPluginIvy = Some(v.chiselCrossVersions(crossValue)._2)

  override def moduleDeps = super.moduleDeps ++ Seq(fixedpointModule)
}

object nrshgldut extends Cross[NrshglDut](v.chiselCrossVersions.keys.toSeq) {
  def defaultCrossValue = v.defaultChiselVersion
  def default() : NrshglDut = apply(defaultCrossValue)
}

trait NrshglDut
  extends TestModule
    with common.HasChisel
    with TestModule.ScalaTest
    with Cross.Module[String]  {

  override def scalaVersion = T(v.scala)

  override def millSourcePath = os.pwd / "nrshgl" / "test"

  def nrshglModule: common.NrshglModule = nrshgl(crossValue)

  def chiselModule = nrshglModule.chiselModule

  def chiselPluginJar: T[Option[PathRef]] = T(nrshglModule.chiselPluginJar())

  def chiselIvy: Option[Dep] = nrshglModule.chiselIvy

  def chiselPluginIvy: Option[Dep] = nrshglModule.chiselPluginIvy

  def scalatestIvy = v.scalatest

  def scalaparIvy = v.scalapar

  def chiselTestIvy = v.chiseltest

  def slIvy = v.sl

  override def moduleDeps = super.moduleDeps ++ Some(nrshglModule)

  override def defaultCommandName() = "test"

  override def ivyDeps = T(
    super.ivyDeps() ++ Agg(
      scalatestIvy,
      scalaparIvy,
      chiselTestIvy,
      slIvy
    )
  )

  override def repositoriesTask = T.task {
    super.repositoriesTask() ++
    Seq(
      coursier.maven.MavenRepository("https://repo.repsy.io/mvn/sdcioc/nrs")
    )
  }
}
