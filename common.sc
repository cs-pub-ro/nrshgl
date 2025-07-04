import coursier.Repository
import mill._
import mill.scalalib._
import mill.scalalib.publish._

trait HasChisel
  extends ScalaModule {
  // Define these for building chisel from source
  def chiselModule: Option[ScalaModule]

  override def moduleDeps = super.moduleDeps ++ chiselModule

  def chiselPluginJar: T[Option[PathRef]]

  override def scalacOptions = T(super.scalacOptions() ++ chiselPluginJar().map(path => s"-Xplugin:${path.path}"))

  override def scalacPluginClasspath: T[Agg[PathRef]] = T(super.scalacPluginClasspath() ++ chiselPluginJar())

  // Define these for building chisel from ivy
  def chiselIvy: Option[Dep]

  override def ivyDeps = T(super.ivyDeps() ++ chiselIvy)

  def chiselPluginIvy: Option[Dep]

  override def scalacPluginIvyDeps: T[Agg[Dep]] = T(super.scalacPluginIvyDeps() ++ chiselPluginIvy.map(Agg(_)).getOrElse(Agg.empty[Dep]))
}

trait NrshglModule
  extends HasChisel {
    def fixedpointModule: ScalaModule

    override def moduleDeps = super.moduleDeps ++ Seq(fixedpointModule)
  }

trait FixedPointModule
  extends HasChisel {
    override def sources = T.sources {
        Seq(PathRef(millSourcePath / "src" / "main" / "scala"))
    }
  }
