import mill._
import mill.scalalib._

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
  extends HasChisel

trait NrshglTestModule
  extends TestModule
    with HasChisel
    with TestModule.ScalaTest {

  def nrshglModule: NrshglModule

  def chiselModule = nrshglModule.chiselModule

  def chiselPluginJar: T[Option[PathRef]] = T(nrshglModule.chiselPluginJar())

  def chiselIvy: Option[Dep] = nrshglModule.chiselIvy

  def chiselPluginIvy: Option[Dep] = nrshglModule.chiselPluginIvy

  def scalatestIvy: Dep

  def scalaparIvy: Dep

  override def moduleDeps = super.moduleDeps ++ Some(nrshglModule)

  override def defaultCommandName() = "test"

  override def ivyDeps = T(
    super.ivyDeps() ++ Agg(
      scalatestIvy,
      scalaparIvy
    )
  )
}