package controlstructures

object DependencyGraph {
  def dsl()(implicit
            input: InputSource
  ): Controller =
    Controller.dsl(Service.dsl())
}

object Boot extends App {
  val program: Controller =
    DependencyGraph.dsl()

  program.run()
}
