package mill.api.daemon.internal

trait ModuleRefApi[+M <: ModuleApi] {
  def apply(): M
}
