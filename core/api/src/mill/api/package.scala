package mill

/**
 * Core language-agnostic Mill APIs for use in your build files to define
 * [[Task]]s, [[Module]]s, etc.
 */
package object api {
  extension [M <: Module](module: M) {
    def ref: ModuleRef[M] = ModuleRef(module)
  }
}
