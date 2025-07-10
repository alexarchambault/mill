package mill

/**
 * Core language-agnostic Mill APIs for use in your build files to define
 * [[Task]]s, [[Module]]s, etc.
 */
package object api {
  extension [T](task: Task[T]) {
    def unresolved: UnresolvedTask[T] =
      UnresolvedTask(task, Map.empty)
  }
}
