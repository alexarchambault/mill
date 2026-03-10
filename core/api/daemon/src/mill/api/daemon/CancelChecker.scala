package mill.api.daemon

import scala.util.control.ControlThrowable

trait CancelChecker {
  def isCanceled(): Boolean

  final def run[T](ifCanceled: => T)(block: (checkCanceled: () => Unit) => T): T =
    CancelChecker(this, ifCanceled)(block)
  final def run0[T](block: (() => Unit) => T): CancelChecker.ValueOrCanceled[T] =
    CancelChecker(this, CancelChecker.ValueOrCanceled(None))(f =>
      CancelChecker.ValueOrCanceled(Some((block(f))))
    )
}

object CancelChecker {
  final class Canceled extends ControlThrowable("canceled")
  def apply[T](
      cancelChecker: CancelChecker,
      ifCanceled: => T
  )(block: (checkCanceled: () => Unit) => T): T = {
    val cancelled = new Canceled
    try block(() => if (cancelChecker.isCanceled()) throw cancelled)
    catch {
      case `cancelled` => ifCanceled
    }
  }

  case class ValueOrCanceled[T](valueOpt: Option[T])
}
