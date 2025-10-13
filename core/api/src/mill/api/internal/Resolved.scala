package mill.api.internal

import mill.api.daemon.Segments

private[mill] sealed trait Resolved {
  def segments: Segments.WithCrossValues
  def cls: Class[?]

  def addCrossValues(crossValues: Seq[(String, String)]): Resolved
}

private[mill] object Resolved {
  implicit object resolvedOrder extends Ordering[Resolved] {
    def orderingKey(r: Resolved) = r match {
      case _: Module => 1
      case _: Command => 2
      case _: NamedTask => 3
    }

    override def compare(x: Resolved, y: Resolved): Int = {
      val keyX = orderingKey(x)
      val keyY = orderingKey(y)
      if (keyX == keyY) Segments.WithCrossValues.ordering.compare(x.segments, y.segments)
      else Ordering.Int.compare(keyX, keyY)
    }
  }

  case class Module(segments: Segments.WithCrossValues, cls: Class[?]) extends Resolved {
    def addCrossValues(crossValues: Seq[(String, String)]): Resolved =
      copy(segments = segments.addCrossValues(crossValues))
  }
  case class Command(segments: Segments.WithCrossValues, cls: Class[?]) extends Resolved {
    def addCrossValues(crossValues: Seq[(String, String)]): Resolved =
      copy(segments = segments.addCrossValues(crossValues))
  }
  case class NamedTask(segments: Segments.WithCrossValues, cls: Class[?]) extends Resolved {
    def addCrossValues(crossValues: Seq[(String, String)]): Resolved =
      copy(segments = segments.addCrossValues(crossValues))
  }
}
