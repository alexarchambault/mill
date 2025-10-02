package mill.api

import mill.api.daemon.internal.UnresolvedTaskApi

final case class UnresolvedTask[+T](task: Task[T], crossValues: Map[String, String])
    extends UnresolvedTaskApi[T]
