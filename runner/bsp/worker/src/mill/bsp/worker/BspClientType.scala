package mill.bsp.worker

/** Used to handle edge cases for specific BSP clients. */
private[mill] enum BspClientType {

  /** Intellij IDEA */
  case IntellijBSP

  /** Any other BSP client */
  case Other(displayName: String)
}
