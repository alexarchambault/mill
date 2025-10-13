package mill.api

import java.util.regex.Matcher

final case class ExecutionPaths private[mill] (dest: os.Path, meta: os.Path, log: os.Path) {}

/**
 * Logic to resolve a [[Task]]'s [[Segments]] to the various paths on disk owned by that task.
 */
object ExecutionPaths {

  def taskPath(segments: Segments.WithCrossValues): os.SubPath = {
    val crossValuesPart = segments.crossValues
      .flatMap {
        case (k, v) =>
          Seq(k, v)
      }
      .map(sanitizePathSegment)
    val segmentStrings = segments.segments.parts.map(sanitizePathSegment)
    os.sub / segmentStrings.init / crossValuesPart / segmentStrings.last
  }

  def taskPath(
      segments: Segments,
      crossValues: Map[String, String]
  ): os.SubPath =
    taskPath(segments.withCrossValues(crossValues.toSeq))

  def resolve(
      outPath: os.Path,
      segments: Segments,
      crossValues: Map[String, String]
  ): ExecutionPaths = {
    val taskPath0 = outPath / taskPath(segments, crossValues)
    ExecutionPaths(
      taskPath0 / os.up / s"${taskPath0.last}.dest",
      taskPath0 / os.up / s"${taskPath0.last}.json",
      taskPath0 / os.up / s"${taskPath0.last}.log"
    )
  }

  def resolve(
      outPath: os.Path,
      task: ResolvedNamedTask[?]
  ): ExecutionPaths = resolve(outPath, task.task.ctx.segments, task.crossValues)

  // case-insensitive match on reserved names
  private val ReservedWinNames =
    raw"^([cC][oO][nN]|[pP][rR][nN]|[aA][uU][xX]|[nN][uU][lL]|[cC][oO][mM][0-9¹²³]|[lL][pP][tT][0-9¹²³])($$|[.].*$$)".r
  // Colons are not supported on Windows
  private val Colon = "[:]".r
  // Dollar sign `$` is our masking-character
  private val Dollar = "[$]".r
  // Forward-slashed are reserved for directory delimiters
  private val Slash = "/".r

  private val steps: Seq[String => String] = Seq(
    // Step 1: mask all existing dollar signs, so we can use the dollar as masking character
    s => Dollar.replaceAllIn(s, Matcher.quoteReplacement("$$")),
    // Step 2: mask reserved Windows names, like CON1 or LPT1
    _ match {
      case ReservedWinNames(keyword, rest) => s"${keyword}~${rest}"
      case s => s
    },
    // Step 3: Replace colon (:) with $colon
    s => Colon.replaceAllIn(s, Matcher.quoteReplacement("$colon")),
    // Step 4: Replace slash (/) with $slash
    s => Slash.replaceAllIn(s, Matcher.quoteReplacement("$slash"))
  )

  def sanitizePathSegment(segment: String): os.PathChunk = {
    // sanitize and implicitly convert to PathChunk
    steps.foldLeft(segment) { (segment, f) => f(segment) }
  }
}
