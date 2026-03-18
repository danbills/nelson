package nelson

/** Typeclass for extracting a Version from a versioned value.
 *  Replaces simulacrum's @typeclass — uses Scala 3 given/extension idioms.
 */
trait Versionable[A]:
  def version(a: A): Version

object Versionable:
  def apply[A](using ev: Versionable[A]): Versionable[A] = ev

  extension [A: Versionable](a: A)
    def version: Version = summon[Versionable[A]].version(a)
