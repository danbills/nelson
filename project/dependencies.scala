import sbt._, Keys._

object dependencies {
  // Scala 3 does not need kind-projector, macro-paradise, or simulacrum plugins.
  // All compiler plugins from Scala 2 era have been removed.
}
