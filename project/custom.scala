import sbt._, Keys._
import spray.revolver.RevolverPlugin.autoImport._

object custom {
  def resources: Seq[Setting[_]] = Seq(
    Test / unmanagedResourceDirectories += baseDirectory.value / ".." / "etc" / "classpath" / "test"
  )

  def revolver: Seq[Setting[_]] = Seq(
    javaOptions ++= Seq(
      s"-Dlogback.configurationFile=${baseDirectory.value}/../etc/classpath/revolver/logback.xml",
      "log4j2.formatMsgNoLookups=true"
    ),
    reStartArgs :=
      (baseDirectory.value / ".." / "etc" / "development" / name.value / s"${name.value}.dev.cfg").getCanonicalPath :: Nil,
    reStart / mainClass := (run / mainClass).value
  )
}
