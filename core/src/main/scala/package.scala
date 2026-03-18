import nelson.Manifest.{Loadbalancer, UnitDef, Versioned}
import nelson.Nelson.NelsonK

package object nelson {

  import io.circe.{Decoder, parser}
  import io.github.iltotore.iron.*
  import io.github.iltotore.iron.constraint.all.*

  import cats.Order
  import cats.data.Kleisli
  import cats.effect.IO

  import fs2.Stream

  import java.io.File
  import java.net.URI
  import java.nio.file.{Files, Path, Paths}
  import java.nio.charset.StandardCharsets
  import java.util.Locale

  import scala.concurrent.duration._

  // ── Iron constraint definitions ───────────────────────────────────────────

  /** Matches lowercase alphanumeric-and-hyphen unit names (e.g. "my-service-v2"). */
  type UnitNamePattern = Match["[a-z][a-z0-9-]*"]

  /** Short deployment hash: exactly 8 hex characters. */
  type DeploymentHashPattern = FixedLength[8] & Match["[0-9a-f]+"]

  /** Simple email pattern constraint. */
  type EmailPattern = Match[".+@.+\\..+"]

  // ── Domain type aliases ───────────────────────────────────────────────────
  // Plain type aliases maintained for backward compatibility throughout the codebase.
  // Iron-validated smart constructors are provided via the `Validate` object below.

  type ID                  = Long
  type GUID                = String
  type TagName             = String
  type UnitName            = String
  type DeploymentHash      = String
  type TempoaryAccessCode  = String
  type WorkflowRef         = String
  type BlueprintRef        = (String, blueprint.Blueprint.Revision)
  type DatacenterRef       = String
  type StatusMessage       = String
  type DependencyEdge      = (routing.RoutingNode, routing.RoutingNode)
  type ExpirationPolicyRef = String
  type EmailAddress        = String
  type UnitRef             = String
  type PlanRef             = String
  type LoadbalancerRef     = String
  type DNSName             = String
  type DeploymentStatusString = String
  type Sha256              = String
  type RenderedBlueprint   = String

  // ── Iron-validated smart constructors ────────────────────────────────────
  // Use these at API / input boundaries to validate values with iron refinements.
  // The validated types carry their constraint proof in the type (e.g. String :| C).

  object Validate {
    /** Validate a unit name: non-empty, lowercase, alphanumeric+hyphen. */
    def unitName(s: String): Either[String, String :| (MinLength[1] & UnitNamePattern)] =
      s.refineEither[MinLength[1] & UnitNamePattern]

    /** Validate a deployment hash: exactly 8 lowercase hex chars. */
    def deploymentHash(s: String): Either[String, String :| DeploymentHashPattern] =
      s.refineEither[DeploymentHashPattern]

    /** Validate an email address against a simple pattern. */
    def emailAddress(s: String): Either[String, String :| EmailPattern] =
      s.refineEither[EmailPattern]

    /** Validate a non-empty string. */
    def nonEmpty(s: String): Either[String, String :| MinLength[1]] =
      s.refineEither[MinLength[1]]

    /** Validate a namespace name string (non-empty, dot-separated lowercase). */
    def namespaceName(s: String): Either[NelsonError, NamespaceName] =
      NamespaceName.fromString(s)
  }

  // ── JSON helper ───────────────────────────────────────────────────────────

  /** Decode a JSON string into `A` using circe. */
  def fromJson[A: Decoder](in: String): IO[A] =
    parser.decode[A](in).fold(e => IO.raiseError(e), IO.pure)

  // ── Versionable typeclass ─────────────────────────────────────────────────

  given versionableUnit: Versionable[Versioned[UnitDef]] with {
    def version(u: Versioned[UnitDef]): Version =
      Manifest.Versioned.unwrap(u).deployable.yolo(
        s"no deployable for $u when attempting to extract version").version
  }

  given versionableLoadbalancer: Versionable[Versioned[Loadbalancer]] with {
    def version(lb: Versioned[Loadbalancer]): Version =
      Manifest.Versioned.unwrap(lb).majorVersion.yolo(
        s"no major version for $lb when attempting to extract version").minVersion
  }

  // ── Option helpers ────────────────────────────────────────────────────────

  extension [A](in: Option[A]) {
    def nfold[B](e: NelsonError)(f: A => B): NelsonK[B] =
      Kleisli.liftF(tfold(e)(f))

    def tfold[B](e: NelsonError)(f: A => B): IO[B] =
      in.fold(IO.raiseError[B](e))(a => IO(f(a)))

    def yolo(err: => String): A =
      in.getOrElse(throw new NoSuchElementException(err))
  }

  // ── IO retry helper ───────────────────────────────────────────────────────

  extension [A](io: IO[A]) {
    /** Retry with exponential backoff using IO.sleep. */
    def retryExponentially(seed: FiniteDuration = 15.seconds, limit: Int = 5): IO[A] = {
      def loop(remaining: Int, delay: FiniteDuration): IO[A] =
        io.attempt.flatMap {
          case Right(a) => IO.pure(a)
          case Left(e)  =>
            if (remaining <= 0) IO.raiseError(e)
            else IO.sleep(delay) *> loop(remaining - 1, delay + seed)
        }
      loop(limit, seed)
    }
  }

  // ── String helpers ────────────────────────────────────────────────────────

  extension (s: String) {
    def toSnakeCase: String =
      s.replaceAll("""(\p{Lower})(\p{Upper})""", "$1_$2")
        .replaceAll("""(\p{Upper}+)(\p{Upper}\p{Lower})""", "$1_$2")
        .replaceAll("""[\s_]+""", "_")
        .toLowerCase(Locale.ROOT)

    def withTrailingSlash: String =
      if (s.trim.endsWith("/")) s else s"${s}/"
  }

  // ── URI link builder ──────────────────────────────────────────────────────

  def linkTo(resource: String)(network: NetworkConfig): URI = {
    val path = if (resource.startsWith("/")) resource else s"/$resource"
    val pro  = if (network.tls) "https" else "http"
    val por  = if (network.externalPort == 80 || network.externalPort == 443) ""
               else s":${network.externalPort}"
    new URI(s"${pro}://${network.externalHost}${por}${path}")
  }

  // ── Random helpers ────────────────────────────────────────────────────────

  private[this] val rng = new java.security.SecureRandom

  def randomAlphaNumeric(desiredLength: Int): String =
    rng.synchronized(new java.math.BigInteger(desiredLength * 5, rng).toString(32))

  // ── Instant ordering ──────────────────────────────────────────────────────

  private[nelson] given orderInstant: Order[java.time.Instant] =
    Order.from(_ compareTo _)

  // ── Version helpers ───────────────────────────────────────────────────────

  def featureVersionFrom1or2DotString(versionString: String): Option[FeatureVersion] =
    Version.fromString(versionString)
      .map(_.toFeatureVersion)
      .orElse(FeatureVersion.fromString(versionString))

  // ── Temp file helpers ─────────────────────────────────────────────────────

  private val DefaultTempDir =
    Paths.get(Option(System.getProperty("java.io.tmpdir")).getOrElse("/tmp"))

  def withTempFile[A](
    s: String,
    prefix: String = "nelson-",
    suffix: String = ".tmp",
    dir: Path = DefaultTempDir
  )(f: File => Stream[IO, A]): Stream[IO, A] =
    Stream.bracket(writeTempFile(dir, s, prefix, suffix))(file => IO { file.delete(); () })
      .flatMap(f)

  private def writeTempFile(dir: Path, s: String, prefix: String, suffix: String): IO[File] =
    IO {
      val path = Files.createTempFile(dir, prefix, suffix)
      val file = path.toFile
      Files.write(path, s.getBytes(StandardCharsets.UTF_8))
      file.deleteOnExit()
      file
    }
}
