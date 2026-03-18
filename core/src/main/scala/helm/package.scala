/** Minimal helm stub.
 *  Replaces `io.verizon.helm` (Consul client) which has no Scala 3 release.
 *  Provides enough of the ConsulOp algebra to compile the nelson codebase.
 */
package object helm {

  import cats.~>
  import cats.effect.IO
  import cats.free.Free

  sealed trait HealthStatus
  object HealthStatus {
    case object Passing  extends HealthStatus
    case object Warning  extends HealthStatus
    case object Critical extends HealthStatus
    case object Unknown  extends HealthStatus
  }

  /** Response from a Consul health check. */
  final case class HealthCheckResponse(
    node: String,
    checkId: String,
    name: String,
    status: HealthStatus,
    notes: String,
    output: String,
    serviceId: String,
    serviceName: String
  )

  /** Response from listing services registered with an agent. */
  final case class AgentServiceRegistration(
    id: String,
    name: String,
    tags: List[String],
    port: Option[Int],
    address: Option[String]
  )

  /** Run a ConsulOpF program against a natural transformation interpreter. */
  def run[A](interp: ConsulOp ~> IO, op: ConsulOp.ConsulOpF[A]): IO[A] =
    op.foldMap(interp)

  object http4s {
    /** Stub for Http4sConsulClient. Real implementation would make HTTP calls to Consul. */
    final class Http4sConsulClient(
      baseUri: org.http4s.Uri,
      client: org.http4s.client.Client[IO],
      token: Option[String],
      creds: Option[(String, String)]
    ) extends (ConsulOp ~> IO) {
      def apply[A](op: ConsulOp[A]): IO[A] =
        IO.raiseError(new NotImplementedError("Http4sConsulClient stub: not implemented"))
    }
  }
}
