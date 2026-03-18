package nelson
package health

import nelson.health.HealthCheckOp._

import nelson.CatsHelpers._
import cats.~>
import cats.effect.IO

import scala.concurrent.duration.FiniteDuration

final class KubernetesHealthClient(
  kubectl: Kubectl,
  timeout: FiniteDuration
) extends (HealthCheckOp ~> IO) {

  def apply[A](fa: HealthCheckOp[A]): IO[A] = fa match {
    case Health(_, ns, stackName) => kubectl.getPods(ns, stackName).timed(timeout)
  }
}
