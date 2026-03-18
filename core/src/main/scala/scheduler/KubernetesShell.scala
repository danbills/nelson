package nelson
package scheduler

import nelson.Datacenter.{Deployment, StackName}
import nelson.Kubectl.{DeploymentStatus, JobStatus, KubectlError}
import nelson.scheduler.SchedulerOp._

import nelson.CatsHelpers._
import cats.~>
import cats.effect.IO
import cats.implicits._

import scala.concurrent.duration.FiniteDuration

/**
 * SchedulerOp interpreter that uses the Kubernetes API server.
 *
 * See: https://kubernetes.io/docs/api-reference/v1.8/
 */
final class KubernetesShell(
  kubectl: Kubectl,
  timeout: FiniteDuration
) extends (SchedulerOp ~> IO) {
  import KubernetesShell._

  def apply[A](fa: SchedulerOp[A]): IO[A] = fa match {
    case Delete(_, deployment) =>
      delete(deployment)
        .retryExponentially(limit = 3)
        .timed(timeout)
    case Launch(_, _, _, _, _, _, bp) =>
      kubectl.apply(bp)
        .retryExponentially(limit = 3)
        .timed(timeout)
    case Summary(_, ns, stackName) =>
      summary(ns, stackName)
        .retryExponentially(limit = 3)
        .timed(timeout)
  }

  def delete(deployment: Deployment): IO[Unit] = {
    val ns = deployment.namespace.name
    val stack = deployment.stackName

    val fallback =
      kubectl.deleteService(ns, stack).void.recoverWith {
        case err@KubectlError(_) if notFound(err) => kubectl.deleteCronJob(ns, stack).void.recoverWith {
          case err@KubectlError(_) if notFound(err) => kubectl.deleteJob(ns, stack).void.recover {
            case err@KubectlError(_) if notFound(err) => ()
          }
        }
      }

    deployment.renderedBlueprint.fold(fallback)(spec => kubectl.delete(spec).void)
  }

  private def notFound(error: KubectlError): Boolean =
    error.stderr.exists(_.startsWith("Error from server (NotFound)"))

  def summary(ns: NamespaceName, stackName: StackName): IO[Option[DeploymentSummary]] =
    deploymentSummary(ns, stackName).recoverWith { case _ =>
      cronJobSummary(ns, stackName).recoverWith { case _ =>
        jobSummary(ns, stackName).recover { case _ => None }
      }
    }

  def deploymentSummary(ns: NamespaceName, stackName: StackName): IO[Option[DeploymentSummary]] =
    kubectl.getDeployment(ns, stackName).map {
      case DeploymentStatus(available, unavailable) =>
        Some(DeploymentSummary(
          running   = available,
          pending   = unavailable,
          completed = None,
          failed    = None
        ))
    }

  def cronJobSummary(ns: NamespaceName, stackName: StackName): IO[Option[DeploymentSummary]] =
    kubectl.getCronJob(ns, stackName).map(js => Some(jobStatusToSummary(js)))

  def jobSummary(ns: NamespaceName, stackName: StackName): IO[Option[DeploymentSummary]] =
    kubectl.getJob(ns, stackName).map(js => Some(jobStatusToSummary(js)))
}

object KubernetesShell {
  private def jobStatusToSummary(js: JobStatus): DeploymentSummary =
    DeploymentSummary(
      running   = js.active,
      pending   = None,
      completed = js.succeeded,
      failed    = js.failed
    )
}
