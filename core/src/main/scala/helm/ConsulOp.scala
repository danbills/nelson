package helm

import cats.free.Free
import io.circe.{Encoder, Json}

/** The ConsulOp algebra — a Free monad over Consul operations. */
sealed abstract class ConsulOp[A] extends Product with Serializable

object ConsulOp {
  type ConsulOpF[A] = Free[ConsulOp, A]

  // KV store operations
  final case class KVGet(key: String)                          extends ConsulOp[Option[String]]
  final case class KVSet(key: String, value: String)          extends ConsulOp[Unit]
  final case class KVDelete(key: String)                       extends ConsulOp[Unit]
  final case class KVListKeys(prefix: String)                  extends ConsulOp[Set[String]]

  // Health check operations
  final case class HealthListChecksForService(
    service: String, tag: Option[String], near: Option[String], dc: Option[String]
  ) extends ConsulOp[List[HealthCheckResponse]]

  final case class HealthListChecksForNode(
    node: String, dc: Option[String]
  ) extends ConsulOp[List[HealthCheckResponse]]

  final case class HealthListChecksInState(
    state: String, near: Option[String], dc: Option[String], tag: Option[String]
  ) extends ConsulOp[List[HealthCheckResponse]]

  final case class HealthListNodesForService(
    service: String, near: Option[String], dc: Option[String],
    tag: Option[String], passingOnly: Option[Boolean], token: Option[String]
  ) extends ConsulOp[List[HealthCheckResponse]]

  // Agent operations
  final case class AgentRegisterService(
    id: Option[String], name: String, tags: Option[List[String]], port: Option[Int],
    address: Option[String], enableTagOverride: Option[Boolean],
    check: Option[String], checks: Option[List[String]]
  ) extends ConsulOp[Unit]

  final case class AgentDeregisterService(id: String) extends ConsulOp[Unit]

  case object AgentListServices extends ConsulOp[Map[String, AgentServiceRegistration]]

  final case class AgentEnableMaintenanceMode(
    id: String, enable: Boolean, reason: Option[String]
  ) extends ConsulOp[Unit]

  // Smart constructors
  def kvGet(key: String): ConsulOpF[Option[String]] =
    Free.liftF(KVGet(key))

  def kvSet(key: String, value: String): ConsulOpF[Unit] =
    Free.liftF(KVSet(key, value))

  def kvSetJson[A: Encoder](key: String, value: A): ConsulOpF[Unit] =
    Free.liftF(KVSet(key, Encoder[A].apply(value).noSpaces))

  def kvDelete(key: String): ConsulOpF[Unit] =
    Free.liftF(KVDelete(key))

  def kvListKeys(prefix: String): ConsulOpF[Set[String]] =
    Free.liftF(KVListKeys(prefix))

  def healthListChecksForService(service: String, tag: Option[String], near: Option[String], dc: Option[String]): ConsulOpF[List[HealthCheckResponse]] =
    Free.liftF(HealthListChecksForService(service, tag, near, dc))

  def healthListChecksForNode(node: String, dc: Option[String]): ConsulOpF[List[HealthCheckResponse]] =
    Free.liftF(HealthListChecksForNode(node, dc))

  def healthListChecksInState(state: String, near: Option[String], dc: Option[String], tag: Option[String]): ConsulOpF[List[HealthCheckResponse]] =
    Free.liftF(HealthListChecksInState(state, near, dc, tag))

  def healthListNodesForService(service: String, near: Option[String], dc: Option[String],
    tag: Option[String], passingOnly: Option[Boolean], token: Option[String]): ConsulOpF[List[HealthCheckResponse]] =
    Free.liftF(HealthListNodesForService(service, near, dc, tag, passingOnly, token))

  def agentRegisterService(id: Option[String], name: String, tags: Option[List[String]], port: Option[Int],
    address: Option[String], enableTagOverride: Option[Boolean],
    check: Option[String], checks: Option[List[String]]): ConsulOpF[Unit] =
    Free.liftF(AgentRegisterService(id, name, tags, port, address, enableTagOverride, check, checks))

  def agentDeregisterService(id: String): ConsulOpF[Unit] =
    Free.liftF(AgentDeregisterService(id))

  def agentListServices: ConsulOpF[Map[String, AgentServiceRegistration]] =
    Free.liftF(AgentListServices)

  def agentEnableMaintenanceMode(id: String, enable: Boolean, reason: Option[String]): ConsulOpF[Unit] =
    Free.liftF(AgentEnableMaintenanceMode(id, enable, reason))
}
