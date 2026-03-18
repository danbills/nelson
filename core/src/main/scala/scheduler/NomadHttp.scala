//: ----------------------------------------------------------------------------
//: Copyright (C) 2017 Verizon.  All Rights Reserved.
//:
//:   Licensed under the Apache License, Version 2.0 (the "License");
//:   you may not use this file except in compliance with the License.
//:   You may obtain a copy of the License at
//:
//:       http://www.apache.org/licenses/LICENSE-2.0
//:
//:   Unless required by applicable law or agreed to in writing, software
//:   distributed under the License is distributed on an "AS IS" BASIS,
//:   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//:   See the License for the specific language governing permissions and
//:   limitations under the License.
//:
//: ----------------------------------------------------------------------------
package nelson
package scheduler

import nelson.Datacenter.{Deployment, StackName}
import nelson.Json._
import nelson.Manifest.{Environment, EnvironmentVariable, HealthCheck, Plan, Port, Ports, UnitDef}
import nelson.docker.Docker
import nelson.docker.Docker.Image

import cats.~>
import cats.effect.IO
import cats.implicits._

import io.circe.{Decoder, Json => CJson}
import io.circe.syntax._
import org.http4s.circe._
import org.http4s.circe.CirceEntityDecoder._

import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

object NomadHttp {
  private val log: Logger[IO] = Slf4jLogger.getLogger[IO]
}

final class NomadHttp(
  cfg: NomadConfig,
  nomad: Infrastructure.Nomad,
  client: org.http4s.client.Client[IO]
) extends (SchedulerOp ~> IO) {
  import NomadJson._
  import NomadHttp.log
  import SchedulerOp._
  import org.http4s._
  import org.http4s.client._
  import org.http4s.Status.NotFound
  import org.http4s.headers.Authorization

  def apply[A](co: SchedulerOp[A]): IO[A] =
    co match {
      case Delete(dc, d) =>
        deleteUnitAndChildren(dc, d).retryExponentially()
      case Launch(i, dc, ns, u, p, hash, _) =>
        val unit = Manifest.Versioned.unwrap(u)
        launch(unit, hash, u.version, i, dc, ns, p).retryExponentially()
      case Summary(dc, _, sn) =>
        summary(dc, sn)
    }

  private def summary(dc: Datacenter, sn: Datacenter.StackName): IO[Option[DeploymentSummary]] = {
    val name = buildName(sn)
    val request = addCreds(dc, Request[IO](Method.GET, nomad.endpoint / "v1" / "job" / name / "summary"))
    given Decoder[DeploymentSummary] = NomadJson.deploymentSummaryDecoder(name)
    client.expect[DeploymentSummary](request)(jsonOf[IO, DeploymentSummary]).map(Some(_)).recoverWith {
      case UnexpectedStatus(NotFound, _, _) => IO.pure(None)
    }
  }

  private def runningUnits(dc: Datacenter, prefix: Option[String]): IO[Set[RunningUnit]] = {
    val baseUri = nomad.endpoint / "v1" / "jobs"
    val uri = prefix.fold(baseUri)(p => baseUri.withQueryParam("prefix", p))
    val request = addCreds(dc, Request[IO](Method.GET, uri))
    client.expect[List[RunningUnit]](request).map(_.toSet)
  }

  private def deleteUnit(dc: Datacenter, name: String): IO[Unit] = {
    val request = addCreds(dc, Request[IO](Method.DELETE, nomad.endpoint / "v1" / "job" / name))
    client.expect[String](request).map(_ => ()).recoverWith {
      case UnexpectedStatus(NotFound, _, _) => IO.unit
    }
  }

  private def deleteUnitAndChildren(dc: Datacenter, d: Deployment): IO[Unit] = {
    val name = buildName(d.stackName)
    for {
      cs <- listChildren(dc, name).map(_.toList)
      _  <- cs.traverse_(_.name.fold(IO.unit)(deleteUnit(dc, _)))
      _  <- deleteUnit(dc, name)
    } yield ()
  }

  private def listChildren(dc: Datacenter, parentID: String): IO[Set[RunningUnit]] = {
    val prefix = parentID + "/periodic"
    runningUnits(dc, Some(prefix)).map(_.filter(_.parentID.exists(_ == parentID)))
  }

  private def addCreds(dc: Datacenter, req: Request[IO]): Request[IO] =
    dc.proxyCredentials.fold(req) { creds =>
      req.putHeaders(Authorization(BasicCredentials(creds.username, creds.password)))
    }

  private def getJson(u: UnitDef, name: String, img: Image, dc: Datacenter, ns: NamespaceName, plan: Plan): CJson = {
    val tags = cfg.requiredServiceTags.getOrElse(List()).toSet.union(u.meta)
    val schedule = Manifest.getSchedule(plan)
    NomadJson.job(name, plan, img, dc, schedule, u.ports, ns, nomad, tags)
  }

  private def buildName(sn: StackName): String =
    cfg.applicationPrefix.map(prefix => s"${prefix}-${sn.toString}").getOrElse(sn.toString)

  private def launch(u: UnitDef, hash: String, version: Version, img: Image, dc: Datacenter, ns: NamespaceName, plan: Plan): IO[String] = {
    val template = plan.environment.blueprint match {
      case Some(Left(_))   => IO.raiseError(new IllegalArgumentException(s"Internal error occured: un-hydrated blueprint passed to scheduler!"))
      case Some(Right(bp)) => IO.pure(bp.template)
      case None            => IO.raiseError(new IllegalArgumentException(s"Internal error occured: there is currently no support for a default Nomad blueprint!"))
    }

    template.flatMap { t =>
      launchDefault(u, hash, version, img, dc, ns, plan)
      val sn   = StackName(u.name, version, hash)
      val name = buildName(sn)
      val spec = t.render(Map.empty)
      io.circe.parser.parse(spec) match {
        case Left(err)   =>
          IO.raiseError(new IllegalArgumentException(s"Rendered blueprint was not valid JSON, failed with error: '${err.message}', render: '${spec}'"))
        case Right(json) => call(name, dc, json)
      }
    }
  }

  private def launchDefault(u: UnitDef, hash: String, version: Version, img: Image, dc: Datacenter, ns: NamespaceName, plan: Plan): IO[String] = {
    val sn = StackName(u.name, version, hash)
    val name = buildName(sn)
    val vars = plan.environment.bindings ::: List(
      EnvironmentVariable("NELSON_STACKNAME",     sn.toString),
      EnvironmentVariable("NELSON_DATACENTER",    dc.name),
      EnvironmentVariable("NELSON_ENV",           ns.root.asString),
      EnvironmentVariable("NELSON_NAMESPACE",     ns.asString),
      EnvironmentVariable("NELSON_DNS_ROOT",      dc.domain.name),
      EnvironmentVariable("NELSON_PLAN",          plan.name),
      EnvironmentVariable("NELSON_DOCKER_IMAGE",  img.toString),
      EnvironmentVariable("NELSON_MEMORY_LIMIT",  plan.environment.memory.limitOrElse(512D).toInt.toString),
      EnvironmentVariable("NELSON_NODENAME",      s"$${node.unique.name}"),
      EnvironmentVariable("NELSON_VAULT_POLICYNAME", getPolicyName(ns, name))
    )
    val p = plan.copy(environment = plan.environment.copy(bindings = vars))
    val json = getJson(u, name, img, dc, ns, p)
    call(name, dc, json)
  }

  def call(name: String, dc: Datacenter, json: CJson): IO[String] = {
    val request = addCreds(dc, Request[IO](Method.POST, nomad.endpoint / "v1" / "job" / name))
    log.debug(s"sending nomad the following payload: ${json.noSpaces}") >>
      client.expect[String](request.withEntity(json))
  }
}

object NomadJson {
  import Infrastructure.Nomad
  import io.circe.{Encoder, Json}
  import io.circe.syntax._
  import scala.concurrent.duration._

  sealed abstract class NetworkMode(val asString: String)
  final case object BridgeMode extends NetworkMode("bridge")
  final case object HostMode extends NetworkMode("host")

  // We need to pass in the id because Nomad uses it as a key in the response :(
  def deploymentSummaryDecoder(id: String): Decoder[DeploymentSummary] =
    Decoder.instance { c =>
      val inner = c.downField("Summary").downField(id)
      for {
        running   <- inner.downField("Running").as[Option[Int]]
        failed    <- inner.downField("Failed").as[Option[Int]]
        queued    <- inner.downField("Queued").as[Option[Int]]
        completed <- inner.downField("Complete").as[Option[Int]]
      } yield DeploymentSummary(running, queued, completed, failed)
    }

  // reference: https://www.nomadproject.io/docs/http/jobs.html
  given runningUnitDecoder: Decoder[RunningUnit] =
    Decoder.instance { c =>
      for {
        nm <- c.downField("Name").as[Option[String]]
        st <- c.downField("Status").as[Option[String]]
        p  <- c.downField("ParentID").as[Option[String]]
      } yield RunningUnit(nm, st, p)
    }

  def dockerConfigJson(
    nomad: Nomad,
    container: Docker.Image,
    ports: Option[Ports],
    nm: NetworkMode
  ): Json = {
    val maybePorts = ports.map(_.nel.map(p => Json.obj(p.ref -> Json.fromInt(p.port))))
    val fields = List(
      maybePorts.map(ps => "port_map" -> ps.toList.asJson),
      Some("image"        -> Json.fromString(s"https://${container.toString}")),
      Some("network_mode" -> Json.fromString(nm.asString)),
      Some("auth"         -> List(Json.obj(
        "username"       -> Json.fromString(nomad.dockerRepoUser),
        "password"       -> Json.fromString(nomad.dockerRepoPassword),
        "server_address" -> Json.fromString(nomad.dockerRepoServerAddress),
        "SSL"            -> Json.fromBoolean(true)
      )).asJson)
    ).flatten
    Json.obj(fields*)
  }

  // cpu in MHZ, mem in MB
  def resourcesJson(cpu: Int, mem: Int, ports: Option[Ports]): Json = {
    val maybePorts = ports.map(_.nel.map(p => Json.obj(
      "Label" -> Json.fromString(p.ref),
      "Value" -> Json.fromInt(0) // for dynamic ports this is required but is then ignored
    )))
    val networkFields = List(
      maybePorts.map(ps => "DynamicPorts" -> ps.toList.asJson),
      Some("mbits" -> Json.fromInt(1)) // https://github.com/hashicorp/nomad/issues/1282
    ).flatten
    Json.obj(
      "CPU"      -> Json.fromInt(cpu),
      "MemoryMB" -> Json.fromInt(mem),
      "IOPS"     -> Json.fromInt(0),
      "Networks" -> List(Json.obj(networkFields*)).asJson
    )
  }

  def logJson(maxFiles: Int, maxFileSize: Int): Json =
    Json.obj(
      "MaxFiles"      -> Json.fromInt(maxFiles),
      "MaxFileSizeMB" -> Json.fromInt(maxFileSize)
    )

  def getPolicyName(ns: NamespaceName, name: String) = s"nelson__${ns.root.asString}__${name}"

  def vaultJson(ns: NamespaceName, name: String): Json = {
    val policyName = getPolicyName(ns, name)
    Json.obj(
      "Policies"     -> List(policyName).asJson,
      "Env"          -> Json.fromBoolean(true),
      "ChangeMode"   -> Json.fromString("restart"),
      "ChangeSignal" -> Json.fromString("")
    )
  }

  def healthCheckJson(check: HealthCheck): Json = {
    val (typ, protocol): (String, Option[String]) =
      if (check.protocol == "http" || check.protocol == "https")
        ("http", Some(check.protocol))
      else (check.protocol, None)
    val skipVerify = check.protocol == "https"
    Json.obj(
      "Name"          -> Json.fromString(check.name),
      "PortLabel"     -> Json.fromString(check.portRef),
      "Path"          -> Json.fromString(check.path.getOrElse("")),
      "Protocol"      -> protocol.asJson,
      "Interval"      -> Json.fromLong(check.interval.toNanos),
      "Timeout"       -> Json.fromLong(check.timeout.toNanos),
      "TLSSkipVerify" -> Json.fromBoolean(skipVerify),
      "Type"          -> Json.fromString(typ),
      "Args"          -> Json.Null,
      "Id"            -> Json.fromString(""),
      "Command"       -> Json.fromString("")
    )
  }

  def servicesJson(name: String, port: Port, tags: Set[String], checks: List[HealthCheck]): Json = {
    val checksJson =
      if (checks.isEmpty)
        List(healthCheckJson(HealthCheck(s"tcp ${port.ref} ${name}", port.ref, "tcp", None, 10.seconds, 4.seconds)))
      else
        checks.map(healthCheckJson)
    Json.obj(
      "Name"      -> Json.fromString(name),
      "PortLabel" -> Json.fromString(port.ref),
      "Tags"      -> tags.toList.asJson,
      "Checks"    -> checksJson.asJson
    )
  }

  def ephemeralDiskJson(sticky: Boolean, migrate: Boolean, size: Int): Json =
    Json.obj(
      "Sticky"  -> Json.fromBoolean(sticky),
      "Migrate" -> Json.fromBoolean(migrate),
      "SizeMB"  -> Json.fromInt(size)
    )

  def envJson(bindings: List[EnvironmentVariable]): Json =
    Json.obj(bindings.map(a => a.name -> Json.fromString(a.value))*)

  def periodicJson(expression: String): Json =
    Json.obj(
      "Spec"            -> Json.fromString(expression),
      "Enabled"         -> Json.fromBoolean(true),
      "SpecType"        -> Json.fromString("cron"),
      "ProhibitOverlap" -> Json.fromBoolean(true)
    )

  def restartJson(retries: Int): Json =
    Json.obj(
      "Interval" -> Json.fromLong(5.minutes.toNanos),
      "Attempts" -> Json.fromInt(retries),
      "Delay"    -> Json.fromLong(15.seconds.toNanos),
      "Mode"     -> Json.fromString("delay")
    )

  def leaderTaskJson(name: String, i: Image, env: Environment, nm: NetworkMode, ports: Option[Ports], nomad: Nomad, ns: NamespaceName, plan: PlanRef, tags: Set[String]): Json = {
    val cpu = (nomad.mhzPerCPU * env.cpu.limitOrElse(0.5)).toInt
    val mem = env.memory.limitOrElse(512.0).toInt
    val services = ports.map(_.nel.map(p => servicesJson(
      name,
      p,
      Set(ns.root.asString, s"port--${p.ref}", s"plan--$plan").union(tags),
      env.healthChecks.filter(_.portRef == p.ref)
    )))
    val fields = List(
      services.map(ss => "Services" -> ss.toList.asJson),
      Some("Vault"     -> vaultJson(ns, name)),
      Some("Name"      -> Json.fromString(name)),
      Some("Driver"    -> Json.fromString("docker")),
      Some("leader"    -> Json.fromBoolean(true)),
      Some("Config"    -> dockerConfigJson(nomad, i, ports, nm)),
      Some("Env"       -> envJson(env.bindings)),
      Some("Resources" -> resourcesJson(cpu, mem, ports)),
      Some("LogConfig" -> logJson(10, 10))
    ).flatten
    Json.obj(fields*)
  }

  def job(name: String, plan: Plan, i: Image, dc: Datacenter, schedule: Option[Schedule], ports: Option[Ports], ns: NamespaceName, nomad: Nomad, tags: Set[String]): Json = {
    val env      = plan.environment
    val periodic = schedule.flatMap(_.toCron().map(periodicJson))
    val sched    = if (schedule.isDefined) "batch" else "service"
    val jobFields = List(
      periodic.map(p => "Periodic"   -> p),
      Some("Region"      -> Json.fromString(dc.name)),
      Some("Datacenters" -> List(dc.name).asJson),
      Some("ID"          -> Json.fromString(name)),
      Some("Name"        -> Json.fromString(name)),
      Some("Type"        -> Json.fromString(sched)),
      Some("Priority"    -> Json.fromInt(50)),
      Some("AllAtOnce"   -> Json.fromBoolean(false)),
      Some("TaskGroups"  -> List(Json.obj(
        "Name"          -> Json.fromString(name),
        "Count"         -> Json.fromInt(env.desiredInstances.getOrElse(1)),
        "RestartPolicy" -> env.retries.map(restartJson).getOrElse(restartJson(3)),
        "Tasks"         -> List(leaderTaskJson(name, i, env, BridgeMode, ports, nomad, ns, plan.name, tags)).asJson
      )).asJson)
    ).flatten
    Json.obj("Job" -> Json.obj(jobFields*))
  }
}
