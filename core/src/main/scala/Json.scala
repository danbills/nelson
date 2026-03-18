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

import java.net.URI

import nelson.scheduler._

import io.circe.{Encoder, Decoder, Json, HCursor, DecodingFailure}
import io.circe.syntax._

import cats.implicits._

import concurrent.duration._

object Json {
  import Datacenter._
  import health.HealthStatus

  // ── Primitives ────────────────────────────────────────────────────────────

  given Encoder[URI] = Encoder[String].contramap(_.toString)
  given Decoder[URI] = Decoder[String].map(new URI(_))

  given Encoder[Duration] = Encoder[Long].contramap(_.toMillis)

  given Encoder[java.time.Instant] = Encoder[Long].contramap(_.toEpochMilli)
  given Decoder[java.time.Instant] = Decoder[Long].map(java.time.Instant.ofEpochMilli)

  // org.http4s.Uri encoder/decoder
  given Encoder[org.http4s.Uri] = Encoder[String].contramap(_.renderString)
  given Decoder[org.http4s.Uri] = Decoder[String].emap(s =>
    org.http4s.Uri.fromString(s).leftMap(_.message)
  )

  // ── Core domain ───────────────────────────────────────────────────────────

  given Encoder[AccessToken] = Encoder.forProduct1("access_token")(_.value)
  given Decoder[AccessToken] = Decoder.forProduct1("access_token")(AccessToken.apply)

  given Encoder[StackName] = Encoder[String].contramap(_.toString)

  given Encoder[Deployment] = Encoder.instance { d =>
    Json.obj(
      "stack_name"  -> d.stackName.asJson,
      "deployed_at" -> d.deployTime.asJson,
      "workflow"    -> d.workflow.asJson,
      "guid"        -> d.guid.asJson,
      "unit"        -> d.unit.name.asJson,
      "plan"        -> d.plan.asJson,
      "resources"   -> d.unit.resources.asJson
    )
  }

  given Encoder[RunningUnit] = Encoder.instance { u =>
    Json.obj(
      "name"   -> u.name.asJson,
      "status" -> u.status.asJson
    )
  }

  given Encoder[routing.RoutingNode] = Encoder.instance { rn =>
    rn.node.fold(
      lb => Json.obj(
        "guid"        -> lb.guid.asJson,
        "stack_name"  -> lb.stackName.toString.asJson,
        "type"        -> "loadbalancer".asJson,
        "deployed_at" -> lb.deployTime.asJson
      ),
      de => Json.obj(
        "guid"        -> de.guid.asJson,
        "stack_name"  -> de.stackName.toString.asJson,
        "type"        -> "unit".asJson,
        "deployed_at" -> de.deployTime.asJson
      )
    )
  }

  given Encoder[(routing.RoutePath, routing.RoutingNode)] = Encoder.instance {
    case (rp, rn) =>
      Json.obj(
        "weight"    -> rp.weight.asJson,
        "port_name" -> rp.portName.asJson
      ).deepMerge(rn.asJson)
  }

  given Encoder[DependencyEdge] = Encoder.instance { de =>
    Json.obj(
      "from" -> de._1.asJson,
      "to"   -> de._2.asJson
    )
  }

  given Encoder[Throwable] = Encoder.instance { t =>
    Json.obj("msg" -> t.getMessage.asJson)
  }

  given [A <: NelsonError]: Encoder[A] = Encoder.instance { e =>
    Json.obj("msg" -> e.getMessage.asJson)
  }

  given Encoder[Organization] = Encoder.instance { o =>
    Json.obj(
      "id"     -> o.id.asJson,
      "name"   -> o.name.asJson,
      "slug"   -> o.slug.asJson,
      "avatar" -> o.avatar.asJson
    )
  }

  given Decoder[Organization] = Decoder.instance { c =>
    for {
      id     <- c.downField("id").as[Long]
      name   <- c.downField("name").as[Option[String]]
      login  <- c.downField("login").as[String]
      avatar <- c.downField("avatar_url").as[URI]
    } yield Organization(id, name, login, avatar)
  }

  given Encoder[Hook] = Encoder.instance { h =>
    Json.obj(
      "id"        -> h.id.asJson,
      "is_active" -> h.isActive.asJson
    )
  }

  given Decoder[RepoAccess] = Decoder.instance { c =>
    for {
      push  <- c.downField("push").as[Boolean]
      pull  <- c.downField("pull").as[Boolean]
      admin <- c.downField("admin").as[Boolean]
    } yield RepoAccess.fromBools(admin, push, pull)
  }

  given Decoder[Repo] = Decoder.instance { c =>
    for {
      id       <- c.downField("id").as[Long]
      fullName <- c.downField("full_name").as[String]
      slug     <- Slug.fromString(fullName).left.map(e => DecodingFailure(e.getMessage, c.history))
      access   <- c.downField("permissions").as[RepoAccess]
    } yield Repo(id, slug, access)
  }

  given Encoder[Repo] = Encoder.instance { r =>
    Json.obj(
      "id"         -> r.id.asJson,
      "slug"       -> r.slug.toString.asJson,
      "owner"      -> r.slug.owner.asJson,
      "repository" -> r.slug.repository.asJson,
      "access"     -> r.access.toString.asJson,
      "hook"       -> r.hook.asJson
    )
  }

  // never encode the whole session — only a non-private subset
  given Encoder[Session] = Encoder.instance { s =>
    Json.obj(
      "user" -> Json.obj(
        "name"          -> s.user.name.asJson,
        "login"         -> s.user.login.asJson,
        "avatar"        -> s.user.avatar.toString.asJson,
        "organizations" -> (s.user.toOrganization +: s.user.orgs).asJson
      )
    )
  }

  // ── User ─────────────────────────────────────────────────────────────────

  given Encoder[User] = Encoder.forProduct5("login", "avatar_url", "name", "email", "organizations")(
    u => (u.login, u.avatar, u.name, u.email, u.orgs)
  )

  given Decoder[User] = Decoder.forProduct5("login", "avatar_url", "name", "email", "organizations")(
    User.apply
  )

  // ── Github types ─────────────────────────────────────────────────────────

  given Decoder[Github.WebHook] = Decoder.instance { c =>
    for {
      id     <- c.downField("id").as[Long]
      name   <- c.downField("name").as[String]
      events <- c.downField("events").as[List[String]]
      active <- c.downField("active").as[Boolean]
      config <- c.downField("config").as[Map[String, String]]
    } yield Github.WebHook(id, name, events, active, config)
  }

  given Encoder[Github.WebHook] = Encoder.instance { w =>
    Json.obj(
      "name"   -> w.name.asJson,
      "events" -> w.events.asJson,
      "active" -> w.active.asJson,
      "config" -> w.config.asJson
    )
  }

  given Decoder[Github.PingEvent] = Decoder.instance { c =>
    c.downField("zen").as[String].map(Github.PingEvent.apply)
  }

  given Decoder[Github.Contents] = Decoder.instance { c =>
    for {
      content <- c.downField("content").as[String]
      name    <- c.downField("name").as[String]
      size    <- c.downField("size").as[Long]
    } yield Github.Contents(content, name, size)
  }

  given Decoder[Github.Asset] = Decoder.instance { c =>
    for {
      id   <- c.downField("id").as[Long]
      name <- c.downField("name").as[String]
      url  <- c.downField("url").as[org.http4s.Uri]
    } yield Github.Asset(id, name, url)
  }

  given Encoder[Github.Asset] = Encoder.instance { asset =>
    Json.obj(
      "id"      -> asset.id.asJson,
      "name"    -> asset.name.asJson,
      "url"     -> asset.url.renderString.asJson,
      "content" -> asset.content.asJson
    )
  }

  given Decoder[Github.Release] = Decoder.instance { c =>
    for {
      id      <- c.downField("id").as[Long]
      url     <- c.downField("url").as[String]
      htmlUrl <- c.downField("html_url").as[String]
      assets  <- c.downField("assets").as[List[Github.Asset]]
      tagName <- c.downField("tag_name").as[String]
    } yield Github.Release(id, url, htmlUrl, assets, tagName)
  }

  given Encoder[Github.Release] = Encoder.instance { r =>
    Json.obj(
      "id"       -> r.id.asJson,
      "url"      -> r.url.asJson,
      "html_url" -> r.htmlUrl.asJson,
      "assets"   -> r.assets.asJson,
      "tag_name" -> r.tagName.asJson
    )
  }

  given Decoder[Github.Deployment] = Decoder.instance { z =>
    for {
      a <- z.downField("id").as[Long]
      c <- z.downField("ref").as[String]
      s <- z.downField("sha").as[String]
      d <- z.downField("environment").as[String]
      e <- z.downField("payload").as[String]
      g <- z.downField("url").as[String]
    } yield {
      val bytes = java.util.Base64.getDecoder.decode(e)
      val unmarshalled = nelson.api.deployable.Deployables.parseFrom(bytes)
      val converted = unmarshalled.deployables.toList.map { a =>
        val v = a.version.semver.get
        Manifest.Deployable(
          name = a.unitName,
          version = Version(v.major, v.minor, v.patch),
          output = Manifest.Deployable.Container(a.kind.container.get.image)
        )
      }
      Github.Deployment(
        id = a,
        ref = Github.Reference.fromString(c, Option(s)),
        environment = d,
        deployables = converted,
        url = g
      )
    }
  }

  given Encoder[Github.Deployment] = Encoder.instance { d =>
    Json.obj(
      "id"  -> d.id.asJson,
      "url" -> d.url.asJson,
      "ref" -> d.ref.toString.asJson
    )
  }

  given Decoder[Github.DeploymentEvent] = Decoder.instance { z =>
    for {
      deployment <- z.downField("deployment").as[Github.Deployment]
      fullName   <- z.downField("repository").downField("full_name").as[String]
      slug       <- Slug.fromString(fullName).left.map(e => DecodingFailure(e.getMessage, z.history))
      repoId     <- z.downField("repository").downField("id").as[Long]
    } yield Github.DeploymentEvent(slug = slug, repositoryId = repoId, deployment = deployment)
  }

  given Decoder[Github.ReleaseEvent] = Decoder.instance { z =>
    for {
      id       <- z.downField("release").downField("id").as[Long]
      fullName <- z.downField("repository").downField("full_name").as[String]
      slug     <- Slug.fromString(fullName).left.map(e => DecodingFailure(e.getMessage, z.history))
      repoId   <- z.downField("repository").downField("id").as[Long]
    } yield Github.ReleaseEvent(id = id, slug = slug, repositoryId = repoId)
  }

  given Decoder[Github.PullRequestEvent] = Decoder.instance { z =>
    for {
      id       <- z.downField("number").as[Long]
      url      <- z.downField("pull_request").downField("url").as[String]
      fullName <- z.downField("repository").downField("full_name").as[String]
      slug     <- Slug.fromString(fullName).left.map(e => DecodingFailure(e.getMessage, z.history))
    } yield Github.PullRequestEvent(id = id, url = url, slug = slug)
  }

  given Decoder[Github.Event] = List[Decoder[Github.Event]](
    Decoder[Github.DeploymentEvent].widen,
    Decoder[Github.ReleaseEvent].widen,
    Decoder[Github.PullRequestEvent].widen,
    Decoder[Github.PingEvent].widen
  ).reduceLeft(_ or _)

  given Encoder[Github.OrgKey] = Encoder.forProduct2("id", "login")(o => (o.id, o.slug))
  given Decoder[Github.OrgKey] = Decoder.forProduct2("id", "login")(Github.OrgKey.apply)

  given Encoder[Github.User] = Encoder.forProduct4("login", "avatar_url", "name", "email")(
    u => (u.login, u.avatar, u.name, u.email)
  )
  given Decoder[Github.User] = Decoder.forProduct4("login", "avatar_url", "name", "email")(
    Github.User.apply
  )

  // ── Audit ─────────────────────────────────────────────────────────────────

  given Encoder[audit.AuditLog] = Encoder.instance { a =>
    Json.obj(
      "id"         -> a.id.asJson,
      "timestamp"  -> a.timestamp.asJson,
      "releaseId"  -> a.releaseId.asJson,
      "event"      -> a.event.getOrElse(Json.Null),
      "category"   -> a.category.asJson,
      "action"     -> a.action.asJson,
      "login"      -> a.login.asJson
    )
  }

  given Decoder[audit.AuditLog] = Decoder.instance { c =>
    for {
      id        <- c.downField("id").as[ID]
      timestamp <- c.downField("timestamp").as[java.time.Instant]
      releaseId <- c.downField("releaseId").as[Option[Long]]
      event     <- c.downField("event").as[Option[Json]]
      category  <- c.downField("category").as[String]
      action    <- c.downField("action").as[String]
      login     <- c.downField("login").as[Option[String]]
    } yield audit.AuditLog(id, timestamp, releaseId, event, category, action, login)
  }

  // ── Manual deployment ────────────────────────────────────────────────────

  given Encoder[Datacenter.ManualDeployment] = Encoder.forProduct7(
    "datacenter", "namespace", "service_type", "version", "hash", "description", "port"
  )(d => (d.datacenter, d.namespace, d.serviceType, d.version, d.hash, d.description, d.port))

  given Decoder[Datacenter.ManualDeployment] = Decoder.forProduct7(
    "datacenter", "namespace", "service_type", "version", "hash", "description", "port"
  )(Datacenter.ManualDeployment.apply)

  // ── Routing graph ────────────────────────────────────────────────────────

  given Encoder[(Namespace, routing.RoutingGraph)] = Encoder.instance { case (n, g) =>
    Json.obj(
      "name" -> n.name.asString.asJson,
      "graph" -> g.nodes.toList.map { node =>
        Json.obj(
          "name"         -> node.stackName.toString.asJson,
          "dependencies" -> g.outs(node).map(_.to.stackName.toString).asJson
        )
      }.asJson
    )
  }

  // ── Scheduler ────────────────────────────────────────────────────────────

  given Encoder[scheduler.DeploymentSummary] = Encoder.instance { ds =>
    Json.obj(
      "running"   -> ds.running.asJson,
      "pending"   -> ds.pending.asJson,
      "completed" -> ds.completed.asJson,
      "failed"    -> ds.failed.asJson
    )
  }

  // ── Health ───────────────────────────────────────────────────────────────

  given Encoder[health.HealthCheck] = Encoder[String].contramap(health.HealthCheck.toString)

  given Encoder[HealthStatus] = Encoder.instance { h =>
    Json.obj(
      "name"     -> h.details.getOrElse("unspecified").asJson,
      "status"   -> h.status.asJson,
      "node"     -> h.node.asJson,
      "check_id" -> h.id.asJson
    )
  }

  // ── Runtime summary ──────────────────────────────────────────────────────

  given Encoder[Nelson.RuntimeSummary] = Encoder.instance { rs =>
    Json.obj(
      "scheduler"      -> rs.deployment.asJson,
      "consul_health"  -> rs.health.asJson,
      "current_status" -> rs.currentStatus.toString.asJson,
      "expires_at"     -> rs.expiresAt.asJson
    )
  }

  // ── Namespace name ───────────────────────────────────────────────────────

  given Decoder[NamespaceName] = Decoder[String].emap(s =>
    NamespaceName.fromString(s).leftMap(_.getMessage)
  )

  final case class NamespaceNameJson(namespace: NamespaceName)

  given Decoder[NamespaceNameJson] = Decoder.instance { c =>
    c.downField("namespace").as[NamespaceName].map(NamespaceNameJson.apply)
  }

  // ── Commit unit ──────────────────────────────────────────────────────────

  given Decoder[Nelson.CommitUnit] = Decoder.instance { c =>
    for {
      u  <- c.downField("unit").as[String]
      v  <- c.downField("version").as[String]
      t  <- c.downField("target").as[NamespaceName]
      vv <- Version.fromString(v).toRight(DecodingFailure(s"unable to parse $v into a version", c.history))
    } yield Nelson.CommitUnit(u, vv, t)
  }

  // ── Traffic shift ────────────────────────────────────────────────────────

  given Encoder[Datacenter.TrafficShift] = Encoder.instance { ts =>
    Json.obj(
      "from"    -> ts.from.asJson,
      "to"      -> ts.to.asJson,
      "start"   -> ts.start.asJson,
      "end"     -> ts.end.asJson,
      "reverse" -> ts.reverse.asJson,
      "policy"  -> ts.policy.ref.asJson
    )
  }

  // ── Helpers ──────────────────────────────────────────────────────────────

  /** Encode a map as a transposed JSON list of maps: [{klabel: k, vlabel: v}, ...] */
  def encodeTransposeMap[K: Encoder, V: Encoder](klabel: String, vlabel: String): Encoder[Map[K, V]] =
    Encoder.instance { m =>
      m.toList.map { case (k, v) =>
        Json.obj(klabel -> k.asJson, vlabel -> v.asJson)
      }.asJson
    }
}
