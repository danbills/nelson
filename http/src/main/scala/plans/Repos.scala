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
package plans

import io.circe.{Encoder, Json}
import io.circe.syntax._
import org.http4s._
import org.http4s.circe._
import org.http4s.dsl.io._
import cats.effect.IO

final case class Repos(config: NelsonConfig) extends Default {
  import nelson.Json.{*, given}

  object Owner extends QueryParamDecoderMatcher[String]("owner")

  object State extends QueryParamDecoderMatcher[String]("state")

  object Page extends QueryParamDecoderMatcher[Int]("page")

  object Limit extends QueryParamDecoderMatcher[Int]("limit")

  private given Encoder[Datacenter.ServiceName] =
    Encoder[String].contramap(_.toString)

  private given Encoder[DeploymentStatus] =
    Encoder[String].contramap(_.toString)

  /**
   * {
   *   "name": "howdy-http",
   *   "description": "...",
   *   "id": 6,
   *   "kind": "service",
   *   "dependencies": [],
   *   "namespace": "dev",
   *   "hash": "c0fgd1x",
   *   "timestamp": "2016-02-20T13:52:12.709Z",
   *   "status": "pending",
   *   "deployment_url": "http://.../v1/deployments/6fdsfse245"
   * }
   */
  private given Encoder[ReleasedDeployment] =
    Encoder.instance { (d: ReleasedDeployment) =>
      Json.obj(
        "id"             -> d.id.asJson,
        "unit_id"        -> d.unit.id.asJson,
        "name"           -> d.unit.name.asJson,
        "description"    -> d.unit.description.asJson,
        "dependencies"   -> d.unit.dependencies.toList.asJson,
        "hash"           -> d.hash.asJson,
        "timestamp"      -> d.timestamp.toString.asJson,
        "status"         -> d.state.asJson,
        "deployment_url" -> linkTo(s"/v1/deployments/${d.guid}")(config.network).asJson
      )
    }

  /**
   * {
   *   "timestamp": "2016-02-20T13:52:12.709Z",
   *   "release_url": "https://github.com/example/howdy/releases/tag/0.23.33",
   *   "slug": "example/howdy",
   *   "version": "0.23.33",
   *   "id": 287,
   *   "deloyments": [ ... ]
   * }
   */
  private given Encoder[(Released, List[ReleasedDeployment])] =
    Encoder.instance { (t: (Released, List[ReleasedDeployment])) =>
      Json.obj(
        "id"          -> t._1.referenceId.asJson,
        "slug"        -> t._1.slug.toString.asJson,
        "version"     -> t._1.version.toString.asJson,
        "timestamp"   -> t._1.timestamp.toString.asJson,
        "deployments" -> t._2.asJson
      )
    }

  val service: HttpRoutes[IO] = HttpRoutes.of[IO] {
    //////////////////// LISTING ////////////////////

    // GET /v1/repos?owner=tim
    // GET /v1/repos?owner=stew
    case GET -> Root / "v1" / "repos" :? Owner(owner) & IsAuthenticated(session) =>
      json(Nelson.listRepositories(session, Option(owner)))

    // GET /v1/repos?state=active
    case GET -> Root / "v1" / "repos" :? State("active") & IsAuthenticated(session) =>
      json(Nelson.listRepositories(session, None))

    // lists repos for the currently logged in user
    case GET -> Root / "v1" / "repos" & IsAuthenticated(session) =>
      json(Nelson.listRepositories(session, Option(session.user.login)))

    //////////////////// WEBHOOKS ////////////////////

    // creates a web hook for the specified repo
    case POST -> Root / "v1" / "repos" / owner / repo / "hook" & IsAuthenticated(session) =>
      json(Nelson.createHook(session, Slug(owner,repo)))

    case DELETE -> Root / "v1" / "repos" / owner / repo / "hook" & IsAuthenticated(session) =>
      json(Nelson.deleteHook(session, Slug(owner,repo)))

    //////////////////// RELEASES ////////////////////

    case GET -> Root / "v1" / "repos" / owner / repo / "releases" & IsAuthenticated(_) =>
      json(Nelson.listRepositoryReleases(Slug(owner,repo)).map(_.toList))

    // GET /v1/releases?limit=30
    case GET -> Root / "v1" / "releases" & IsAuthenticated(_) =>
      json(Nelson.listReleases(None).map(_.toList))

    // GET /v1/releases/12345
    case GET -> Root / "v1" / "releases" / id & IsAuthenticated(_) =>
      jsonF(Nelson.getRelease(id.toLong).map(_.toList.headOption)){ option =>
        option match {
          case Some(dc) => Ok(dc.asJson)
          case None     => NotFound(s"the release with id '$id' was not found")
        }
      }
  }
}
