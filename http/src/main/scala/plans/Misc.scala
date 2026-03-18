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

import ManifestValidator.{ManifestValidation}
import ManifestValidator.Json.given
import cleanup.ExpirationPolicy.Json.given

import io.circe.{Encoder, Decoder, Json}
import io.circe.syntax._

import org.http4s.{BuildInfo => _, _}
import org.http4s.circe._
import org.http4s.dsl.io._

import cats.data.Validated.{Invalid, Valid}
import cats.effect.IO
import cats.syntax.apply._

final case class Misc(config: NelsonConfig) extends Default {
  import Misc.{*, given}
  import nelson.Json.{*, given}

  /*
   * {
   *   "stacks_by_status": {
   *     "labels": ["ready", "failed", "terminated"],
   *     "data": [300, 500, 100]
   *   }
   * }
   */
  private given Encoder[Nelson.RecentStatistics] =
    Encoder.instance { (s: Nelson.RecentStatistics) =>
      Json.obj(
        "stacks_by_status" -> Json.obj(
          "labels" -> s.statusCounts.map(_._1).asJson,
          "data"   -> s.statusCounts.map(_._2).asJson
        ),
        "most_deployed" -> Json.obj(
          "labels" -> s.mostDeployed.map(_._1).asJson,
          "data"   -> s.mostDeployed.map(_._2).asJson
        ),
        "least_deployed" -> Json.obj(
          "labels" -> s.leastDeployed.map(_._1).asJson,
          "data"   -> s.leastDeployed.map(_._2).asJson
        )
      )
    }

  given Encoder[Datacenter] = Encoder[String].contramap(_.name)

  def handleLintRequest(str: String, units: List[ManifestValidator.NelsonUnit]): IO[Response[IO]] =
    ManifestValidator.validate(str, units).run(config).attempt.flatMap {
      case Left(errors) =>
        // server error, blame ourselves
        InternalServerError(errors.toString.asJson)
      case Right(Invalid(errors)) =>
        // validation error, blame the user
        BadRequest(errors.toList.map(_.getMessage).mkString("\n\n"))
      case Right(Valid(_)) =>
        Ok()
    }

  val service: HttpRoutes[IO] = HttpRoutes.of[IO] {
    /*
     * POST /v1/profile/sync
     * Purpose of this resource is primarily to support the UI such that
     * it instructs Nelson to refresh its list of repositories from Github.
     */
    case POST -> Root / "v1" / "profile" / "sync" & IsAuthenticated(session) =>
      json(Nelson.syncRepos(session))

    /*
     * POST /v1/lint
     *
     * This resource expects the YAML of a .nelson.yml file to be posted,
     * to the service and we will check it for valid construction.
     *
     * The POST body should look something like:
     *
     * {{{
     * {
     *   "units": [
     *     {
     *       "kind": "nelson",
     *       "name": "nelson-0.4"
     *     }
     *   ],
     *   "manifest": "<base64 yaml>"
     * }
     * }}}
     *
     */
    case req @ POST -> Root / "v1" / "lint" =>
      decode[ManifestValidation](req){ mv => handleLintRequest(mv.config, mv.units) }

    /*
     * POST /v1/validate-template
     *
     * This resource expects a unit name and a template to be posted
     * We run it with a temporary vault policy and fake Nomad environment
     * variables created for the unit.
     *
     * The POST body should look something like this:
     *
     * {{{
     * {
     *   "unit": "howdy-http",
     *   "resources": ["s3"],
     *   "template": "<base64 encoded consul template>"
     * }
     * }}}
     *
     * If the template can be rendered, a 204 response is returned.  Output
     * is not returned to the user, so we don't expose secrets accessible
     * through the vault token.
     *
     * If the template can't be rendered, a 400 response is returned.  It
     * looks like this:
     *
     * {{{
     * {
     *   "details": "2017/02/06 20:31:57.832769 [INFO] consul-template v0.18.0 ...",
     *   "message": "template rendering failed"
     * }
     * }}}
     */
    case req @ POST -> Root / "v1" / "validate-template" & IsAuthenticated(_) => {
      import Templates._
      decode[Templates.TemplateValidation](req) { tv =>
        validateTemplate(tv).run(config).flatMap {
          case Rendered =>
            NoContent()
          case InvalidTemplate(errors) =>
            BadRequest(Json.obj(
              "message" -> Json.fromString("template rendering failed"),
              "details" -> Json.fromString(errors)
            ))
          case TemplateTimeout(errors) =>
            GatewayTimeout(Json.obj(
              "message" -> Json.fromString("template rendering timed out"),
              "details" -> Json.fromString(errors)
            ))
        }
      }
    }

    /*
     * GET /v1/cleanup-policies
     *
     * This resource return a list of cleanup policies with a descrption
     */
    case GET -> Root / "v1" / "cleanup-policies" & IsAuthenticated(_) =>
      Ok(cleanup.ExpirationPolicy.policies.toList.asJson)

    /*
     * GET /v1/statistics
     */
    case GET -> Root / "v1" / "statistics" & IsAuthenticated(_) =>
      json(Nelson.recentActivityStatistics)

    /*
     * Get /v1/build-info
     */
    case GET -> Root / "v1" / "build-info" =>
      val buildInfo = BuildInfo.asJson
      val json = buildInfo.deepMerge(Json.obj("banner" -> Banner.text.asJson))
      Ok(json)
  }
}

object Misc {
  import nelson.Json.{*, given}

  given Decoder[Templates.TemplateValidation] =
    Decoder.instance { c =>
      for {
        unit      <- c.downField("unit").as[UnitRef]
        resources <- c.downField("resources").as[Set[String]]
        template  <- c.downField("template").as[Base64].map(_.decoded)
      } yield Templates.TemplateValidation(unit, resources, template)
    }

  given Encoder[BuildInfo.type] =
    Encoder.instance { x =>
      Json.obj(
        "build_info" -> Json.obj(
          "name"          -> x.name.asJson,
          "version"       -> x.version.asJson,
          "scala_version" -> x.scalaVersion.asJson,
          "sbt_version"   -> x.sbtVersion.asJson,
          "git_revision"  -> x.gitRevision.asJson,
          "build_date"    -> x.buildDate.asJson
        )
      )
    }
}
