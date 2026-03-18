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

import nelson.blueprint.Blueprint

import io.circe.{Encoder, Decoder, Json}
import io.circe.syntax._
import cats.effect.IO
import cats.implicits._
import org.http4s._
import org.http4s.circe._
import org.http4s.dsl.io._
import org.apache.commons.codec.digest.DigestUtils

object Blueprints {
  import nelson.Json.{*, given}

  final case class BlueprintRequestJson(
    name: String,
    description: Option[String],
    sha256: Sha256,
    template: String
  )

  given Decoder[BlueprintRequestJson] =
    Decoder.instance { c =>
      for {
        name        <- c.downField("name").as[String]
        description <- c.downField("description").as[Option[String]]
        sha256      <- c.downField("sha256").as[Sha256]
        template    <- c.downField("template").as[Base64].map(_.decoded)
      } yield BlueprintRequestJson(name, description, sha256, template)
    }

  given Encoder[Blueprint.Revision] =
    Encoder.instance {
      case Blueprint.Revision.HEAD       => Json.fromString("HEAD")
      case Blueprint.Revision.Discrete(x) => Json.fromString(x.toString)
    }

  given Encoder[Blueprint] =
    Encoder.instance { (b: Blueprint) =>
      val fields = List(
        Some("name"       -> b.name.asJson),
        b.description.map(d => "description" -> d.asJson),
        Some("revision"   -> b.revision.asJson),
        Some("state"      -> b.state.toString.toLowerCase.asJson),
        Some("sha256"     -> b.sha256.asJson),
        Some("template"   -> b.template.toString.asJson),
        Some("created_at" -> b.createdAt.asJson)
      ).flatten
      Json.obj(fields*)
    }

  given Encoder[BlueprintProof] =
    Encoder.instance { r =>
      Json.obj("content" -> Base64(r.content).asJson)
    }

  given Decoder[BlueprintProof] =
    Decoder.instance { c =>
      c.downField("content").as[Base64].map(b => BlueprintProof(b.decoded))
    }
}

final case class BlueprintProof(
  content: String
)

final case class Blueprints(config: NelsonConfig) extends Default {
  import Blueprints.{*, given}

  /* ensure that the the user-supplied template survived encode/decode */
  def hasIntegrity(suppliedSha256: Sha256, template: String): Boolean = {
    val computed = DigestUtils.sha256Hex(template)
    computed == suppliedSha256
  }

  val service: HttpRoutes[IO] = HttpRoutes.of[IO] {
    /*
     * GET /v1/blueprints
     *
     * List all the available blueprints
     */
    case GET -> Root / "v1" / "blueprints" & IsAuthenticated(_) =>
      json(Nelson.listBlueprints)

    /*
     * GET /v1/blueprints/gpu-accelerated-job
     * GET /v1/blueprints/gpu-accelerated-job@HEAD
     * GET /v1/blueprints/gpu-accelerated-job@5
     *
     * List all the available blueprints
     */
    case GET -> Root / "v1" / "blueprints" / keyAndRevision & IsAuthenticated(_) =>
      Blueprint.parseNamedRevision(keyAndRevision) match {
        case Right((n,r)) => json(Nelson.fetchBlueprint(n,r))
        case Left(_) => BadRequest(s"Unable to parse the supplied '${keyAndRevision}' blueprint reference.")
      }

    /*
     * POST /v1/blueprints/proof
     *
     * Provide a blueprint template and have Nelson render it with example data
     * for development purposes. This makes itterating on a blueprint much more
     * seamless and does not require a full deployment cycle.
     *
     * {{{
     *  {
     *    "content": "<base64 encoded template>"
     *  }
     * }}}
     */
    case req @ POST -> Root / "v1" / "blueprints" / "proof" & IsAuthenticated(_) => {
      decode[BlueprintProof](req){ proof =>
        json(Nelson.proofBlueprint(proof.content).map(BlueprintProof(_)))
      }
    }

    /*
     * POST /v1/blueprints
     *
     * Create or revise a new blueprint. Here we're using POST constantly as blueprints
     * are entirely immutable and there's no in-place mutation, but rather, discrete versions
     * are revised and all revisions are stored in perpetuity.
     *
     * {{{
     *  {
     *    "name": "use-nvidia-1080ti",
     *    "description": "only scheudle on nodes with nvida 1080ti hardware"
     *    "sha256": "1e34a423ebe1fafeda8277386ede3263b01357e490b124b69bc0bfb493e64140"
     *    "template": "<base64 encoded template>"
     *  }
     * }}}
     */
    case req @ POST -> Root / "v1" / "blueprints" & IsAuthenticated(session) if IsAuthorized(session) =>
      decode[BlueprintRequestJson](req){ bpr =>
        if (hasIntegrity(bpr.sha256, bpr.template))
          if (Option(bpr.name).exists(_.trim.nonEmpty))
            json(Nelson.createBlueprint(bpr.name, bpr.description, bpr.sha256, bpr.template))
          else
            BadRequest(s"Blueprints must have a name, otherwise users will not be able to reference them.")
        else
          BadRequest(s"The supplied Sha256 for decoded template content did not match the computed Sha256.")
      }
  }
}
