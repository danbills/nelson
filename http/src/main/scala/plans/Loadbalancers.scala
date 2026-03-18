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

import io.circe.{Encoder, Decoder, Json, DecodingFailure}
import io.circe.syntax._

import cats.effect.IO
import cats.implicits._

import org.http4s._
import org.http4s.circe._
import org.http4s.dsl.io._

final case class Loadbalancers(config: NelsonConfig) extends Default {
  import Datacenter.{Namespace, LoadbalancerDeployment}
  import Loadbalancers.{*, given}
  import Params._

  private given Encoder[Manifest.Route] =
    Encoder.instance { (r: Manifest.Route) =>
      Json.obj(
        "lb_port"              -> r.port.port.asJson,
        "backend_name"         -> r.destination.name.asJson,
        "backend_port_reference" -> r.destination.portReference.asJson
      )
    }

  private given Encoder[LoadbalancerDeployment] =
    Encoder.instance { (lb: LoadbalancerDeployment) =>
      Json.obj(
        "name"          -> lb.stackName.toString.asJson,
        "guid"          -> lb.guid.asJson,
        "deploy_time"   -> lb.deployTime.toEpochMilli.asJson,
        "routes"        -> lb.loadbalancer.routes.asJson,
        "address"       -> lb.address.asJson,
        "major_version" -> lb.loadbalancer.version.major.asJson
      )
    }

  private given Encoder[(DatacenterRef, Namespace, LoadbalancerDeployment)] =
    Encoder.instance { case (d, n, lb) =>
      Json.obj(
        "datacenter" -> d.asJson,
        "namespace"  -> n.name.asString.asJson
      ).deepMerge(lb.asJson)
    }

  import nelson.Json.{*, given}
  private given Encoder[Nelson.LoadbalancerSummary] =
    Encoder.instance { (ls: Nelson.LoadbalancerSummary) =>
      Json.obj(
        "namespace"  -> ls.namespace.name.asString.asJson,
        "datacenter" -> ls.namespace.datacenter.asJson,
        "dependencies" -> Json.obj(
          "outbound" -> ls.outboundDependencies.asJson
        )
      ).deepMerge(ls.loadbalancer.asJson)
    }

  val service: HttpRoutes[IO] = HttpRoutes.of[IO] {

    /*
     * POST /v1/loadbalancers
     *
     * {
     *   "name": "howdy-lb",
     *   "major_version": 1,
     *   "datacenter": "texas",
     *   "namespace": "dev"
     * }
     */
    case req @ POST -> Root / "v1" / "loadbalancers" & IsAuthenticated(_) =>
      decode[LoadbalancerLaunch](req) { lb =>
        json(Nelson.commitLoadbalancer(lb.name, lb.version, lb.datacenter, lb.namespace))
      }

    /*
     * DELETE /v1/loadbalancers/guid
     *
     * Deletes the loadbalancer for the given guid
     */
    case DELETE -> Root / "v1" / "loadbalancers" / guid & IsAuthenticated(_) =>
      json(Nelson.deleteLoadbalancerDeployment(guid))

    /*
     * GET /v1/loadbalaners/guid
     *
     * Returns the loadbalancer deployment for given guid
     */
    case GET -> Root / "v1" / "loadbalancers" / guid & IsAuthenticated(_) =>
      json(Nelson.fetchLoadbalancerDeployment(guid))

    /*
     * GET /v1/loadbalancers?dc=texas&ns=dev,prod
     *
     * List all the loadbalancer deployments given a list of datacenters and namespaces.
     * ns is required
     * dc is optional and if empty will query all datacenters
     */
    case GET -> Root / "v1" / "loadbalancers" :? Ns(ns) +& Dc(dc) & IsAuthenticated(_) =>
      val namespace = commaSeparatedStringToNamespace(ns)
      val datacenters = dc.map(commaSeparatedStringToList).getOrElse(Nil)
      namespace.toNel.toRight("This endpoint requires a non-empty 'ns' parameter.")
        .fold(
          e => BadRequest(e),
          ns => ns.sequence.fold(
            e => BadRequest(e.getMessage),
            n => json(Nelson.listLoadbalancers(datacenters, n)))
        )

  }
}

object Loadbalancers {

  final case class LoadbalancerLaunch(name: String, version: Int, datacenter: String, namespace: NamespaceName)

  given Decoder[LoadbalancerLaunch] =
    Decoder.instance { c =>
      for {
        a  <- c.downField("name").as[String]
        b  <- c.downField("major_version").as[Int]
        d  <- c.downField("datacenter").as[String]
        n  <- c.downField("namespace").as[String]
        nn <- NamespaceName.fromString(n)
                .left.map(e => DecodingFailure(s"unable to parse $n into a namespace: ${e.getMessage}", c.history))
      } yield LoadbalancerLaunch(a, b, d, nn)
    }
}
