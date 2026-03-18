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

import io.circe.{Encoder, Decoder, Json}
import io.circe.syntax._
import cats.effect.IO
import cats.implicits._
import org.http4s._
import org.http4s.circe._
import org.http4s.dsl.io._
import org.http4s.headers.Location

import java.time.Instant

final case class Datacenters(config: NelsonConfig) extends Default {
  import nelson.Json.{*, given}
  import Datacenter._
  import Params._

  private given Encoder[Nelson.StackSummary] =
    Encoder.instance { (s: Nelson.StackSummary) =>
      Json.obj(
        "expiration"   -> s.expiration.asJson,
        "statuses"     -> s.statuses.asJson,
        "namespace"    -> s.namespace.name.asString.asJson,
        "dependencies" -> Json.obj(
          "inbound"  -> s.inboundDependencies.asJson,
          "outbound" -> s.outboundDependencies.asJson
        )
      ).deepMerge(s.deployment.asJson)
    }

  private given Encoder[(DatacenterRef, Namespace, GUID, ServiceName)] =
    Encoder.instance { case (d, n, i, s) =>
      Json.obj(
        "datacenter" -> d.asJson,
        "namespace"  -> n.name.asString.asJson,
        "guid"       -> i.asJson
      ).deepMerge(s.asJson)
    }

  private given Encoder[(DatacenterRef, Namespace, Deployment, DeploymentStatus)] =
    Encoder.instance { case (d, n, s, ds) =>
      Json.obj(
        "datacenter" -> d.asJson,
        "namespace"  -> n.name.asString.asJson,
        "status"     -> ds.toString.asJson
      ).deepMerge(s.asJson)
    }

  private given Encoder[Namespace] =
    Encoder.instance { (ns: Namespace) =>
      Json.obj(
        "id"   -> ns.id.asJson,
        "name" -> ns.name.asString.asJson
      )
    }

  private given Encoder[(Datacenter, Set[Namespace])] =
    Encoder.instance { case (d, ns) =>
      Json.obj(
        "name"           -> d.name.asJson,
        "datacenter_url" -> linkTo(s"/v1/datacenters/${d.name}")(config.network).asJson,
        "namespaces"     -> ns.toList.map { n =>
          Json.obj(
            "deployments_url" -> linkTo(s"/v1/deployments?dc=${d.name}&ns=${n.name.asString}")(config.network).asJson,
            "units_url"       -> linkTo(s"/v1/units?dc=${d.name}&status=active,manual,deprecated")(config.network).asJson,
            "statistics_url"  -> linkTo(s"/v1/statistics?dc=${d.name}&namespace=${n.name.asString}")(config.network).asJson
          ).deepMerge(n.asJson)
        }.asJson
      )
    }

  private given Encoder[(DeploymentStatus, Option[StatusMessage], Instant)] =
    Encoder.instance { case (s, msg, ts) =>
      val fields = List(
        Some("status"    -> s.toString.asJson),
        msg.map(m        => "message" -> m.asJson),
        Some("timestamp" -> ts.toString.asJson)
      ).flatten
      Json.obj(fields*)
    }

  given Encoder[FeatureVersion] =
    Encoder.forProduct2("major", "minor")(fv => (fv.major, fv.minor))

  given Decoder[FeatureVersion] =
    Decoder.forProduct2("major", "minor")(FeatureVersion.apply)

  given Encoder[Datacenter.ServiceName] =
    Encoder.forProduct2("service_type", "version")(sn => (sn.serviceType, sn.version))

  given Decoder[Datacenter.ServiceName] =
    Decoder.forProduct2("service_type", "version")(Datacenter.ServiceName.apply)

  given Encoder[(Int, List[String])] =
    Encoder.instance { (r: (Int, List[String])) =>
      Json.obj(
        "offset"  -> r._1.asJson,
        "content" -> r._2.asJson
      )
    }

  val service: HttpRoutes[IO] = HttpRoutes.of[IO] {

    /*
     * GET /v1/datacenters
     *
     * List all the datacenters and their subordinate namespaces
     */
   case GET -> Root / "v1" / "datacenters" & IsAuthenticated(_) =>
      json(Nelson.listDatacenters(config.pools.defaultExecutor).map(_.toList))

    /*
     * GET /v1/datacenters/portland
     *
     * Show details for a single datacenter
     */
   case GET -> Root / "v1" / "datacenters" / dcname & IsAuthenticated(_) =>
      jsonF(Nelson.fetchDatacenterByName(dcname)){ option =>
        option match {
          case Some(dc) => Ok(dc.asJson)
          case None     => NotFound(s"datacenter '$dcname' does not exist")
        }
      }

    /*
     * GET /v1/datacenters/portland/graph?ns=devel,prod
     *
     * Returns a list of Namespaces with corresponding RoutingGraph within this datacenter
     */
   case GET -> Root /"v1" / "datacenters" / dcname / "graph" :? NsO(ns) & IsAuthenticated(_) =>
     ns.map(commaSeparatedStringToNamespace) match {
       case Some(ns) =>
         ns.sequence.fold(
           e => BadRequest(e.getMessage),
           n => json(Nelson.getRoutingGraphs(dcname, n)))
       case None =>
         json(Nelson.getRoutingGraphs(dcname, Nil))
     }

    /*
     * GET /v1/deployments?dc=texas,california&status=active,deploying&ns=devel
     *
     * List all the deployments given a list of datacenters and namespaces. Filter by deployment status
     * ns is required
     * dc is optional and if empty will query all datacenters
     * status is optional and if empty will filter by all DeploymentStatus
     */
   case GET -> Root / "v1" / "deployments" :? Ns(ns) +& Status(s) +& Dc(dc) +& U(u) & IsAuthenticated(_) =>
      val namespace = commaSeparatedStringToNamespace(ns)
      val datacenters = dc.map(commaSeparatedStringToList).getOrElse(Nil)
      val statuses = s.flatMap(commaSeparatedStringToStatus(_).toNel).getOrElse(DeploymentStatus.nel)
      val units = u
      namespace.toNel.toRight("This endpoint requires a non-empty 'ns' parameter.")
        .fold(
          e => BadRequest(e),
          ns => ns.sequence.fold(
            e => BadRequest(e.getMessage),
            n => json(Nelson.listDeployments(datacenters, n, statuses, units)))
        )

     /* POST /v1/datacenters/<dc>/namespaces
      *
      * Create namespace(s) (including roots) in the specified datacenter, must be an admin
      */
    case req @ POST -> Root / "v1" / "datacenters" / dcname / "namespaces" & IsAuthenticated(session) if IsAuthorized(session) =>
       decode[NamespaceNameJson](req){ ns =>
         json(Nelson.recursiveCreateNamespace(dcname.trim.toLowerCase, ns.namespace))
       }

    /*
     * POST /v1/datacenters/<dc>/namespaces
     *
     * Create subordinate namespace(s) in the specified datacenter.
     */
    case req @ POST -> Root / "v1" / "datacenters" / dcname / "namespaces" & IsAuthenticated(_) =>
      decode[NamespaceNameJson](req){ ns =>
        if (ns.namespace.isRoot) BadRequest("creating root namespace is not allowed")
        else json(Nelson.recursiveCreateSubordinateNamespace(dcname.trim.toLowerCase, ns.namespace))
      }


    /*
     * POST /v1/deployments
     *
     * Upon posting, if sucsessful will redirect you to the new deployment
     */
    case req @ POST -> Root / "v1" / "deployments" & IsAuthenticated(session) if IsAuthorized(session) =>
      decode[ManualDeployment](req){ md =>
        jsonF(Nelson.createManualDeployment(session,md)){ guid =>
          Uri.fromString(linkTo(s"/v1/deployments/$guid")(config.network).toString).fold(
            e => InternalServerError(s"Bad redirect: ${e.details}"),
            s => Found(Location(s))
          )
        }
      }

    /*
     * GET /v1/deployments/1a2dfg34
     *
     * Returns a summary of everything we know about this deployment
     */
    case GET -> Root / "v1" / "deployments" / guid & IsAuthenticated(_) =>
      jsonF(Nelson.fetchDeployment(guid)){
         _ match {
          case Some(summary) => Ok(summary.asJson)
          case None          => NotFound(s"the requested deployment, '${guid}', could not be found.")
        }
      }

    /*
     * GET /v1/deployments/1a2dfg34/runtime
     *
     * Returns a summary of everything we know about this deployment runtime
     */
    case GET -> Root / "v1" / "deployments" / guid / "runtime" & IsAuthenticated(_) =>
      jsonF(Nelson.getRuntimeSummary(guid)){
         _ match {
          case Some(summary) => Ok(summary.asJson)
          case None          => NotFound(s"the requested deployment, '${guid}', could not be found.")
        }
      }

    /*
     * GET /v1/deployments/<guid>/log
     *
     * Returns the log of all that Nelson did for a given deployment
     */
    case GET -> Root / "v1" / "deployments" / guid / "log" :? Offset(o) & IsAuthenticated(_) =>
      jsonF(Nelson.fetchWorkflowLog(guid, o.getOrElse(0))){
        _ match {
          case Some(tuple) => Ok(tuple.asJson)
          case None        => NotFound(s"the requested deployment, '${guid}', could not be found.")
        }
      }

    /*
     * POST /v1/deployments/<guid>/redeploy
     *
     * Triggers a redeployment of the specified deployment GUID
     */
    case POST -> Root / "v1" / "deployments" / guid / "redeploy" & IsAuthenticated(_) =>
      json(Nelson.redeploy(guid))

    /*
     * POST /v1/deployments/<guid>/trafficshift/reverse
     *
     * Triggers a reverse of an in progress traffic shift given the guid of the to deployment
     */
    case POST -> Root / "v1" / "deployments" / guid / "trafficshift" / "reverse" & IsAuthenticated(_) =>
      json(Nelson.reverseTrafficShift(guid))

    /*
     * GET /v1/units?dc=texas,california&status=active,deploying&ns=devel
     *
     * List all the units given a list of datacenters and namespaces. Filter by deployment status
     * ns is required
     * dc is optional and if empty will query all datacenters
     * status is optional and if empty will filter by all DeploymentStatus
     */
    case GET -> Root / "v1" / "units" :? Ns(ns) +& Status(s) +& Dc(dc) & IsAuthenticated(_) =>
      val namespace = commaSeparatedStringToNamespace(ns)
      val datacenters = dc.map(commaSeparatedStringToList).getOrElse(Nil)
      val statuses = s.flatMap(commaSeparatedStringToStatus(_).toNel).getOrElse(DeploymentStatus.nel)
      namespace.toNel.toRight("This endpoint requires a non-empty 'ns' parameter.")
        .fold(
          e => BadRequest(e),
          ns => ns.sequence.fold(
            e => BadRequest(e.getMessage),
            n => json(Nelson.listUnitsByStatus(datacenters, n, statuses)))
        )

    /*
     * POST /v1/units/deprecate
     *
     * Deprecates all of the deployments given a service and feature version
     * accross all datacenters and namespaces
     */
    case req @ POST -> Root / "v1" / "units" / "deprecate" & IsAuthenticated(_) =>
      decode[Datacenter.ServiceName](req) { service =>
        json(Nelson.deprecateService(service))
      }

    /*
     * POST /v1/units/expire
     *
     * Expires all of the deployments given a service and feature version
     * accross all datacenters and namespaces.
     * Note this does not guarurtee a deployment will be cleaned up as the
     * expiration policy for the deployment will still run.
     */
    case req @ POST -> Root / "v1" / "units" / "expire" & IsAuthenticated(_) =>
      decode[Datacenter.ServiceName](req) { service =>
        json(Nelson.expireService(service))
      }

    /*
     * POST /v1/units/commit
     *
     * {
     *   "unit": "unit-name",
     *   "version": "1.2.40",
     *   "target": "prod"
     * }
     *
     * commits a unit / version to the specified namespace target
     */
    case req @ POST -> Root / "v1" / "units" / "commit" & IsAuthenticated(_) =>
      decode[Nelson.CommitUnit](req) { commit =>
        json(Nelson.commit(commit.unitName, commit.version, commit.target))
      }
  }
}
