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
package routing

import helm.ConsulOp
import cats.Foldable
import cats.data.NonEmptyList
import cats.implicits._
import scala.collection.immutable.SortedMap
import journal._

import io.circe.{Encoder, Decoder, Json}
import io.circe.syntax._

object Discovery {

  val log: Logger = Logger[Discovery.type]

  val discoveryKeyPrefix = "lighthouse/discovery/v1/"
  val DiscoveryKeyPattern = s"""$discoveryKeyPrefix(.*)""".r

  import Datacenter._
  import NamespaceName._
  import NamedService._

  final case class DeploymentDiscovery(defaultNamespace: NamespaceName,
                                 domain: String,
                                 namespaces: DiscoveryTables)

  given Encoder[RoutePath] = Encoder.instance { rp =>
    Json.obj(
      "stack"    -> rp.stack.stackName.toString.asJson,
      "port"     -> rp.port.asJson,
      "protocol" -> rp.protocol.asJson,
      "weight"   -> rp.weight.asJson
    )
  }

  given Encoder[Version] = Encoder[String].contramap(_.toString)
  given Decoder[Version] = Decoder[String].emap(s =>
    Version.fromString(s).toRight(s"Invalid version: $s")
  )

  given Encoder[StackName] = Encoder.forProduct3("serviceType", "version", "hash")(
    sn => (sn.serviceType, sn.version, sn.hash)
  )
  given Decoder[StackName] = Decoder.forProduct3("serviceType", "version", "hash")(StackName.apply)

  given Encoder[DiscoveryTables] = Encoder.instance { rt =>
    rt.foldLeft(Json.arr()) { case (a, (k, v)) =>
      val routes = v.foldLeft(Json.arr()) { case (a, (k, v)) =>
        val r = Json.obj(
          "service" -> k.serviceType.asJson,
          "targets" -> v.toList.asJson,
          "port"    -> v.head.j.portName.asJson
        )
        Json.arr((r +: a.asArray.getOrElse(Vector.empty))*)
      }
      val ns = Json.obj("name" -> k.asString.asJson, "routes" -> routes)
      Json.arr((ns +: a.asArray.getOrElse(Vector.empty))*)
    }
  }

  given Encoder[DeploymentDiscovery] = Encoder.instance { dd =>
    Json.obj(
      "defaultNamespace" -> dd.defaultNamespace.asString.asJson,
      "domain"           -> dd.domain.asJson,
      "namespaces"       -> dd.namespaces.asJson
    )
  }

  def discoveryTables[F[_]: Foldable](graphs: F[(Namespace, RoutingGraph)]): SortedMap[(StackName,NamespaceName), DiscoveryTables] = {
    graphs.foldLeft[SortedMap[(StackName,NamespaceName), DiscoveryTables]](SortedMap.empty){(smap,g) =>
      val (ns, rg) = g
      rg.nodes.filter(_.nsid == ns.id).foldLeft(smap){(s,rn) =>
        s + (((rn.stackName, ns.name), discoveryTable(rn, rg)))
      }
    }
  }

  def discoveryTable(rn: RoutingNode, rg: RoutingGraph): DiscoveryTables = {
    val context = rg.decomp(rn).ctx.yolo(s"discoveryTables: no ctx after decomposing ${rn.stackName}")
    context.outEdges.foldLeft[DiscoveryTables](SortedMap.empty)((m,e) =>
      e.to.deployment.fold[DiscoveryTables](SortedMap.empty){ to =>
        val path = e.label
        val service = NamedService(to.unit.serviceName.serviceType, path.portName)
        m |+| SortedMap(to.namespace.name -> SortedMap(service -> NonEmptyList.of(path)))
      }
    )
  }

  def writeDiscoveryInfoToConsul(ns: NamespaceName, sn: StackName, domain: String, dt: DiscoveryTables): ConsulOp.ConsulOpF[Unit] =
    ConsulOp.kvSetJson(consulDiscoveryKey(sn), DeploymentDiscovery(ns, domain, dt))

  def listDiscoveryKeys: ConsulOp.ConsulOpF[Set[String]] = ConsulOp.kvListKeys(discoveryKeyPrefix)

  def stackNameFrom(discoveryKey: String): Option[String] = discoveryKey match {
    case DiscoveryKeyPattern(s) if StackName.parsePublic(s).isDefined => Some(s)
    case _ => None
  }

  def consulDiscoveryKey(sn: StackName): String =  discoveryKeyPrefix + sn.toString
}
