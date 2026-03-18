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
package vault
package http4s

import io.circe.{Decoder, DecodingFailure, Encoder, Json}
import io.circe.syntax._

import scala.collection.immutable.SortedMap

trait Json {
  import Vault._

  given Encoder[Rule] = Encoder.instance(_ => Json.obj())

  given Encoder[CreatePolicy] = Encoder.instance { cp =>
    def capabilitiesOpt(rule: Rule): Option[List[String]] = rule.capabilities match {
      case Nil => None
      case cs  => Some(cs)
    }

    def ruleJson(rule: Rule): (String, Json) = {
      val fields = List(
        rule.policy.map(p => "policy" -> Json.fromString(p)),
        capabilitiesOpt(rule).map(cs => "capabilities" -> cs.asJson)
      ).flatten
      rule.path -> Json.obj(fields*)
    }

    val pathObj = Json.obj(cp.rules.map(ruleJson)*)
    val innerJson = Json.obj("path" -> pathObj).noSpaces
    Json.obj("policy" -> Json.fromString(innerJson))
  }

  given Decoder[Initialized] = Decoder.instance { c =>
    c.downField("initialized").as[Boolean].map(Initialized.apply)
  }

  def jsonMount(path: String): Decoder[Mount] = Decoder.instance { c =>
    for {
      defaultLease <- c.downField("config").downField("default_lease_ttl").as[Int]
      maxLease     <- c.downField("config").downField("max_lease_ttl").as[Int]
      tipe         <- c.downField("type").as[String]
      desc         <- c.downField("description").as[String]
    } yield Mount(path, tipe, desc, defaultLease, maxLease)
  }

  given Decoder[SortedMap[String, Mount]] = Decoder.instance { c =>
    c.value.asObject match {
      case None =>
        Left(DecodingFailure("expected mounts to be a JsonObject", c.history))
      case Some(obj) =>
        obj.toList
          .filter(_._1.endsWith("/"))
          .foldLeft[Decoder.Result[SortedMap[String, Mount]]](Right(SortedMap.empty)) {
            case (Right(acc), (key, json)) =>
              jsonMount(key).decodeJson(json).map(m => acc + (key -> m))
            case (left, _) => left
          }
    }
  }

  given Decoder[RootToken] = Decoder[String].map(RootToken.apply)

  given Decoder[InitialCreds] = Decoder.instance { c =>
    for {
      k <- c.downField("keys").as[List[MasterKey]]
      t <- c.downField("root_token").as[RootToken]
    } yield InitialCreds(k, t)
  }

  given Encoder[Initialization] =
    Encoder.forProduct2("secret_shares", "secret_threshold")(i => (i.secretShares, i.secretThreshold))

  given Decoder[Initialization] =
    Decoder.forProduct2("secret_shares", "secret_threshold")(Initialization.apply)

  // "n" = total, "t" = quorum per vault API
  given Decoder[SealStatus] =
    Decoder.forProduct4("sealed", "n", "t", "progress")(SealStatus.apply)

  val jsonUnseal: Encoder[String] = Encoder.instance { s =>
    Json.obj("key" -> Json.fromString(s))
  }

  given Encoder[CreateToken] = Encoder.instance { ct =>
    val fields = List(
      ct.policies.map(p => "policies" -> p.asJson),
      Some("renewable" -> Json.fromBoolean(ct.renewable)),
      ct.ttl.map(d => "ttl" -> Json.fromString(s"${d.toMillis}ms")),
      Some("num_uses" -> Json.fromLong(ct.numUses))
    ).flatten
    Json.obj(fields*)
  }

  given Encoder[CreateKubernetesRole] = Encoder.instance { kr =>
    val fields = List(
      Some("bound_service_account_names" -> kr.serviceAccountNames.asJson),
      Some("bound_service_account_namespaces" -> kr.seviceAccountNamespaces.asJson),
      kr.defaultLeaseTTL.map(d => "ttl" -> Json.fromString(s"${d.toMillis}ms")),
      kr.maxLeaseTTL.map(d => "max_ttl" -> Json.fromString(s"${d.toMillis}ms")),
      kr.policies.map(p => "policies" -> p.asJson)
    ).flatten
    Json.obj(fields*)
  }

  given Encoder[CreatePKIRole] = Encoder.instance { cpkir =>
    val fields = List(
      Some("name" -> cpkir.serviceAccountNames.asJson),
      cpkir.defaultLeaseTTL.map(d => "ttl" -> Json.fromString(s"${d.toMillis}ms")),
      cpkir.maxLeaseTTL.map(d => "max_ttl" -> Json.fromString(s"${d.toMillis}ms")),
      Some("allow_localhost" -> Json.fromBoolean(cpkir.allowLocalhost))
    ).flatten
    Json.obj(fields*)
  }
}
