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

import io.circe.{Json => CJson}
import io.circe.syntax._

import org.scalatest._

class NomadJsonSpec extends FlatSpec with Matchers with Inspectors {
  import Manifest.{Ports,Port}
  import scala.concurrent.duration._
  import nelson.scheduler.NomadJson
  import nelson.docker.Docker.Image
  import nelson.Manifest._

  val nomad = Infrastructure.Nomad(
    org.http4s.Uri.uri("http://endpoint:8080"),
    1.second, "user", "pass", "addy",
    2300
  )

  val image = Image("image", None, None)

  val env = Environment(
    bindings = List(EnvironmentVariable("NELSON_STACKNAME", "stackname"), EnvironmentVariable("NELSON_DATACENTER", "dc1")),
    cpu = ResourceSpec.limitOnly(2.0).get,
    memory = ResourceSpec.limitOnly(512.0).get)

  val ports = Ports(Port("http",8080,"http"), Nil)

  it should "generate docker config json with ports" in {
    val json = NomadJson.dockerConfigJson(nomad, image, Some(ports), NomadJson.BridgeMode)

    json should equal (CJson.obj(
      "port_map"     -> List(CJson.obj("http" -> 8080.asJson)).asJson,
      "image"        -> "https://image".asJson,
      "network_mode" -> "bridge".asJson,
      "auth"         -> List(CJson.obj(
        "username"       -> "user".asJson,
        "password"       -> "pass".asJson,
        "server_address" -> "addy".asJson,
        "SSL"            -> true.asJson
      )).asJson
    ))
  }

  it should "generate docker config json without ports" in {
    val json = NomadJson.dockerConfigJson(nomad, image, None, NomadJson.BridgeMode)
    json should equal (CJson.obj(
      "image"        -> "https://image".asJson,
      "network_mode" -> "bridge".asJson,
      "auth"         -> List(CJson.obj(
        "username"       -> "user".asJson,
        "password"       -> "pass".asJson,
        "server_address" -> "addy".asJson,
        "SSL"            -> true.asJson
      )).asJson
    ))
  }

  it should "generate resources json with ports" in {
    val json = NomadJson.resourcesJson(4000000, 512, Some(ports))
    json should equal (CJson.obj(
      "CPU"      -> 4000000.asJson,
      "MemoryMB" -> 512.asJson,
      "IOPS"     -> 0.asJson,
      "Networks" -> List(CJson.obj(
        "mbits" -> 1.asJson,
        "DynamicPorts" -> List(CJson.obj(
            "Label" -> "http".asJson,
            "Value" -> 0.asJson
        )).asJson
      )).asJson
    ))
  }

  it should "generate resources json without ports" in {
    val json = NomadJson.resourcesJson(4000000, 512, None)
    json should equal (CJson.obj(
      "CPU"      -> 4000000.asJson,
      "MemoryMB" -> 512.asJson,
      "IOPS"     -> 0.asJson,
      "Networks" -> List(CJson.obj("mbits" -> 1.asJson)).asJson
    ))
  }

  it should "generate environment json" in {
    val json = NomadJson.envJson(env.bindings)
    json should equal (CJson.obj(
      "NELSON_STACKNAME"  -> "stackname".asJson,
      "NELSON_DATACENTER" -> "dc1".asJson
    ))
  }

  it should "generate services json" in {
    val json = NomadJson.servicesJson("name", ports.default, Set("tag1", "tag2"), Nil)
    json should equal (CJson.obj(
      "Name"      -> "name".asJson,
      "PortLabel" -> "http".asJson,
      "Tags"      -> List("tag1","tag2").asJson,
      "Checks"    -> List(CJson.obj(
        "Name"         -> "tcp http name".asJson,
        "Type"         -> "tcp".asJson,
        "PortLabel"    -> "http".asJson,
        "Args"         -> CJson.Null,
        "Command"      -> "".asJson,
        "Id"           -> "".asJson,
        "Path"         -> "".asJson,
        "Protocol"     -> CJson.Null,
        "Interval"     -> 10000000000L.asJson,
        "Timeout"      -> 4000000000L.asJson,
        "TLSSkipVerify" -> false.asJson
      )).asJson
    ))
  }

  it should "generate log json" in {
    val json = NomadJson.logJson(10,10)
    json should equal (CJson.obj(
      "MaxFiles"      -> 10.asJson,
      "MaxFileSizeMB" -> 10.asJson
    ))
  }

  it should "genrate periodic json" in {
    val json = NomadJson.periodicJson("* * * 24")
    json should equal (CJson.obj(
      "Spec"            -> "* * * 24".asJson,
      "Enabled"         -> true.asJson,
      "SpecType"        -> "cron".asJson,
      "ProhibitOverlap" -> true.asJson
    ))
  }

  it should "generate restart json" in {
    val json = NomadJson.restartJson(3)
    json should equal(CJson.obj(
      "Interval" -> 5.minutes.toNanos.asJson,
      "Attempts" -> 3.asJson,
      "Delay"    -> 15.seconds.toNanos.asJson,
      "Mode"     -> "delay".asJson
    ))
  }

  it should "generate empheral disk json" in {
    val json = NomadJson.ephemeralDiskJson(false,false,3)
    json should equal(CJson.obj(
      "Sticky"  -> false.asJson,
      "Migrate" -> false.asJson,
      "SizeMB"  -> 3.asJson
    ))
  }

  it should "generate task json with ports defined" in {
    val json = NomadJson.leaderTaskJson("name--1-0-0--abcdef12", image, env, NomadJson.BridgeMode, Some(ports), nomad, NamespaceName("qa"), "default", Set("required-tag1","required-tag2"))
    json should equal(CJson.obj(
      "Name"   -> "name--1-0-0--abcdef12".asJson,
      "Driver" -> "docker".asJson,
      "Services" -> List(CJson.obj(
        "Name"      -> "name--1-0-0--abcdef12".asJson,
        "PortLabel" -> "http".asJson,
        "Tags"      -> Set("qa","port--http","plan--default","required-tag1","required-tag2").asJson,
        "Checks"    -> List(CJson.obj(
          "Name"          -> "tcp http name--1-0-0--abcdef12".asJson,
          "Type"          -> "tcp".asJson,
          "PortLabel"     -> "http".asJson,
          "Args"          -> CJson.Null,
          "Command"       -> "".asJson,
          "Id"            -> "".asJson,
          "Path"          -> "".asJson,
          "Protocol"      -> CJson.Null,
          "Interval"      -> 10000000000L.asJson,
          "Timeout"       -> 4000000000L.asJson,
          "TLSSkipVerify" -> false.asJson
        )).asJson
      )).asJson,
      "leader" -> true.asJson,
      "Config" -> CJson.obj(
        "image"        -> "https://image".asJson,
        "network_mode" -> "bridge".asJson,
        "port_map"     -> List(CJson.obj("http" -> 8080.asJson)).asJson,
        "auth"         -> List(CJson.obj(
          "username"       -> "user".asJson,
          "password"       -> "pass".asJson,
          "server_address" -> "addy".asJson,
          "SSL"            -> true.asJson
         )).asJson
      ),
      "Vault" -> CJson.obj(
        "ChangeSignal" -> "".asJson,
        "ChangeMode"   -> "restart".asJson,
        "Env"          -> true.asJson,
        "Policies"     -> List("nelson__qa__name--1-0-0--abcdef12").asJson
      ),
      "Env" -> CJson.obj(
        "NELSON_STACKNAME"  -> "stackname".asJson,
        "NELSON_DATACENTER" -> "dc1".asJson
      ),
      "Resources" -> CJson.obj(
        "CPU"      -> 4600.asJson,
        "MemoryMB" -> 512.asJson,
        "IOPS"     -> 0.asJson,
        "Networks" -> List(CJson.obj(
          "mbits"        -> 1.asJson,
          "DynamicPorts" -> List(CJson.obj(
            "Label" -> "http".asJson,
            "Value" -> 0.asJson
          )).asJson
        )).asJson
      ),
      "LogConfig" -> CJson.obj(
        "MaxFiles"      -> 10.asJson,
        "MaxFileSizeMB" -> 10.asJson
      )
    ))
  }

  it should "generate task json without ports defined" in {
    val json = NomadJson.leaderTaskJson("name--1-0-0--abcdef12", image, env, NomadJson.HostMode, None, nomad, NamespaceName("qa"), "default", Set("required-tag1","required-tag2"))
    json should equal(CJson.obj(
      "Name"   -> "name--1-0-0--abcdef12".asJson,
      "Driver" -> "docker".asJson,
      "leader" -> true.asJson,
      "Config" -> CJson.obj(
        "image"        -> "https://image".asJson,
        "network_mode" -> "host".asJson,
        "auth"         -> List(CJson.obj(
          "username"       -> "user".asJson,
          "password"       -> "pass".asJson,
          "server_address" -> "addy".asJson,
          "SSL"            -> true.asJson
        )).asJson
      ),
      "Vault" -> CJson.obj(
        "ChangeSignal" -> "".asJson,
        "ChangeMode"   -> "restart".asJson,
        "Env"          -> true.asJson,
        "Policies"     -> List("nelson__qa__name--1-0-0--abcdef12").asJson
      ),
      "Env" -> CJson.obj(
        "NELSON_STACKNAME"  -> "stackname".asJson,
        "NELSON_DATACENTER" -> "dc1".asJson
      ),
      "Resources" -> CJson.obj(
        "CPU"      -> 4600.asJson,
        "MemoryMB" -> 512.asJson,
        "IOPS"     -> 0.asJson,
        "Networks" -> List(CJson.obj("mbits" -> 1.asJson)).asJson
      ),
      "LogConfig" -> CJson.obj(
        "MaxFiles"      -> 10.asJson,
        "MaxFileSizeMB" -> 10.asJson
      )
    ))
  }
}
