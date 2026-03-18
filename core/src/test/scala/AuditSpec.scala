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

import io.circe.{Encoder, Json}
import io.circe.syntax._

import cats.effect.IO
import cats.implicits._

import fs2.Stream

import doobie.implicits._

import org.scalatest._

class AuditSpec extends NelsonSuite with BeforeAndAfterEach {

  import nelson.Json.{*, given}
  import audit._

  case class Foo(n: Int)
  given Encoder[Foo] = Encoder.forProduct1("n")(_.n)

  case class Bar(n: Int)
  given Encoder[Bar] = Encoder.forProduct1("n")(_.n)

  val storage = TestStorage.storage("AuditSpec")
  val defaultSystemLogin = "nelson"

  given Auditable[Foo] with
    def encode(foo: Foo): Json = foo.asJson
    def category = InfoCategory

  given Auditable[Bar] with
    def encode(bar: Bar): Json = bar.asJson
    def category = DeploymentCategory

  val truncEvery = { sql"DELETE FROM audit_log".update.run }.void

  val setup = for { _ <- truncEvery.transact(storage.xa) } yield ()

  override def beforeEach: Unit = setup.unsafeRunSync()

  it should "enqueue all events in the stream" in {
    val audit = new Auditor(config.auditQueue,defaultSystemLogin)
    val p: Stream[IO, Foo] = Stream(Foo(1),Foo(2),Foo(3),Foo(10))

    p.observe(audit.auditSink(LoggingAction)).compile.drain.unsafeRunSync()

    val vec = audit.process(storage).take(4).compile.toVector.unsafeRunSync()

    vec.length should equal (vec.length)
  }

  it should "store auditable events in storage" in {
    val audit = new Auditor(config.auditQueue,defaultSystemLogin)
    val events = Vector(Foo(1),Foo(2),Foo(3),Foo(10))
    val p: Stream[IO, Foo] = Stream(events*)

    p.observe(audit.auditSink(LoggingAction)).compile.drain.unsafeRunSync()

    audit.process(storage).take(4).compile.toVector.unsafeRunSync()

    val ev = nelson.storage.StoreOp.listAuditLog(10, 0).foldMap(storage).unsafeRunSync()

    ev.length should equal (events.length)
  }

  it should "be able to write to audit log directly" in {
    val audit = new Auditor(config.auditQueue, defaultSystemLogin)
    val foo = Foo(1)

    audit.write(foo, CreateAction).unsafeRunSync()
    audit.write(foo, CreateAction).unsafeRunSync()

    audit.process(storage).take(2).compile.drain.unsafeRunSync()

    val ev = nelson.storage.StoreOp.listAuditLog(10, 0).foldMap(storage).unsafeRunSync()

    ev.length should equal (2)
  }
}
