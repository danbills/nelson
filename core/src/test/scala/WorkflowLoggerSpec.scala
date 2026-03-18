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

import org.scalatest.{FlatSpec,Matchers,BeforeAndAfterAll,BeforeAndAfterEach}
import java.nio.file.Files

import cats.effect.IO
import cats.effect.std.Queue
import cats.effect.unsafe.implicits.global

import fs2.io.file.{Files => Fs2Files, Path => FPath}
import fs2.text

class WorkflowLoggerSpec extends FlatSpec with Matchers with BeforeAndAfterAll with BeforeAndAfterEach {

  val queue =
    Queue.bounded[IO, (ID,String)](10).unsafeRunSync()

  val base = Files.createTempDirectory("nelson")
  val logger = new logging.WorkflowLogger(queue, base)

  override def beforeAll: Unit = {
    logger.setup().unsafeRunSync()
  }

  override def beforeEach: Unit = {
    delete()
    ()
  }

  override def afterAll: Unit = {
    delete()
    ()
  }

  private def delete() = {
    import scala.util.Try
    import scala.jdk.CollectionConverters._
    Try(Files.newDirectoryStream(base)).map(stream =>
      stream.asScala.toIterator.foreach(file => scala.util.Try(Files.delete(file)))
    )
  }

  def removeTimestamp(line: String): String =
    line.split("Z:")(1).trim

  it should "generate a file with the correct name" in {
    logger.log(1L, "foo").unsafeRunSync()
    logger.process.take(1).compile.drain.unsafeRunSync()
    assert(Files.exists((base.resolve("1.log"))))
  }

  it should "log to the correct file base on id" in {
    val path = base.resolve("1.log")
    logger.log(1L, "foo").unsafeRunSync()
    logger.process.take(1).compile.drain.unsafeRunSync()
    val line = Fs2Files[IO].readAll(FPath.fromNioPath(path)).through(text.utf8.decode).through(text.lines).compile.toVector.unsafeRunSync()
    line.filter(_.nonEmpty).map(removeTimestamp) should equal (Vector("foo"))
  }

  it should "log with newlines to the file" in {
    val path = base.resolve("1.log")
    logger.log(1L, "foo").unsafeRunSync()
    logger.log(1L, "bar").unsafeRunSync()
    logger.process.take(2).compile.drain.unsafeRunSync()
    val lines = Fs2Files[IO].readAll(FPath.fromNioPath(path)).through(text.utf8.decode).through(text.lines).compile.toVector.unsafeRunSync()
    lines.filter(_.nonEmpty).map(removeTimestamp) should equal (Vector("foo","bar"))
  }

  it should "not add an extra newline" in {
    val path = base.resolve("1.log")
    logger.log(1L, "foo\n").unsafeRunSync()
    logger.log(1L, "bar").unsafeRunSync()
    logger.process.take(2).compile.drain.unsafeRunSync()
    val lines = Fs2Files[IO].readAll(FPath.fromNioPath(path)).through(text.utf8.decode).through(text.lines).compile.toVector.unsafeRunSync()
    lines.filter(_.nonEmpty).map(removeTimestamp) should equal (Vector("foo","bar"))
  }

  it should "read the file entirely" in {
    logger.log(1L, "foo\n").unsafeRunSync()
    logger.log(1L, "bar\n").unsafeRunSync()
    logger.log(1L, "baz\n").unsafeRunSync()
    logger.process.take(3).compile.drain.unsafeRunSync()
    val lines = logger.read(1L,0).unsafeRunSync()
    lines.filter(_.nonEmpty).map(removeTimestamp) should equal (Vector("foo","bar","baz"))
  }
  it should "read the file from offset" in {
    logger.log(1L, "foo\n").unsafeRunSync()
    logger.log(1L, "bar\n").unsafeRunSync()
    logger.log(1L, "baz\n").unsafeRunSync()
    logger.process.take(3).compile.drain.unsafeRunSync()
    val lines = logger.read(1L,2).unsafeRunSync()
    lines.filter(_.nonEmpty).map(removeTimestamp) should equal (Vector("baz"))
  }
}
