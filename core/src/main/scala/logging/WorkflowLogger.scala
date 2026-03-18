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
package logging

import cats.~>
import cats.effect.IO
import cats.effect.std.Queue
import cats.syntax.apply._

import fs2.Stream
import fs2.io.file.{Files, Path => FPath}
import fs2.text

import journal.Logger

import java.nio.file.{Path, Files => JFiles, StandardOpenOption}
import java.time.Instant

/*
 * The WorkflowLogger is a process that logs workflow deployment progress
 * information to a namespaced file (based on deployment id). The main
 * purpose of this file is to provide the frontend with insight into
 * what is happening with this deployment, similar to what travis provides.
 */
class WorkflowLogger(queue: Queue[IO, (ID, String)], base: Path) extends (LoggingOp ~> IO) {

  private val logger = Logger[this.type]

  import LoggingOp._
  def apply[A](op: LoggingOp[A]) =
    op match {
      case Debug(msg: String) =>
        IO(logger.debug(msg))
      case Info(msg: String) =>
        IO(logger.info(msg))
      case LogToFile(id, msg) =>
        log(id, msg)
    }

  def setup(): IO[Unit] =
    IO {
      if (!exists(base)) {
        logger.info(s"creating workflow log base directory at $base")
        JFiles.createDirectories(base)
        ()
      }
    }

  def log(id: ID, line: String): IO[Unit] =
    queue.offer((id, line))

  def process: Stream[IO, Unit] =
    Stream.fromQueueUnterminated(queue).through(appendToFile)

  def read(id: ID, offset: Int): IO[List[String]] =
    for {
      file <- getPath(id)
      lines <- Files[IO].readAll(FPath.fromNioPath(file))
                 .through(text.utf8.decode)
                 .through(text.lines)
                 .drop(offset.toLong)
                 .compile
                 .toList
    } yield lines

  private def appendToFile: fs2.Pipe[IO, (ID, String), Nothing] =
    _.evalMap { case (id, line) =>
      for {
        path <- getPath(id)
        _    <- createFile(path) *> append(path, line)
      } yield ()
    }.drain

  private def append(path: Path, line: String): IO[Unit] =
    IO {
      val withNewline = if (!line.endsWith("\n")) s"$line\n" else line
      val withTimestamp = s"$NOW: $withNewline"
      JFiles.write(path, withTimestamp.getBytes(), StandardOpenOption.APPEND)
      ()
    }

  private def getPath(id: ID): IO[Path] =
    IO(base.resolve(s"${id}.log"))

  private def createFile(path: Path): IO[Unit] =
    IO {
      if (!exists(path))
        JFiles.createFile(path)
      ()
    }

  private def exists(path: Path): Boolean =
    JFiles.exists(path)

  private def NOW = Instant.now
}
