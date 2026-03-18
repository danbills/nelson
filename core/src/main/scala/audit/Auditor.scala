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
package audit

import nelson.storage.StoreOp

import cats.~>
import cats.effect.IO
import cats.effect.std.Queue
import cats.implicits._

import fs2.{Pipe, Stream}

import journal.Logger

class Auditor(queue: Queue[IO, AuditEvent[?]], defaultLogin: String) {

  private[this] val logger = Logger[Auditor]

  @SuppressWarnings(Array("org.brianmckenna.wartremover.warts.IsInstanceOf"))
  private def logPipe: Pipe[IO, AuditEvent[?], Nothing] =
    _.evalMap {
      case AuditEvent(t: Throwable, _, _, _, login, _) =>
        IO(logger.error(s"[fatal] audit error event ${t.getMessage} by user ${login}"))
      case a =>
        IO(logger.info(s"[info] audit event ${a.event} action ${a.action} by user ${a.userLogin}"))
    }.drain

  private def persistPipe(stg: StoreOp ~> IO): Pipe[IO, AuditEvent[?], Nothing] =
    _.evalMap { a =>
      storage.StoreOp.audit(a).void.foldMap(stg).recoverWith {
        case t => IO(logger.error(s"[fatal] audit error while persisting event ${t.getMessage}"))
      }
    }.drain

  def auditSink[A](action: AuditAction)(using au: Auditable[A]): Pipe[IO, A, Nothing] =
    _.evalMap(a => write(a, action)(using au)).drain

  def errorSink: Pipe[IO, Throwable, Nothing] =
    _.evalMap(t => IO(logger.error(t.getMessage))).drain

  def write[A](a: A, action: AuditAction, releaseId: Option[Long] = None, login: String = defaultLogin)(using au: Auditable[A]): IO[Unit] =
    queue.offer(AuditEvent(a, action, releaseId, login))

  def process(stg: StoreOp ~> IO): Stream[IO, Unit] =
    Stream.fromQueueUnterminated(queue)
      .observe(persistPipe(stg))
      .attempt
      .collect { case Right(a) => a }
      .through(logPipe)
}
