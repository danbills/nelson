package nelson
package storage

import doobie.hikari._
import cats.effect.{IO, Resource}
import cats.effect.unsafe.implicits.global

object Hikari {

  def build(db: DatabaseConfig): HikariTransactor[IO] = {
    val trans: Resource[IO, HikariTransactor[IO]] =
      HikariTransactor.newHikariTransactor[IO](
        db.driver, db.connection,
        db.username.getOrElse(""), db.password.getOrElse("")
      ).evalTap(xa => xa.configure(hx => IO(db.maxConnections.foreach(max => hx.setMaximumPoolSize(max)))))

    // Allocate the transactor without releasing it: it lives for the app lifetime.
    trans.allocated.unsafeRunSync()._1
  }
}
