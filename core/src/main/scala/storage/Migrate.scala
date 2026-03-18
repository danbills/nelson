package nelson
package storage

import cats.effect.IO
import org.flywaydb.core.Flyway
import journal.Logger

object Migrate {

  val log = Logger[Migrate.type]

  def migrate(cfg: DatabaseConfig): IO[Unit] =
    IO {
      val flyway = Flyway.configure()
        .dataSource(
          cfg.connection,
          cfg.username.getOrElse(""),
          cfg.password.getOrElse(""))
        .load()

      try {
        log.info("Conducting database schema migrations if needed.")
        val result = flyway.migrate()
        log.info(s"Completed ${result.migrationsExecuted} succsessful migrations.")
      } catch {
        case e: Throwable =>
          // attempt a repair (useful for local debugging)
          log.error(s"Failed to migrate database. ${e.getMessage}")
          log.info("Repairing database before retrying migration")
          flyway.repair()
          val result = flyway.migrate()
          log.info(s"After repair, completed ${result.migrationsExecuted} succsessful migrations.")
      }
    }
}
