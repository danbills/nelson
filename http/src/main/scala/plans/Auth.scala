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

import io.circe.{Encoder, Json}
import io.circe.syntax._
import org.http4s._
import org.http4s.circe._
import org.http4s.dsl.io._
import org.http4s.headers.{Location, `Set-Cookie`}
import cats.effect.IO
import cats.effect.unsafe.implicits.global

final case class Auth(config: NelsonConfig) extends Default {
  import nelson.Json.{*, given}
  private val cfg = config

  private given Encoder[(java.time.Instant, String)] =
    Encoder.instance { case (exp, enc) =>
      Json.obj(
        "expires_at"    -> exp.asJson,
        "session_token" -> enc.asJson
      )
    }

  val service: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case GET -> Root / "auth" / "login" =>
      if(cfg.security.useEnvironmentSession){
        Found(Location(uri("/auth/exchange?code=yolo")))
      } else {
        redirectToLogin
      }

    case GET -> Root / "auth" / "logout" =>
      Found(Location(uri("/"))).map(_.putHeaders(`Set-Cookie`(ResponseCookie(CookieName, "").clearCookie)))

    /*
     * used to exchange github token for a nelson token. This endpoint
     * is primarily intended for non-web applications to integrate with
     * the nelson API.
     * the body of the post should look like:
     * { "access_token": "<your github token>" }
     */
    case req @ POST -> Root / "auth" / "github" =>
      decode[AccessToken](req){ tk =>
        (for {
          a <- Nelson.createSessionFromGithubToken(tk)(cfg).attempt.unsafeRunSync()
          b <- cfg.security.authenticator.serialize(a)
        } yield (a.expiry, b)).fold(
          e => IO.raiseError(new RuntimeException(e.toString)),
          s => Ok(s.asJson)
        )
      }

    case req @ GET -> Root / "auth" / "exchange" =>
      val code = req.params.getOrElse("code", "unknown")
      (for {
        a <- Nelson.createSessionFromOAuthCode(code)(cfg).attempt.unsafeRunSync()
        b <- cfg.security.authenticator.serialize(a)
      } yield b).fold(
        e => IO.raiseError(new RuntimeException(e.toString)),
        s => {
          val cookie = ResponseCookie(CookieName, s,
            path     = Some("/"),
            domain   = Some(cfg.network.externalHost),
            secure   = cfg.network.tls,
            maxAge   = Some(cfg.security.expireLoginAfter.toSeconds.toLong),
            httpOnly = false // determines if js can read this cookie
          )
          Found(Location(uri("/"))).map(_.putHeaders(`Set-Cookie`(cookie)))
        }
      )

    case GET -> "v1" /: _ & NotAuthenticated() =>
      IO.pure(Response[IO](Status.Unauthorized).withEntity("Supplied authentication token is invalid."))
  }
}
