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

import io.circe.{Encoder, Json}
import io.circe.syntax._
import Datacenter.Deployment


/** Enumerates all the known Auditable categories and is used to provides context about the Auditable. */
sealed trait AuditCategory extends Product with Serializable
case object GithubReleaseCategory extends AuditCategory
case object GithubWebHookCategory extends AuditCategory
case object GithubDeploymentCategory extends AuditCategory
case object GithubRepoCategory extends AuditCategory
case object ManualDeploymentCategory extends AuditCategory
case object DeploymentCategory extends AuditCategory
case object ErrorCategory extends AuditCategory
case object InfoCategory extends AuditCategory

object AuditCategory {
  def stringify(cat: AuditCategory) = cat match {
    case GithubReleaseCategory    => "release"
    case GithubDeploymentCategory => "deployment"
    case GithubWebHookCategory    => "hook"
    case GithubRepoCategory       => "repo"
    case ManualDeploymentCategory => "manual_deploy"
    case DeploymentCategory       => "deploy"
    case ErrorCategory            => "error"
    case InfoCategory             => "info"
  }
}

/** Enumerates all the actions that can to applied to an Auditable */
sealed trait AuditAction extends Product with Serializable
case object CreateAction extends AuditAction
case object DeleteAction extends AuditAction
case object UpdateAction extends AuditAction
case object DeprecateAction extends AuditAction
case object GarbageAction extends AuditAction
case object ReadyAction extends AuditAction
case object LoggingAction extends AuditAction

object AuditAction {
  def stringify(action: AuditAction) = action match {
    case CreateAction    => "create"
    case DeleteAction    => "delete"
    case UpdateAction    => "update"
    case DeprecateAction => "deprecate"
    case GarbageAction   => "garbage"
    case ReadyAction     => "ready"
    case LoggingAction   => "logging"
  }
}

final case class AuditContext(action: AuditAction, category: AuditCategory) {
  def stringify = s"${AuditAction.stringify(action)}.${AuditCategory.stringify(category)}"
}

/** Represents an `event` that we wish to audit.
  * Because we are storing the events in a persistent store an encoding is necessary.
  * Json was chosen because the shape of events varies and because most
  * events already have a json encoder available.
  * The category provides context outside of the json blob concerning the `event`.
  * the category is useful from a querying perspective.
  */
trait Auditable[A] {
  def encode(a: A): io.circe.Json
  def category: AuditCategory
}

object AuditableInstances {

  given [A <: NelsonError]: Auditable[A] with {
    def encode(error: A): Json = nelson.Json.NelsonErrorEncoder[NelsonError].apply(error)
    def category = ErrorCategory
  }

  given Auditable[Github.Release] with {
    def encode(a: Github.Release): Json = a.asJson(using nelson.Json.GithubReleaseEncoder)
    def category = GithubReleaseCategory
  }

  given Auditable[Github.Deployment] with {
    def encode(a: Github.Deployment): Json = a.asJson(using nelson.Json.GithubDeploymentEncoder)
    def category = GithubDeploymentCategory
  }

  given Auditable[Github.WebHook] with {
    def encode(a: Github.WebHook): Json = a.asJson(using nelson.Json.GithubWebHookEncoder)
    def category = GithubReleaseCategory
  }

  given Auditable[Datacenter.ManualDeployment] with {
    def encode(a: Datacenter.ManualDeployment): Json = a.asJson(using nelson.Json.ManualDeploymentEncoder)
    def category = ManualDeploymentCategory
  }

  given Auditable[Repo] with {
    def encode(a: Repo): Json = a.asJson(using nelson.Json.RepoEncoder)
    def category = GithubRepoCategory
  }

  given Auditable[Hook] with {
    def encode(a: Hook): Json = a.asJson(using nelson.Json.HookEncoder)
    def category = GithubWebHookCategory
  }

  given Auditable[String] with {
    def encode(s: String): Json = Json.fromString(s)
    def category = InfoCategory
  }

  given Auditable[NelsonError] with {
    def encode(error: NelsonError): Json = nelson.Json.NelsonErrorEncoder[NelsonError].apply(error)
    def category = ErrorCategory
  }

  given Auditable[Session] with {
    def encode(s: Session): Json = s.asJson(using nelson.Json.SessionEncoder)
    def category = InfoCategory
  }

  given Auditable[Deployment] with {
    def encode(d: Deployment): Json = d.asJson(using nelson.Json.DeploymentEncoder)
    def category = DeploymentCategory
  }
}
