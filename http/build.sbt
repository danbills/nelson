import sbt._, Keys._
import com.typesafe.sbt.packager.archetypes._
import com.typesafe.sbt.packager.docker._

enablePlugins(AshScriptPlugin, JavaAppPackaging, DockerPlugin)

scalacOptions ++= List(
  "-Wvalue-discard",
  "-source:future",
  "-language:implicitConversions",
)

Docker / packageName := "getnelson/nelson"

Docker / version := version.value

Docker / daemonUser := "root"

Docker / defaultLinuxInstallLocation := "/opt/application"

dockerUpdateLatest := true

dockerExposedPorts := Seq(9000, 5775)

dockerBaseImage := "eclipse-temurin:21-jre-alpine"

publishLocal := (Docker / publishLocal).value

publish := (Docker / publish).value

releasePublishArtifactsAction := publish.value

custom.resources

custom.revolver

coverageMinimum := 20

libraryDependencies ++= Seq(
  "org.http4s"   %% "http4s-circe"         % V.http4s,
  "org.http4s"   %% "http4s-dsl"           % V.http4s,
  "org.http4s"   %% "http4s-ember-server"  % V.http4s,
  "io.prometheus" % "prometheus-metrics-exposition-httpserver" % V.prometheus,
  "org.scalatest"  %% "scalatest"   % V.scalaTest  % Test,
  "org.scalacheck" %% "scalacheck"  % V.scalaCheck % Test,
)

run / mainClass := Some("nelson.Main")

val kubectlVersion = SettingKey[String]("kubectl-version", "The version of kubectl to install")
kubectlVersion := sys.env.getOrElse("KUBECTL_VERSION", "1.30.0")

val prometheusVersion = SettingKey[String]("prometheus-version", "The version of Prometheus to install")
prometheusVersion := sys.env.getOrElse("PROMETHEUS_VERSION", "2.53.0")

dockerCommands ++= Seq(
  ExecCmd("RUN", "addgroup", "nelson"),
  ExecCmd("RUN", "adduser", "-s", "/bin/false", "-u", "2000", "-G", "nelson", "-S", "-D", "-H", "nelson"),
  ExecCmd("RUN", "ln", "-s", s"${(Docker / defaultLinuxInstallLocation).value}/bin/${normalizedName.value}", "/usr/local/bin/sbt"),
  ExecCmd("RUN", "chmod", "555", s"${(Docker / defaultLinuxInstallLocation).value}/bin/${normalizedName.value}"),
  ExecCmd("RUN", "chown", "-R", "nelson:nelson", s"${(Docker / defaultLinuxInstallLocation).value}"),
  ExecCmd("RUN", "apk", "add", "--update-cache", "bash", "graphviz", "wget", "libc6-compat", "docker")
)

dockerCommands ++= Seq(
  ExecCmd("RUN", "wget", "-nv", "--retry-connrefused", "--waitretry", "1", "--read-timeout", "10", "--timeout", "15", "-t", "5",
    s"https://dl.k8s.io/release/v${kubectlVersion.value}/bin/linux/amd64/kubectl", "-P", "/usr/local/bin"),
  ExecCmd("RUN", "chmod", "+x", "/usr/local/bin/kubectl")
)

dockerCommands ++= {
  val prometheusBase = s"prometheus-${prometheusVersion.value}.linux-amd64"
  Seq(
    ExecCmd("RUN", "wget", "-nv", "--retry-connrefused", "--waitretry", "1", "--read-timeout", "10", "--timeout", "15", "-t", "5",
      s"https://github.com/prometheus/prometheus/releases/download/v${prometheusVersion.value}/${prometheusBase}.tar.gz", "-P", "/tmp"),
    ExecCmd("RUN", "tar", "xzf", s"/tmp/${prometheusBase}.tar.gz", "-C", "/tmp"),
    ExecCmd("RUN", "cp", s"/tmp/${prometheusBase}/promtool", "/usr/local/bin"),
    ExecCmd("RUN", "rm", "-rf", s"/tmp/${prometheusBase}", s"/tmp/${prometheusBase}.tar.gz")
  )
}

dockerCommands += Cmd("USER", "2000")
