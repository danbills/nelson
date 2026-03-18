enablePlugins(BuildInfoPlugin)

libraryDependencies ++= Seq(
  "org.typelevel"      %% "cats-core"                               % V.cats,
  "org.typelevel"      %% "cats-effect"                             % V.catsEffect,
  "org.typelevel"      %% "cats-free"                               % V.cats,
  "co.fs2"             %% "fs2-core"                                % V.fs2,
  "co.fs2"             %% "fs2-io"                                  % V.fs2,
  "co.fs2"             %% "fs2-scodec"                              % V.fs2,
  "io.github.iltotore" %% "iron"                                    % V.iron,
  "io.github.iltotore" %% "iron-cats"                               % V.iron,
  "io.circe"           %% "circe-core"                              % V.circe,
  "io.circe"           %% "circe-generic"                           % V.circe,
  "io.circe"           %% "circe-parser"                            % V.circe,
  "org.typelevel"      %% "log4cats-core"                           % V.log4cats,
  "org.typelevel"      %% "log4cats-slf4j"                          % V.log4cats,
  "org.http4s"         %% "http4s-client"                           % V.http4s,
  "org.http4s"         %% "http4s-circe"                            % V.http4s,
  "org.http4s"         %% "http4s-ember-client"                     % V.http4s,
  "org.tpolecat"       %% "doobie-core"                             % V.doobie,
  "org.tpolecat"       %% "doobie-h2"                               % V.doobie,
  "org.tpolecat"       %% "doobie-hikari"                           % V.doobie,
  "org.scodec"         %% "scodec-core"                             % V.scodec,
  "org.typelevel"      %% "spire"                                   % V.spire,
  "org.flywaydb"        % "flyway-core"                             % "10.15.0",
  "org.yaml"            % "snakeyaml"                               % "2.2",
  "org.scalatra.scalate" %% "scalate-core"                          % "1.10.1",
  "io.prometheus"       % "prometheus-metrics-core"                 % V.prometheus,
  "io.prometheus"       % "prometheus-metrics-instrumentation-jvm"  % V.prometheus,
  "com.cronutils"       % "cron-utils"                              % "9.2.1",
  "org.apache.commons"  % "commons-email"                           % "1.6.0",
  "commons-codec"       % "commons-codec"                           % "1.17.1",
  "com.amazonaws"       % "aws-java-sdk-autoscaling"                % "1.12.772",
  "com.amazonaws"       % "aws-java-sdk-elasticloadbalancingv2"     % "1.12.772",
  "com.google.guava"    % "guava"                                   % "33.2.1-jre",
  "com.google.code.findbugs" % "jsr305"                            % "3.0.2",
  "org.typelevel"      %% "cats-laws"                               % V.cats       % Test,
  "org.scalatest"      %% "scalatest"                               % V.scalaTest  % Test,
  "org.scalacheck"     %% "scalacheck"                              % V.scalaCheck % Test,
)

buildInfoPackage := "nelson"

scalacOptions ++= List(
  "-Wvalue-discard",
  "-source:future",
  "-language:implicitConversions",
)

scalacOptions in (Compile, doc) ++= Seq("-no-link-warnings")
