addSbtPlugin("com.typesafe.sbt"  % "sbt-native-packager"  % "1.10.4")
addSbtPlugin("io.spray"          % "sbt-revolver"         % "0.10.0")
addSbtPlugin("org.xerial.sbt"    % "sbt-sonatype"         % "3.12.2")
addSbtPlugin("com.eed3si9n"      % "sbt-buildinfo"        % "0.12.0")

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.4.14"
