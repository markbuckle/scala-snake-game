name := "scalafx-playground"
version := "0.1"
scalaVersion := "3.5.2"

// dependency on ScalaFX library
libraryDependencies ++= Seq(
  "org.scalafx" %% "scalafx" % "16.0.0-R25"
)

// Determine OS version of JavaFX binaries
lazy val osName = System.getProperty("os.name") match {
  case n if n.startsWith("Linux") => "linux"
  case n if n.startsWith("Windows") => "win"
  case n if n.startsWith("Mac") => "mac"
  case _ => throw new Exception("Unknown platform!")
}

// Add dependency on JavaFX libraries, OS dependent
lazy val javaFXModules = Seq("base", "controls", "fxml", "graphics", "media", "swing", "web")
libraryDependencies ++= javaFXModules.map(m =>
  "org.openjfx" % s"javafx-$m" % "16" classifier osName
)

// configure module path
fork := true
javaOptions ++= Seq(
  "--module-path", System.getProperty("java.home") + "/lib",
  "--add-modules", "javafx.controls,javafx.fxml,javafx.graphics,javafx.media,javafx.swing,javafx.web"
)