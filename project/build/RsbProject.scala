import sbt._

class RsbProject(info: ProjectInfo) extends DefaultProject(info) {
  object Artifacts {
    val VERSION_JETTY = "6.1.23"

    val jetty = "org.mortbay.jetty" % "jetty" % VERSION_JETTY
    val servlet_api = "org.mortbay.jetty" % "servlet-api-2.5" % VERSION_JETTY
  }

  val jetty = Artifacts.jetty
    val specs = "org.scala-tools.testing" % "specs" % "1.6.2.1" % "test"
}

