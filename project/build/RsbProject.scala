import sbt._

class RsbProject(info: ProjectInfo) extends DefaultProject(info) {
//    val scala_lang_releases = "scala-lang-releases" at "http://maven.scala-tools.org/repo-releases"
    val scala_lang_snapshots = "scala-lang-snapshots" at "http://maven.scala-tools.org/repo-snapshots"

    object Artifacts {
        val VERSION_JETTY = "6.1.23"

        val jetty = "org.mortbay.jetty" % "jetty" % VERSION_JETTY
        val servlet_api = "org.mortbay.jetty" % "servlet-api-2.5" % VERSION_JETTY
    }

    val jetty = Artifacts.jetty
    val specs = "org.scala-tools.testing" % "specs" % "1.6.2.1" % "test"
    val joda_time = "joda-time" % "joda-time" % "1.6"
    val httpcache4j_core = "org.codehaus.httpcache4j" % "httpcache4j-core" % "3.0"
    val httpcache4j_storage_file = "org.codehaus.httpcache4j.storage" % "storage-file" % "3.0"
    val httpcache4j_resolvers_resolvers_net_urlconnection = "org.codehaus.httpcache4j.resolvers" % "resolvers-net-urlconnection" % "3.0"
    val scalaz_core = "com.googlecode.scalaz" % "scalaz-core_2.8.0.RC1" % "5.0-SNAPSHOT"
}

