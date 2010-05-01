import java.net._
import java.io._
import scala.Function._
import scala.collection.immutable.{SortedMap, TreeMap}
import scala.xml.NodeSeq

import Rsb._
import IrcFeed._

case class AtomDocument(val entries: List[AtomEntry]) {
    def toNodeSeq =
        <feed>
            {entries.map(_.toNodeSeq)}
        </feed>
}

object AtomDocument {
    // this is silly
    val toNodeSeq = {(atomDocument: AtomDocument) => atomDocument.toNodeSeq}
}

case class AtomEntry(title: String, content: String) {
    def toNodeSeq =
        <entry>
            <title>{title}</title>
            <content>{content}</content>
        </entry>
}

object IrcFeed {
    def nodeSeqToEntry(nodes: NodeSeq) = AtomEntry((nodes \ "title").text, (nodes \ "content").text)

    val nodeSeqToAtomDocument = { (nodes: NodeSeq) => AtomDocument((nodes \ "entry").map(nodeSeqToEntry).toList) }

    def githubRepositoryListToList(nodes: NodeSeq): List[(String, URL)] = 
        (nodes \ "repository").
            map(node => ((node \ "name").text, new URL("http://github.com/javaBin/" + (node \ "name").text + "/commits/master.atom"))).
            toList

    def githubRepositoryToEntries(t: (String, URL)): List[AtomEntry] = List(AtomEntry(t._1, t._2.toString))
}

class IrcFeedRsbResource extends RsbResource {
    val javabinRepositories = new URL("http://github.com/api/v2/xml/repos/show/javabin")
//    val gitIncogitoMaster = "http://github.com/javaBin/incogito/commits/master.atom"
//    val gitEmsMaster = "http://github.com/javaBin/ems/commits/master.atom"

    val Projects = """/projects""".r
    val Project = """/projects/([a-z]*)""".r
    val PassThroughProject = """/direct-([a-z]*)""".r

    def requestRepositoryList(request: RsbRequest): Either[ResourceResponse, SortedMap[String, URL]] = {
//        val f = inputStreamToNodeSeq.andThen(githubRepositoryListToList).andThen(_.foldLeft(TreeMap)({(map, entry) => map + (entry._1, entry._2)}))
        val f = inputStreamToNodeSeq.andThen(githubRepositoryListToList).andThen(list => TreeMap[String, URL]() ++ list)
        request.subRequest(javabinRepositories, "GET") match {
            case ResourceResponse(200, headers, is) => Right(f(is))
            case x => Left(x)
        }
    }

    def requestRepository(request: RsbRequest, url: URL): Either[ResourceResponse, List[AtomEntry]] = {
        val f = inputStreamToNodeSeq.andThen(nodeSeqToAtomDocument).andThen(_.entries)
        request.subRequest(url, "GET") match {
            case ResourceResponse(200, headers, is) => Right(f(is))
            case x => Left(x)
        }
    }

    def apply(request: RsbRequest): ResourceResponse = {
        request.path match {
            case Projects => 
//                requestRepositoryList(request) match {
//                    case response @ ResourceObjectResponse(200, headers, s @ Some(githubRepositories: List[(String, URL)])) => 
//                        ResourceResponse(200, response.headers, Some(AtomDocument(List.flatten(githubRepositories.map(githubRepositoryToEntries)))))
//                    case _ => textErrorResponse("Unable to get list of repositories")
//                }
                internalError("all projects")
            case PassThroughProject(project) =>
                // This will simply stream the data from the backend directly
                request.subRequest(new URL("http://github.com/javaBin/" + project + "/commits/master.atom"), "GET")
            case Project(project) =>
                // Download the repositories url, find the correct project, download the atom for the project, parse it as atom, and pass it back out as atom
                requestRepositoryList(request) match {
                    case Right(repositories) => repositories.get(project) match {
                        case None => notFound("No such project '" + project + "'")
                        case Some(_) =>
                            val f: Function1[InputStream, InputStream] = inputStreamToNodeSeq.andThen(nodeSeqToAtomDocument.andThen(AtomDocument.toNodeSeq).andThen(curried(nodeSeqToInputStream)("UTF-8")))
                            request.subRequest(new URL("http://github.com/javaBin/" + project + "/commits/master.atom"), "GET")
                    }
                    case Left(x) => x
                }
            case resource =>
                internalError("Invalid resource: " + resource)
        }
    }
}

