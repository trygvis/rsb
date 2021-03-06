import java.io._
import java.net._
import org.joda.time._
import scala.Function._
import scala.collection.immutable.{SortedMap, TreeMap}
import scala.xml.NodeSeq

import Rsb._
import IrcFeed._

object IrcFeed {
    def nodeSeqToAuthor(nodes: NodeSeq) = AtomAuthor((nodes \ "name").text)

    def nodeSeqToEntry(nodes: NodeSeq) = AtomEntry((nodes \ "title").text, nodeSeqToAuthor(nodes \ "author"), (nodes \ "content").text)

    val nodeSeqToAtomDocument = { (nodes: NodeSeq) => AtomDocument((nodes \ "id").text, (nodes \ "title").text, AtomDocument.formatter.parseDateTime((nodes \ "updated").text), (nodes \ "entry").map(nodeSeqToEntry).toList) }

    def githubRepositoryListToList(nodes: NodeSeq): List[(String, URL)] = 
        (nodes \ "repository").
            map(node => ((node \ "name").text, new URL("http://github.com/javaBin/" + (node \ "name").text + "/commits/master.atom"))).
            toList
}

class IrcFeedRsbResource extends RsbResource {
    val javabinRepositories = new URL("http://github.com/api/v2/xml/repos/show/javabin")
//    val gitIncogitoMaster = "http://github.com/javaBin/incogito/commits/master.atom"
//    val gitEmsMaster = "http://github.com/javaBin/ems/commits/master.atom"

    val Projects = """/projects""".r
    val Project = """/projects/([a-z]*)""".r
    val SimpleProject = """/simple-projects/([a-z]*)""".r
    val DirectProject = """/direct-projects/([a-z]*)""".r

    val streamToAtom = inputStreamToNodeSeq andThen nodeSeqToAtomDocument

    /*
    def requestRepositoryList(request: RsbRequest): ObjectRR[SortedMap[String,  URL]] = {
        val f: (InputStream => SortedMap[String,  URL]) = inputStreamToNodeSeq.andThen(githubRepositoryListToList).andThen(list => TreeMap[String, URL]() ++ list)
        request.subRequest(javabinRepositories, "GET")(withDefaults(AtomDocument.serializer) {
            case RR(200, is) => f
        })
    }

    def requestRepository(request: RsbRequest, url: URL): Either[ResourceResponse[InputStream], List[AtomEntry]] = {
        val f = inputStreamToNodeSeq.andThen(nodeSeqToAtomDocument).andThen(_.entries)
        request.subRequest(url, "GET")( withDefaults(AtomDocument.toNodeSeq andThen nodeSeqToInputStream("UTF-8")) {
            case (rr@RR(200, headers), is) => (rr, f(is))
        })
    }
    */

    def apply(request: RsbRequest) = {
        request.path match {
            /*
            case Projects() =>
                requestRepositoryList(request) match {
                    case Right(repositories) =>
                        val documents = repositories.
                            map(t => request.subRequest(new URL("http://github.com/javaBin/" + t._1 + "/commits/master.atom"), "GET")).
                            filter(_.status == 200).
                            map({response: ResourceResponse[InputStream] => response.value}.andThen(inputStreamToNodeSeq.andThen(nodeSeqToAtomDocument)))

                        val updated = (new DateTime(0l) /: documents)((max, current) => if(current.updated.isAfter(max)) current.updated else max)
                        val document = (AtomDocument("tag:javabin:github", "Combined commit feed for javaBin", updated) /: documents)((collection, document) => collection + document.entries)

                        ResourceResponse(200, HttpHeaders().withContentType("application/atom+xml; charset=utf-8"), nodeSeqToInputStream("UTF-8", document.toNodeSeq))
                    case _ => internalError("Unable to get list of repositories")
                }
            */
            case DirectProject(project) =>
                // Stream the data from the backend directly
                request.subRequest[StreamableRR, StreamableRR](new URL("http://github.com/javaBin/" + project + "/commits/master.atom"), "GET"){ identity[StreamableRR] }
            case SimpleProject(project) =>
                // Parse the data and send it back out
                // val f: Function1[InputStream, InputStream] = inputStreamToNodeSeq.andThen(nodeSeqToAtomDocument.andThen(AtomDocument.toNodeSeq).andThen(nodeSeqToInputStream("UTF-8")))
                val f: Function1[InputStream, AtomDocument] = inputStreamToNodeSeq andThen nodeSeqToAtomDocument
                val x: StreamableRR => StreamableRR = withDefaults[AtomDocument](AtomDocument.serializer) {
                    case RR(200) => f
                }
                request.subRequest(new URL("http://github.com/javaBin/" + project + "/commits/master.atom"), "GET")(x)
            /*
            case Project(project) =>
                // Download the repositories url, find the correct project, download the atom for the project, parse it as atom, and pass it back out as atom
                requestRepositoryList(request) match {
                    case Right(repositories) => repositories.get(project) match {
                        case None => notFound("No such project '" + project + "'")
                        case Some(_) =>
                            val f: Function1[InputStream, InputStream] = inputStreamToNodeSeq.andThen(nodeSeqToAtomDocument.andThen(AtomDocument.toNodeSeq).andThen(curried(nodeSeqToInputStream)("UTF-8")))
                            request.subRequest(new URL("http://github.com/javaBin/" + project + "/commits/master.atom"), "GET").map({
                                case RR(200, headers) => f
                            })
                    }
                    case Left(x) => x
                }
            */
            case resource =>
                notFound("Invalid resource: " + resource)
        }
    }
}

