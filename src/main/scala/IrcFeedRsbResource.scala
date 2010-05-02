import java.io._
import java.net._
import org.joda.time._
import org.joda.time.format._
import scala.Function._
import scala.collection.immutable.{SortedMap, TreeMap}
import scala.xml.NodeSeq

import Rsb._
import IrcFeed._

case class AtomDocument(val id: String, val title: String, val updated: DateTime, val entries: List[AtomEntry]) {
    def +(entries: List[AtomEntry]) = new AtomDocument(id, title, updated, this.entries ++ entries)

    def toNodeSeq =
        <feed xml:lang="en-US" xmlns="http://www.w3.org/2005/Atom">
            <id>{id}</id>
            <title>{title}</title>
            <updated>{updated}</updated>
            {entries.map(_.toNodeSeq)}
        </feed>
}

object AtomDocument {
    def apply(id: String, title: String, updated: DateTime): AtomDocument = new AtomDocument(id, title, updated, Nil)

    // this is silly
    val toNodeSeq = {(atomDocument: AtomDocument) => atomDocument.toNodeSeq}

    val formatter = ISODateTimeFormat.dateTimeParser
}

case class AtomAuthor(name: String) {
    def toNodeSeq =
        <author>
            <name>{name}</name>
        </author>
}

case class AtomEntry(title: String, author: AtomAuthor, content: String) {
    def toNodeSeq =
        <entry>
            <title>{title}</title>
            {author.toNodeSeq}
            <content>{content}</content>
        </entry>
}

object IrcFeed {
    def nodeSeqToAuthor(nodes: NodeSeq) = AtomAuthor((nodes \ "name").text)

    def nodeSeqToEntry(nodes: NodeSeq) = AtomEntry((nodes \ "title").text, nodeSeqToAuthor(nodes \ "author"), (nodes \ "content").text)

    val nodeSeqToAtomDocument = { (nodes: NodeSeq) => AtomDocument((nodes \ "id").text, (nodes \ "title").text, AtomDocument.formatter.parseDateTime((nodes \ "updated").text), (nodes \ "entry").map(nodeSeqToEntry).toList) }

    def githubRepositoryListToList(nodes: NodeSeq): List[(String, URL)] = 
        (nodes \ "repository").
            map(node => ((node \ "name").text, new URL("http://github.com/javaBin/" + (node \ "name").text + "/commits/master.atom"))).
            toList

//    def githubRepositoryToEntries(t: (String, URL)): List[AtomEntry] = List(AtomEntry(t._1, t._2.toString))
}

class IrcFeedRsbResource extends RsbResource {
    val javabinRepositories = new URL("http://github.com/api/v2/xml/repos/show/javabin")
//    val gitIncogitoMaster = "http://github.com/javaBin/incogito/commits/master.atom"
//    val gitEmsMaster = "http://github.com/javaBin/ems/commits/master.atom"

    val Projects = """/projects""".r
    val Project = """/projects/([a-z]*)""".r
    val PassThroughProject = """/direct-([a-z]*)""".r

    def requestRepositoryList(request: RsbRequest): Either[ResourceResponse, SortedMap[String, URL]] = {
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
            case Projects() =>
                requestRepositoryList(request) match {
                    case Right(repositories) =>
                        val documents = repositories.
                            map(t => request.subRequest(new URL("http://github.com/javaBin/" + t._1 + "/commits/master.atom"), "GET")).
                            filter(_.status == 200).
                            map(_.stream).
                            map(inputStreamToNodeSeq.andThen(nodeSeqToAtomDocument))

                        val updated = documents.foldLeft(new DateTime(0l))((max, current) => if(current.updated.isAfter(max)) current.updated else max)
                        val document = documents.foldLeft(AtomDocument("tag:javabin:github", "Combined commit feed for javaBin", updated))((collection, document) => collection + document.entries)

                        ResourceResponse(200, HttpHeaders().withContentType("application/atom+xml; charset=utf-8"), nodeSeqToInputStream("UTF-8", document.toNodeSeq))
                    case _ => internalError("Unable to get list of repositories")
                }
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
                            request.subRequest(new URL("http://github.com/javaBin/" + project + "/commits/master.atom"), "GET") match {
                                case ResourceResponse(200, headers, is) => ResourceResponse(200, headers, f(is))
                                case x => x
                            }
                    }
                    case Left(x) => x
                }
            case resource =>
                internalError("Invalid resource: " + resource)
        }
    }
}

