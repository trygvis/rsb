import java.io._
import java.net._
import scala.xml.NodeSeq

sealed case class ResourceResponse(status: Int, headers: HttpHeaders, stream: InputStream)

case class ContentType(val value: String)

object ContentType {
}

case class HttpHeaders(val values: Map[String, List[String]]) extends Iterable[(String, List[String])] {
    def elements = values.elements

    def withContentType(contentType: ContentType) = new HttpHeaders(values + (("Content-Type",  List(contentType.value))))

    def withContentEncoding(encoding: String) = new HttpHeaders(values + (("Content-Encoding",  List(encoding))))
}

object HttpHeaders {
    def apply() = new HttpHeaders(Map())
}

class RsbRequest(val path: String) {
    def subRequest(url: URL, verb: String): ResourceResponse = {
        import scala.collection.jcl.{Map => JMap, Conversions}
        // TODO: Caching of the request
        // TODO: Caching of the transformed value
        println("OUT: " + verb + " " + url)

        val connection = url.openConnection.asInstanceOf[HttpURLConnection]

        println("OUT: " + connection.getResponseCode + " " + connection.getResponseMessage)

        val headers: HttpHeaders = new HttpHeaders(JMap(connection.getHeaderFields).filter((t) => t._1 != null).foldLeft(Map[String, List[String]]())({(map, entry) => (map + ((entry._1, Conversions.convertList(entry._2).toList)))}))

        connection.getResponseCode match {
            case 404 => 
                new ResourceResponse(404, headers, connection.getErrorStream)
            case code => 
                new ResourceResponse(code, headers, connection.getInputStream)
        }
    }
}

abstract class RsbResource {
    def apply(request: RsbRequest): ResourceResponse
}

object Rsb {
    def stringResponse(status: Int, message: String): ResourceResponse = stringResponse(status, HttpHeaders(), message)

    def stringResponse(status: Int, headers: HttpHeaders, message: String) = new ResourceResponse(status, 
        HttpHeaders().withContentType("text/plain").withContentEncoding("UTF-8"), 
        new ByteArrayInputStream((message + "\n").getBytes("UTF-8")))

    def internalError(message: String) = stringResponse(500, message)

    def notFound(message: String) = stringResponse(400, message)

    def notFound: ResourceResponse = notFound("Resource not found")

    implicit def contentType(value: String): ContentType = ContentType(value)

    val inputStreamToNodeSeq: Function1[InputStream, NodeSeq] = {is => scala.xml.XML.load(is)}

    val nodeSeqToInputStream = {(encoding: String, nodes: NodeSeq) => new ByteArrayInputStream(nodes.toString.getBytes(encoding))}

    def identity[T]: Function[T, T] = {t: T => t}
}

