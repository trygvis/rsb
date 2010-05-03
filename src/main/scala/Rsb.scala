import java.io._
import java.net._
import scala.xml.NodeSeq
import org.codehaus.httpcache4j._
import org.codehaus.httpcache4j.cache._
import org.codehaus.httpcache4j.urlconnection._

case class RR(status: Int, headers: HttpHeaders)

sealed class ResourceResponse[A] private(_status: Int, _headers: HttpHeaders, producer: => A) {
    private var v: Any = null

    def map(f: PartialFunction[RR, A => ResourceResponse[InputStream]]): ResourceResponse[InputStream] = {
        try {
            f(new RR(_status, _headers))(producer)
        }
        catch {
            case e: MatchError => Rsb.internalError("No match")
            case e => throw e
        }
    }

    def map[B](f: PartialFunction[RR, A => B]): Either[ResourceResponse[InputStream], B] = new ResourceResponse[B](_status, _headers, {
        try {
            Right(f(new RR(_status, _headers))(producer))
        }
        catch {
            case e: MatchError => Left(Rsb.internalError("No match"))
            case e => throw e
        }
    })

    def value: A = {
        if(v == null) {
            v = producer
        }
        v
    }

    /*
    def value: Either[ResourceResponse[InputStream, InputStream], B] = {
        val rr = RR(_status, _headers)

        f.isDefinedAt(rr) match {
            case true => Right(f(rr)(value))
            case false => Left(Rsb.internalError("poop"))
        }
    }
    */

    def status = _status

    def headers = _headers
}

object ResourceResponse {
    def apply(status: Int, headers: HttpHeaders, value: InputStream) = new ResourceResponse(status, headers, value)
}

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

case class QueryParameters(private val values: Map[String, List[String]]) {
    def headers = values.keySet

    def header(header: String): Option[List[String]] = values.get(header)
}

object QueryParameters {
    def apply(value: String) = new QueryParameters(Map())
}

class RsbRequest(val path: String, queryParameters: QueryParameters) {
    def subRequest[A](url: URL, verb: String): ResourceResponse[InputStream] = {
        import scala.collection.jcl.{Conversions, MutableIterator}
    
        println("OUT: " + verb + " " + url)

        val method = verb match {
            case "GET" => HTTPMethod.GET
            case _ => error("Unsupported verb '" + verb + "'")
        }
        val request = new HTTPRequest(url.toURI, method)
        val response = RsbRequest.cache.doCachedRequest(request)

        val h = response.getHeaders
        val headers = new MutableIterator.Wrapper(h.keySet.iterator).foldLeft(Map[String, List[String]]())((map, key) => (map + ((key, Conversions.convertList(h.getHeaders(key)).map(_.getValue).toList))))

        println("OUT: " + response.getStatus.getCode + " " + response.getStatus.getName)

        ResourceResponse(response.getStatus.getCode, HttpHeaders(headers), response.getPayload.getInputStream)
    }

    private def doJavaNetUrlRequest(url: URL, verb: String): ResourceResponse[InputStream, InputStream] = {
        import scala.collection.jcl.{Map => JMap, Conversions}
        // TODO: Caching of the request
        // TODO: Caching of the transformed value
        println("OUT: " + verb + " " + url)

        val connection = url.openConnection.asInstanceOf[HttpURLConnection]

        println("OUT: " + connection.getResponseCode + " " + connection.getResponseMessage)

        val headers: HttpHeaders = new HttpHeaders(JMap(connection.getHeaderFields).filter((t) => t._1 != null).foldLeft(Map[String, List[String]]())({(map, entry) => (map + ((entry._1, Conversions.convertList(entry._2).toList)))}))

        connection.getResponseCode match {
            case 404 => 
                ResourceResponse(404, headers, connection.getErrorStream)
            case code => 
                ResourceResponse(code, headers, connection.getInputStream)
        }
    }
}

object RsbRequest {
    private val cacheStoreDirectory = new File(System.getProperty("user.home"), ".rsb/cache")
    if(!cacheStoreDirectory.isDirectory && !cacheStoreDirectory.mkdirs()) error("Could not create directory: " + cacheStoreDirectory)

    val cache = new HTTPCache(new PersistentCacheStorage(cacheStoreDirectory), new URLConnectionResponseResolver(new URLConnectionConfigurator))
}

abstract class RsbResource {
    def apply(request: RsbRequest): ResourceResponse[InputStream]
}

object Rsb {
    def stringResponse(status: Int, message: String): ResourceResponse[InputStream] = stringResponse(status, HttpHeaders(), message)

    def stringResponse(status: Int, headers: HttpHeaders, message: String) = ResourceResponse(status, 
        HttpHeaders().withContentType("text/plain").withContentEncoding("UTF-8"), 
        new ByteArrayInputStream((message + "\n").getBytes("UTF-8")).asInstanceOf[InputStream])

    def internalError(message: String) = stringResponse(500, message)

    def notFound(message: String) = stringResponse(400, message)

    def notFound: ResourceResponse[InputStream] = notFound("Resource not found")

    implicit def contentType(value: String): ContentType = ContentType(value)

    val inputStreamToNodeSeq: Function1[InputStream, NodeSeq] = {is => scala.xml.XML.load(is)}

    val nodeSeqToInputStream = {(encoding: String, nodes: NodeSeq) => new ByteArrayInputStream(nodes.toString.getBytes(encoding))}

    def identity[T]: Function[T, T] = {t: T => t}
}

