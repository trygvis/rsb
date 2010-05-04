import java.io._
import java.net._
import scala.xml.NodeSeq
import Function._
import org.codehaus.httpcache4j._
import org.codehaus.httpcache4j.cache._
import org.codehaus.httpcache4j.urlconnection._

sealed class RR(val status: Int, val headers: HttpHeaders, val stream: InputStream)

class ObjectRR[A](private val _status: Int, private val _headers: HttpHeaders, private val _stream: InputStream, val value: A) extends RR(_status, _headers, _stream) {
    def makeCacheable(serializer: A => InputStream) = new CacheableObjectRR[A](_status, _headers, _stream, value, serializer)
}

class CacheableObjectRR[A](private val _status: Int, private val _headers: HttpHeaders, private val _stream: InputStream, val value: A, val serializer: A => InputStream) extends RR(_status, _headers, _stream) {

    /*
    var v: Option[(RR, A)] = None
    def value: (RR, A) =
        v match {
            case Some(value) => value
            case None => {
                val value = f(new RR(_status, _headers), _value)
                v = Some(value)
                value
            }
        }

    def status = _status

    def headers = _headers
    */
}

object RR {
    def apply(status: Int, headers: HttpHeaders, stream: InputStream) = new RR(status, headers, stream)

    def apply[A](status: Int, headers: HttpHeaders, stream: InputStream, value: A) = new ObjectRR(status, headers, stream, value)

    def unapply(rr: RR) = Some((rr.status, rr.headers))
}

case class ContentType(val value: String)

object ContentType {
}

case class HttpHeaders(private val values: Map[String, List[String]]) extends Iterable[(String, List[String])] {
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
    def subRequest[A](url: URL, verb: String)(f: RR => RR): RR = {
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

        RR(response.getStatus.getCode, HttpHeaders(headers), response.getPayload.getInputStream)
    }

    /*
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
    */
}

object RsbRequest {
    private val cacheStoreDirectory = new File(System.getProperty("user.home"), ".rsb/cache")
    if(!cacheStoreDirectory.isDirectory && !cacheStoreDirectory.mkdirs()) error("Could not create directory: " + cacheStoreDirectory)

    val cache = new HTTPCache(new PersistentCacheStorage(cacheStoreDirectory), new URLConnectionResponseResolver(new URLConnectionConfigurator))
}

abstract class RsbResource {
    def apply(request: RsbRequest): RR
}

object Rsb {
//    def stringResponse(message: String): (RR, InputStream) => InputStream = { (rr: RR, InputStream) => stringResponse(rr.status, message)}

    // TODO: Make Cacheable
    def stringResponse(status: Int, message: String): RR =
        new ObjectRR(status, HttpHeaders().withContentType("text/plain").withContentEncoding("UTF-8"),
        new ByteArrayInputStream((message + "\n").getBytes("UTF-8")).asInstanceOf[InputStream], identity[InputStream])

    def internalError(message: String) = stringResponse(500, message)

    def notFound(message: String) = stringResponse(400, message)

    def notFound: RR = notFound("Resource not found")

    implicit def contentType(value: String): ContentType = ContentType(value)

    val inputStreamToNodeSeq: Function1[InputStream, NodeSeq] = {is => scala.xml.XML.load(is)}

    val nodeSeqToInputStream = curried({(encoding: String, nodes: NodeSeq) => new ByteArrayInputStream(nodes.toString.getBytes(encoding))})

    def identity[T]: Function[T, T] = {t: T => t}

    def withDefaults[A](serializer: A => InputStream)(f: PartialFunction[RR, A]): RR => RR = { (rr: RR) =>
        if(f.isDefinedAt(rr)) {
            new CacheableObjectRR(rr.status, rr.headers, rr.stream, f(rr), serializer)
        } else {
            Rsb.internalError("Bad request")
        }
    }
}
