import javax.servlet._
import javax.servlet.http._
import org.mortbay.jetty.{HttpHeaders => _, _}
import org.mortbay.jetty.handler._
import org.mortbay.util._

class RsbHandler extends AbstractHandler {
    val resource: RsbResource = new IrcFeedRsbResource

    def handle(target: String, httpRequest: HttpServletRequest, httpResponse: HttpServletResponse, dispatch: Int) {
        val baseRequest = if (httpRequest.isInstanceOf[Request]) httpRequest.asInstanceOf[Request] else HttpConnection.getCurrentConnection().getRequest();
        if (httpResponse.isCommitted() || baseRequest.isHandled()) {
            return
        }

        println(" IN: " + httpRequest.getMethod + " " + httpRequest.getRequestURI);
        val request = new RsbRequest(httpRequest.getRequestURI)
        val result = resource.apply(request)
        println(" IN: " + result.status)
        httpResponse.setStatus(result.status)
        result.headers.foreach(t => t._2.foreach(value => httpResponse.setHeader(t._1, value)))

        IO.copy(result.stream, httpResponse.getOutputStream())
        baseRequest.setHandled(true)
    }
}

object RsbServer {
    def main(args: Array[String]) {
        var server = new Server(8200)

        server.addHandler(new RsbHandler)

        var defaultHandler = new DefaultHandler()
        server.addHandler(defaultHandler)

        val t = new Thread() {
            override def run() {
                println("Press any key to stop...");
                System.in.read();
                println("Stopping");
                server.stop();
            }
        }
        t.setDaemon(true)
        t.start()
        server.start();
        server.join();
    }
}

