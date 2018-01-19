import java.io.IOException;
import java.io.OutputStream;
import java.net.InetSocketAddress;
import java.util.Map;

import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpHandler;
import com.sun.net.httpserver.HttpServer;

public class ticket_server {

    private String airport_ID;
    //private Map<String, Integer>

    public static void run(int port) throws Exception {
        HttpServer server = HttpServer.create(new InetSocketAddress(port), 0);
        server.createContext("/test", new MyHandler());
        server.setExecutor(null); // creates a default executor
        server.start();
    }

    static class MyHandler implements HttpHandler {
        @Override
        public void handle(HttpExchange t) throws IOException {
            // gets what method was used (GET, POST, etc.)
            String response;
            if(t.getRequestMethod() == "GET")
                response = "This is the response GET";
            else if (t.getRequestMethod() == "POST")
                response = "This is the response " + t.getRequestMethod();
            else
                response = "This is the GENERIC response";
            t.sendResponseHeaders(200, response.length());
            OutputStream os = t.getResponseBody();
            os.write(response.getBytes());
            os.close();
        }
    }

}