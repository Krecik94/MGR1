import java.io.IOException;
import java.io.OutputStream;
import java.net.InetSocketAddress;
import java.util.Map;

import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpHandler;
import com.sun.net.httpserver.HttpServer;

public class TicketServer {

    private String airport_ID;
    private TicketDataManager dataManager;
    private Map<String, Integer> airport_data;
    int port;

    TicketServer(String airport_ID, Map<String, Integer> airport_data, int port) {
        this.airport_ID = airport_ID;
        this.airport_data = airport_data;
        this.dataManager = new TicketDataManager(airport_ID, airport_data);
    }

    public void run() throws Exception {
        HttpServer server = HttpServer.create(new InetSocketAddress(port), 0);
        server.createContext("/test", createHandler(this.dataManager));
        server.setExecutor(null); // creates a default executor
        server.start();
    }

    static HttpHandler createHandler (TicketDataManager dataManager) {

        class MyHandler implements HttpHandler {
            @Override
            public void handle(HttpExchange t) throws IOException {
                // gets what method was used (GET, POST, etc.)
                String response;
                switch (t.getRequestMethod()) {
                    case "POST" :
                        response = "This is the response " + t.getRequestMethod();
                        break;
                    case "GET" :
                        response = "This is the response GET";
                        break;
                    default:
                        response = "This is the GENERIC response";
                        break;
                }
                t.sendResponseHeaders(200, response.length());
                OutputStream os = t.getResponseBody();
                os.write(response.getBytes());
                os.close();
            }
        }

        return new MyHandler();
    }

}