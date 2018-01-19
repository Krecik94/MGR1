import java.io.IOException;
import java.io.OutputStream;
import java.net.InetSocketAddress;
import java.util.Map;

import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpHandler;
import com.sun.net.httpserver.HttpServer;

public class ticket_server {

    private String airport_ID;
    private TicketDataManager dataManager;
    //private Map<String, Integer>

    public void run(String airport_ID, Map<String, Integer> airport_data, int port) throws Exception {
        this.airport_ID = airport_ID;
        this.dataManager = new TicketDataManager(airport_ID, airport_data);
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