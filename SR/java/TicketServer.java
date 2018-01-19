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
    HttpServer server;

    Boolean should_shutdown;

    TicketServer(String airport_ID, Map<String, Integer> airport_data) throws Exception {
        System.out.print("Creating supervisor. Airport_ID: " + airport_ID + "\nTicket_quantities: " + airport_data + "\n");

        this.airport_ID = airport_ID;
        this.airport_data = airport_data;
        this.dataManager = new TicketDataManager(airport_ID, airport_data);
        HttpHandler handler = createHandler(this.dataManager);

        // Check if airport ID is defined in dict.
        if (this.dataManager.airport_ID_to_port_map.containsKey(this.airport_ID)) {
            this.server = HttpServer.create(
                    new InetSocketAddress(this.dataManager.airport_ID_to_port_map.get(this.airport_ID)), 0);
            server.createContext("/", handler);
            server.setExecutor(null); // creates a default executor
        }
        else {
            throw new Exception ("Airport ID not found");
        }

        this.should_shutdown = false;
    }

    public void run() throws Exception {
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