import java.io.IOException;
import java.io.OutputStream;
import java.net.InetSocketAddress;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpHandler;
import com.sun.net.httpserver.HttpServer;

public class TicketServer implements Runnable{

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
        } else {
            throw new Exception("Airport ID not found");
        }

        this.should_shutdown = false;
    }

    public void run() {
        server.start();
        this.should_shutdown = false;
        handle_requests_until_shutdown();
    }

    public void stop() {
        this.should_shutdown = true;
        this.server.stop(0);
    }

    // Loop that iterates over all transactions and handles them accordingly
    private void handle_requests_until_shutdown() {
        while(!this.should_shutdown) {
            for (Transaction t: this.dataManager.registered_transactions) {
                if (t.status == TransactionStatus.REGISTERED)
                    handle_registered_transaction(t);
                try {
                    Thread.sleep(2000);
                } catch (Exception e) {
                    System.out.println(e);
                }
            }
        }

    }

    private void handle_registered_transaction(Transaction t) {
        // Setting transaction status to let outer loop know it is being processed
        t.status = TransactionStatus.PENDING_RESERVATION;

        // Extracting tickets that belong to local server
        List<String> local_tickets = new ArrayList<String>(t.ticket_ID_list);
        local_tickets.retainAll(this.dataManager.myTicketList);

        // Extracting tickets that need to be acquired from remote servers
        List<String> remote_tickets = new ArrayList<String>(t.ticket_ID_list);
        remote_tickets.removeAll(local_tickets);

        // Counting if local server has enough tickets START
        Boolean has_enough_local_tickets = true;
        for (String local_ticket : local_tickets) {
            int reserved_tickets = (this.dataManager.ticket_reserved_to_transaction_list_map.get(local_ticket)).size();
            int purchased_tickets = (this.dataManager.ticket_completed_to_transaction_list_map.get(local_ticket)).size();
            // Doesn't work with deadlocks TODO FIX THIS
            if(reserved_tickets + purchased_tickets >= this.dataManager.ticket_quantities.get(local_ticket)) {
                has_enough_local_tickets = false;
            }
        }
        // Counting if local server has enough tickets END

        // Reserving local tickets
        if (has_enough_local_tickets) {
            for (String local_ticket : local_tickets) {
                // reserving local tickets
                this.dataManager.ticket_reserved_to_transaction_list_map.get(local_ticket).add(t);
                t.status = TransactionStatus.RESERVED;
            }
        }
        else {
            // Case when there is not enough local tickets
            t.status = TransactionStatus.OUT_OF_TICKETS;
            return;
        }

        // Case when there is no tickets to be acquired remotely, jump straight to completion
        if (remote_tickets.size() == 0) {
            for (String local_ticket : local_tickets) {
                this.dataManager.ticket_reserved_to_transaction_list_map.get(local_ticket).remove(t);
                this.dataManager.ticket_completed_to_transaction_list_map.get(local_ticket).add(t);
            }
            t.status = TransactionStatus.COMPLETED;
            return;
        }

        // Reserving remote tickets
        // No need for 'else' since if there are no remote tickets the method will return
        List<String> successfully_reserved_remote_tickets = new ArrayList<String>(){};
        for (String remote_ticket : remote_tickets) {
            String result = contact_remote_server("reserve", remote_ticket, t);
            // Case when ticket was reserved successfully
            if (result == "success")
                successfully_reserved_remote_tickets.add(remote_ticket);

            // Case when ticket could not be reserved, aborting
            else {
                t.status = TransactionStatus.ERROR;
                for (String local_ticket : local_tickets)
                    this.dataManager.ticket_reserved_to_transaction_list_map.get(local_ticket).remove(t);

                for (String remote_reserved_ticket : successfully_reserved_remote_tickets)
                    contact_remote_server("abort", remote_reserved_ticket, t);
                return;
            }
        }

        // All tickets are reserved
        t.status = TransactionStatus.RESERVED;

        // Committing all tickets
        // Committing local tickets
        for (String local_ticket : local_tickets) {
            // Removing from reserved list
            this.dataManager.ticket_reserved_to_transaction_list_map.get(local_ticket).remove(t);
            // Adding to completed list
            this.dataManager.ticket_completed_to_transaction_list_map.get(local_ticket).add(t);
        }

        // Committing remote tickets
        List<String> successfully_committed_remote_tickets = new ArrayList<String>(){};
        for (String remote_ticket : remote_tickets) {
            String result = contact_remote_server("commit", remote_ticket, t);
            // Case when ticket was reserved successfully
            if (result == "success")
                successfully_committed_remote_tickets.add(remote_ticket);

            // Case when ticket could not be committed, aborting
            else {
                t.status = TransactionStatus.ERROR;
                for (String local_ticket : local_tickets)
                    this.dataManager.ticket_completed_to_transaction_list_map.get(local_ticket).remove(t);

                for (String remote_reserved_ticket : successfully_reserved_remote_tickets)
                    this.contact_remote_server("abort", remote_reserved_ticket, t);
                return;
            }
        }

        t.status = TransactionStatus.COMPLETED;
    }

    private String contact_remote_server(String action, String ticket_ID, Transaction transaction) {
        return "";
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