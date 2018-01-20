import java.io.*;
import java.net.HttpURLConnection;
import java.net.InetSocketAddress;
import java.net.URL;
import java.util.*;

import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpHandler;
import com.sun.net.httpserver.HttpServer;
import org.json.JSONArray;
import org.json.JSONObject;

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
            List<Transaction> copy = new ArrayList<Transaction>(this.dataManager.registered_transactions);
            for (Transaction t: copy) {
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
            if (result.equals("success"))
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
            if (result.equals("success"))
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
        try {
            String airport_ID = this.dataManager.ticket_ID_to_airport_ID_map.get(ticket_ID);
            int port = this.dataManager.airport_ID_to_port_map.get(airport_ID);

            System.out.println("Contacting " + airport_ID + " with " + action);
            HttpURLConnection request_to_send;
            URL url = new URL("http://localhost:" + port + "/" + action);
            request_to_send = (HttpURLConnection) url.openConnection();
            request_to_send.setRequestMethod("POST");


            // Preparing data of the request
            // UUID4 ot ensure no collisions
            String ticket_ID_json = ticket_ID;
            String owning_transaction_ID = transaction.ID;
            String transaction_home_server_ID = transaction.home_server_ID;
            String data_to_json = new JSONObject()
                    .put("ticket_ID_json", ticket_ID_json)
                    .put("owning_transaction_ID", owning_transaction_ID)
                    .put("transaction_home_server_ID", transaction_home_server_ID).toString();

            //System.out.println(data_to_json.toString());
            request_to_send.setDoOutput(true);
            try {
                DataOutputStream wr = new DataOutputStream(request_to_send.getOutputStream());
                wr.writeBytes(data_to_json);
                wr.flush();
                wr.close();
            } catch (Exception e) {
                System.out.println(e);
            }

            BufferedReader in = new BufferedReader(
                    new InputStreamReader(request_to_send.getInputStream()));
            String inputLine;
            StringBuffer response = new StringBuffer();

            while ((inputLine = in.readLine()) != null) {
                response.append(inputLine);
            }
            in.close();

            //print result
            System.out.println("Received: " + response.toString());

            if (response.toString().equals("SUCCESS"))
                return "success";
            else
                return "error";
        }
        catch (Exception e) {
            System.out.println(e);
        }
        return  "error";
    }


    static HttpHandler createHandler (TicketDataManager dataManager) {

        class MyHandler implements HttpHandler {

            TicketDataManager dataManager;

            MyHandler (TicketDataManager dataManager) {
                super();
                this.dataManager = dataManager;
            }

            @Override
            public void handle(HttpExchange t) throws IOException {
                // gets what method was used (GET, POST, etc.)
                String response = "";
                OutputStream os = t.getResponseBody();
                switch (t.getRequestMethod()) {
                    case "POST" :
                        String data = "";
                        //System.out.println("received POST");
                        BufferedReader in = new BufferedReader(
                                new InputStreamReader(t.getRequestBody()));
                        String inputLine;
                        while ((inputLine = in.readLine()) != null) {
                            data = data + inputLine;
                        }
                        //System.out.println(data);
                        //System.out.println("POST accepted");
                        System.out.println(t.getRequestURI().getPath());
                        //System.out.println("Odpowiedz :" + (t.getRequestURI().getPath().equals("/register_transaction")));
                        //t.getRequestURI().getPath()
                        if (t.getRequestURI().getPath().equals("/register_transaction"))
                            response = this.register_transaction(data);
                        else if (t.getRequestURI().getPath().equals("/ping"))
                            response = this.ping(data);
                        else if (t.getRequestURI().getPath().equals("/reserve"))
                            response = this.reserve_ticket(data);
                        else if (t.getRequestURI().getPath().equals("/commit"))
                            response = this.commit_ticket(data);
                        else if (t.getRequestURI().getPath().equals("/abort"))
                            response = this.abort_ticket(data);

                        // decoded_object = json.loads(received_bytes)
                        // print(decoded_object)
                        t.sendResponseHeaders(200, response.length());
                        os.write(response.getBytes());
                        os.close();
                        break;

                    case "GET" :
                        response = "This is the response GET";
                        response = "<html><body><pre>Dane serwera " + this.dataManager.ID + "\n";
                        response += "Zarejestrowane transakcje:\n<hr>";
                        for(Transaction transaction : this.dataManager.registered_transactions) {
                            response += transaction.toString();
                            response += "<hr/>";
                        }
                        response += "\nZarezerwowane bilety:\n";
                        for (String ticket_ID : this.dataManager.ticket_reserved_to_transaction_list_map.keySet()) {
                            response += "\n" + ticket_ID + ":";
                            for (Transaction transaction : this.dataManager.ticket_reserved_to_transaction_list_map.get(ticket_ID)){
                                response += "\n" + transaction.ID;
                            }
                        }
                        response += "\n<hr>Kupione bilety:\n";
                        for (String ticket_ID : this.dataManager.ticket_completed_to_transaction_list_map.keySet()) {
                            response += "\n" + ticket_ID + ":";
                            for (Transaction transaction : this.dataManager.ticket_completed_to_transaction_list_map.get(ticket_ID)) {
                                response += "\n" + transaction.ID;
                            }
                        }
                        response += "</pre></body></html>";

                        t.sendResponseHeaders(200, response.length());
                        os.write(response.getBytes());
                        os.close();
                        break;

                    default:
                        response = "This is the GENERIC response";
                        t.sendResponseHeaders(200, response.length());
                        os.write(response.getBytes());
                        os.close();
                        break;
                }
            }

            private String register_transaction(String data) {
                JSONObject received_json = new JSONObject(data);
                Transaction incoming_transaction = new Transaction(received_json.get("transaction_ID").toString(),
                        Utils.jsonStringToArray(received_json.get("required_tickets").toString()),
                        this.dataManager.ID,
                        null);
                this.dataManager.registered_transactions.add(incoming_transaction);
                return "Registration complete";
            }

            private String ping(String data) {
                JSONObject received_json = new JSONObject(data);
                Transaction pinged_transaction = null;
                for (Transaction t : this.dataManager.registered_transactions) {
                    if(t.ID.equals(received_json.getString("transaction_ID"))){
                        pinged_transaction = t;
                        break;
                    }
                }
                if (pinged_transaction == null)
                    return "Invalid transaction ID";

                pinged_transaction.last_ping_timestamp = new Date();

                if (pinged_transaction.status == TransactionStatus.COMPLETED)
                    pinged_transaction.status = TransactionStatus.ACKNOWLEDGED;

                return "TransactionStatus." + pinged_transaction.status.toString();
            }

            private String reserve_ticket(String data) {
                JSONObject received_json = new JSONObject(data);
                String ticket_ID_json = received_json.getString("ticket_ID_json");
                String owning_transaction_ID = received_json.getString("owning_transaction_ID");
                String transaction_home_server_ID = received_json.getString("transaction_home_server_ID");
                System.out.println("Reserving ticket " + ticket_ID_json + " for " + transaction_home_server_ID);

                if (!this.dataManager.myTicketList.contains(ticket_ID_json))
                    return "WRONG_SERVER";
                int purchased_tickets = this.dataManager.ticket_completed_to_transaction_list_map.get(ticket_ID_json).size();
                if ( purchased_tickets >= this.dataManager.ticket_quantities.get(ticket_ID_json))
                    return "SOLD_OUT";
                Random generator = new Random();
                int sleep_time = generator.nextInt(5) + 1;
                for (int i = 0; i < 3; ++i ) {
                    int reserved_tickets = this.dataManager.ticket_reserved_to_transaction_list_map.get(ticket_ID_json).size();
                    int purchased_tickets_no = this.dataManager.ticket_completed_to_transaction_list_map.get(ticket_ID_json).size();
                    if (purchased_tickets_no + reserved_tickets >= this.dataManager.ticket_quantities.get(ticket_ID_json)) {
                        try {
                            Thread.sleep(sleep_time);
                        }
                        catch (Exception e) {
                            System.out.println(e);
                        }
                    }
                }
                //  If no tickets became available - this transaction is deadlocked. Abort.
                int reserved_tickets = this.dataManager.ticket_reserved_to_transaction_list_map.get(ticket_ID_json).size();
                purchased_tickets = this.dataManager.ticket_completed_to_transaction_list_map.get(ticket_ID_json).size();
                if (purchased_tickets + reserved_tickets >= this.dataManager.ticket_quantities.get(ticket_ID_json))
                    return "ALL_RESERVED";

                this.dataManager.ticket_reserved_to_transaction_list_map.get(ticket_ID_json).add(
                        new Transaction(owning_transaction_ID,
                                new ArrayList<String>(){},
                                transaction_home_server_ID,
                                TransactionStatus.REMOTE));
                return "SUCCESS";
            }

            private String commit_ticket(String data) {
                JSONObject received_json = new JSONObject(data);
                String ticket_ID_json = received_json.getString("ticket_ID_json");
                String owning_transaction_ID = received_json.getString("owning_transaction_ID");
                String transaction_home_server_ID = received_json.getString("transaction_home_server_ID");
                System.out.println("Commiting ticket " + ticket_ID_json + " for " + transaction_home_server_ID);

                if (!this.dataManager.myTicketList.contains(ticket_ID_json))
                    return "WRONG_SERVER";

                //System.out.println(owning_transaction_ID);
                //System.out.println(this.dataManager.ticket_reserved_to_transaction_list_map.get(ticket_ID_json));
                Transaction existing_transaction = null;

                for (Transaction t : this.dataManager.ticket_reserved_to_transaction_list_map.get(ticket_ID_json)) {
                    if(t.ID.equals(owning_transaction_ID)){
                        existing_transaction = t;
                        break;
                    }
                }

                if (existing_transaction == null)
                    return "NOT_RESERVED";

                this.dataManager.ticket_reserved_to_transaction_list_map.get(ticket_ID_json).remove(existing_transaction);
                this.dataManager.ticket_completed_to_transaction_list_map.get(ticket_ID_json).add(existing_transaction);
                return "SUCCESS";
            }
            
            private String abort_ticket(String data) {
                JSONObject received_json = new JSONObject(data);
                String ticket_ID_json = received_json.getString("ticket_ID_json");
                String owning_transaction_ID = received_json.getString("owning_transaction_ID");
                String transaction_home_server_ID = received_json.getString("transaction_home_server_ID");
                System.out.println("Aborting ticket " + ticket_ID_json + " for " + transaction_home_server_ID +
                        ", transaction " + owning_transaction_ID);

                Transaction check_if_reserved = null;
                for (Transaction t : this.dataManager.ticket_reserved_to_transaction_list_map.get(ticket_ID_json)) {
                    if(t.ID.equals(owning_transaction_ID)){
                        check_if_reserved = t;
                        break;
                    }
                }

                if (check_if_reserved != null)
                    this.dataManager.ticket_reserved_to_transaction_list_map.get(ticket_ID_json).remove(check_if_reserved);

                Transaction check_if_committed = null;
                for (Transaction t : this.dataManager.ticket_completed_to_transaction_list_map.get(ticket_ID_json)) {
                    if(t.ID.equals(owning_transaction_ID)){
                        check_if_committed = t;
                        break;
                    }
                }

                if (check_if_committed != null)
                    this.dataManager.ticket_completed_to_transaction_list_map.get(ticket_ID_json).remove(check_if_committed);

                return "SUCCESS";
            }
        }

        return new MyHandler(dataManager);
    }

}