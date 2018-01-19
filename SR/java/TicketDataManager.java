import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class TicketDataManager {

    String ID;

    Map<String, Integer> aiport_ID_to_port_map = new HashMap<String, Integer>(){};
    Map<String, String> ticket_ID_to_airport_ID_map = new HashMap<String, String>(){};

    Map<String, TransactionStatus[]> ticket_reserved_to_transaction_list_map = new HashMap<String, TransactionStatus[]>(){};
    Map<String, TransactionStatus[]> ticket_completed_to_transaction_list_map = new HashMap<String, TransactionStatus[]>(){};

    Map<String, Integer> ticket_quantities;

    List<String> ticket_list = new ArrayList<String>(){};
    List<String> registered_transactions = new ArrayList<String>(){};


    TicketDataManager (String ID, Map<String, Integer> ticket_quantities) {
        fillMaps();
        this.ID = ID;
        this.ticket_quantities = ticket_quantities;

        for (Map.Entry<String, String> entry : ticket_ID_to_airport_ID_map.entrySet())
        {
            if(entry.getValue() == ID) {
                ticket_list.add(entry.getKey());
                ticket_reserved_to_transaction_list_map.put(entry.getKey(), null);
                ticket_completed_to_transaction_list_map.put(entry.getKey(), null);
            }
            System.out.println(entry.getKey());
        }
    }

    private void fillMaps() {
        // Hardcoded information about which airport listens on what port
        aiport_ID_to_port_map.put("AirportA", 8000);
        aiport_ID_to_port_map.put("AirportB", 8001);
        aiport_ID_to_port_map.put("AirportC", 8002);
        aiport_ID_to_port_map.put("AirportD", 8003);

        // Hardcoded information about which airport handles which tickets
        ticket_ID_to_airport_ID_map.put("Ticket0", "AirportA");
        ticket_ID_to_airport_ID_map.put("Ticket1", "AirportA");
        ticket_ID_to_airport_ID_map.put("Ticket2", "AirportB");
        ticket_ID_to_airport_ID_map.put("Ticket3", "AirportB");
        ticket_ID_to_airport_ID_map.put("Ticket4", "AirportB");
        ticket_ID_to_airport_ID_map.put("Ticket5", "AirportC");
        ticket_ID_to_airport_ID_map.put("Ticket6", "AirportD");
        ticket_ID_to_airport_ID_map.put("Ticket7", "AirportD");

    }

}