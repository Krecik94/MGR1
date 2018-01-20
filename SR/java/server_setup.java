import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

class server_setup {

    static Map<String, Integer> airport_a_ticket_quantity_dict = new HashMap<>();
    static Map<String, Integer> airport_b_ticket_quantity_dict = new HashMap<>();
    static Map<String, Integer> airport_c_ticket_quantity_dict = new HashMap<>();
    static Map<String, Integer> airport_d_ticket_quantity_dict = new HashMap<>();

    static Map<String, Map<String, Integer>> airport_data = new HashMap<>();

    static List<Thread> threadList = new ArrayList<Thread>(){};

    private static void fillMaps() {
        airport_a_ticket_quantity_dict.put("Ticket0", 8);
        airport_a_ticket_quantity_dict.put("Ticket1", 8);

        airport_b_ticket_quantity_dict.put("Ticket2", 1);
        airport_b_ticket_quantity_dict.put("Ticket3", 1);
        airport_b_ticket_quantity_dict.put("Ticket4", 1);

        airport_c_ticket_quantity_dict.put("Ticket5", 2);

        airport_d_ticket_quantity_dict.put("Ticket6", 1);
        airport_d_ticket_quantity_dict.put("Ticket7", 1);

        airport_data.put("AirportA", airport_a_ticket_quantity_dict);
        airport_data.put("AirportB", airport_b_ticket_quantity_dict);
        airport_data.put("AirportC", airport_c_ticket_quantity_dict);
        airport_data.put("AirportD", airport_d_ticket_quantity_dict);

    }

    public static void main(String args[]) {
        try {
            fillMaps();

            Thread t;
            for (Map.Entry<String, Map<String, Integer>> entry : airport_data.entrySet())
            {
                t = new Thread(new TicketServer(entry.getKey(), entry.getValue()));
                t.run();
                threadList.add(t);
            }
        }
        catch (Exception e) {
            System.out.println(e);
        }
    }
}