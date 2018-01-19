import java.util.HashMap;
import java.util.Map;

class server_setup {

    static Map<String, Integer> airport_a_ticket_quantity_dict = new HashMap<>();
    static Map<String, Integer> airport_b_ticket_quantity_dict = new HashMap<>();
    static Map<String, Integer> airport_c_ticket_quantity_dict = new HashMap<>();
    static Map<String, Integer> airport_d_ticket_quantity_dict = new HashMap<>();

    static Map<String, Map<String, Integer>> airport_data = new HashMap<>();

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

            Integer startPort = 8000;
            int i = 0;
            for (Map.Entry<String, Map<String, Integer>> entry : airport_data.entrySet())
            {
                ticket_server.run(entry.getKey(), entry.getValue(), startPort + i);
                ++i;
            }
        }
        catch (Exception e) {
            System.out.println(e);
        }
    }
}