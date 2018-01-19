import java.util.Date;
import java.util.List;

public class Transaction {
    /*
    Class representing single transaction.
    All tickets from ticket ID list must be reserved or purchased, or whole transaction should be rejected.
     */
    String ID;
    List<String> ticket_ID_list;
    String home_server_ID;
    TransactionStatus status;

    Date last_ping_timestamp;

    Transaction (String ID, List<String> ticket_ID_list, String home_server_ID, TransactionStatus status) {
        status = status == null ? TransactionStatus.REGISTERED : status;
        this.ID = ID;
        this.ticket_ID_list = ticket_ID_list;
        this.home_server_ID = home_server_ID;
        this.status = status;
        this.last_ping_timestamp = new Date();
    }

    public String toString() {
        String return_string = "\nID: " + ID +"\n";
        return_string += "Ticket list: " + ticket_ID_list + "\n";
        return_string += "Home server: " + home_server_ID + "\n";
        return_string += "Status: " + status + "\n";
        return_string += "Last ping: " + last_ping_timestamp + "\n\n";
        return return_string;
    }
}