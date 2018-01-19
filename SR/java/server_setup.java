
class server_setup {
    public static void main(String args[]) {
        try {
            ticket_server.run(8000);
            ticket_server.run(8001);
            ticket_server.run(8002);
        }
        catch (Exception e) {
            System.out.print(e);
        }
    }
}