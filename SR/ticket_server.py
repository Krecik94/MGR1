from http.server import SimpleHTTPRequestHandler, HTTPServer
import threading
import time

""""
Test handler that returns requestline
After launching the server call localhost:8000 in browser to test if it's working
"""

# Globals for easier parameter injection into request handler
airport_ID = None
initial_ticket_quantities = None


class AirportSupervisor:
    def __init__(self, port):
        # server instance
        # "" means to listen on all available interfaces
        self.http_server = HTTPServer(("", port), TicketServerRequestHandler)

        # server thread, for easier cleanup
        self.server_thread = threading.Thread(target=self.http_server.serve_forever)

    def start_server(self):
        self.server_thread.start()

    def stop_server(self):
        self.http_server.shutdown()
        self.server_thread.join()


class TicketDataManager:
    # Hardcoded information about which airport listens on what port
    airport_ID_to_port_map = {
        'AirportA': 8000,
        'AirportB': 8001,
        'AirportC': 8002,
        'AirportD': 8003
    }

    # Hardcoded information about which airport handles which tickets
    ticket_ID_to_airpot_ID_map = {
        'Ticket0': 'AirportA',
        'Ticket1': 'AirportA',
        'Ticket2': 'AirportB',
        'Ticket3': 'AirportB',
        'Ticket4': 'AirportB',
        'Ticket5': 'AirportC',
        'Ticket6': 'AirportD',
        'Ticket7': 'AirportD',
    }

    def __init__(self):
        self.ID = airport_ID


class TicketServerRequestHandler(SimpleHTTPRequestHandler):

    ticket_data_manager = TicketDataManager()

    def do_method(self, caller_method_name):
        print(caller_method_name)

        self.send_response(200)
        self.end_headers()
        self.wfile.write((TicketServerRequestHandler.ticket_data_manager.ID).encode())

    # POST method override
    def do_POST(self):
        self.do_method("POST")

    # GET method override
    def do_GET(self):
        self.do_method("GET")


def main(port, ID, ticket_quantities):

    global airport_ID
    airport_ID = ID
    test = AirportSupervisor(port)

    test.start_server()
    time.sleep(4)
    test.stop_server()


if __name__ == "__main__":
    main(8000, "test_param", None)
