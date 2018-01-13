from http.server import SimpleHTTPRequestHandler, HTTPServer
import threading
import time

""""
Test handler that returns requestline
After launching the server call localhost:8000 in browser to test if it's working
"""


class AirportSupervisor:
    def __init__(self, port, ID):
        # server instance
        # "" means to listen on all available interfaces
        data_manager = TicketDataManager(ID)
        handler_class = make_handler_class_from_argv(data_manager)
        self.http_server = HTTPServer(("", port), handler_class)

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
    ticket_ID_to_airport_ID_map = {
        'Ticket0': 'AirportA',
        'Ticket1': 'AirportA',
        'Ticket2': 'AirportB',
        'Ticket3': 'AirportB',
        'Ticket4': 'AirportB',
        'Ticket5': 'AirportC',
        'Ticket6': 'AirportD',
        'Ticket7': 'AirportD',
    }

    def __init__(self, ID):
        self.ID = ID


def make_handler_class_from_argv(data_manager):
    class TicketServerRequestHandler(SimpleHTTPRequestHandler):

        def __init__(self, *args, **kwargs):
            self.data_manager = data_manager
            super(TicketServerRequestHandler, self).__init__(*args, **kwargs)

        def do_method(self, caller_method_name):
            print(caller_method_name)
            self.data_manager.ID += '1'
            self.send_response(200)
            self.end_headers()
            self.wfile.write(self.data_manager.ID.encode())

        # POST method override
        def do_POST(self):
            self.do_method("POST")

        # GET method override
        def do_GET(self):
            self.do_method("GET")

    return TicketServerRequestHandler


def main(port, ID, ticket_quantities):
    test = AirportSupervisor(port, ID)

    test.start_server()
    # time.sleep(4)
    # test.stop_server()


if __name__ == "__main__":
    main(8000, "test_param", None)
