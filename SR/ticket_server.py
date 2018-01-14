from http.server import SimpleHTTPRequestHandler, HTTPServer
import threading
import time


class AirportSupervisor:
    def __init__(self, airport_ID, ticket_quantities):
        # server instance
        # "" means to listen on all available interfaces
        data_manager = TicketDataManager(airport_ID, ticket_quantities)
        handler_class = make_handler_class_from_argv(data_manager)

        # Check if airport ID is defined in dict.
        if airport_ID in data_manager.airport_ID_to_port_map:
            self.http_server = HTTPServer(("", data_manager.airport_ID_to_port_map[airport_ID]), handler_class)
        else:
            raise Exception("Airport ID not found")

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

    def __init__(self, ID, ticket_quantities):
        self.ID = ID
        self.ticket_quantities = ticket_quantities


def make_handler_class_from_argv(data_manager):
    """
    Class factory.
    :param data_manager: data manager to inject into resulting request handler class
    :return: Request handler class with data manager injected into it
    """

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


def main(airport_ID, ticket_quantities):
    airport_supervisor = AirportSupervisor(airport_ID, ticket_quantities)

    airport_supervisor.start_server()
    # time.sleep(4)
    # test.stop_server()


if __name__ == "__main__":
    ticket_quantities_dict = {
        'Ticket0': 1,
        'Ticket1': 1
    }
    main('AirportA', "test_param", ticket_quantities_dict)
