from http.server import SimpleHTTPRequestHandler, HTTPServer
import threading
import json
import time
from socketserver import ThreadingMixIn

from Transaction import Transaction


class ThreadedHTTPServer(ThreadingMixIn, HTTPServer):
    """Handle requests in a separate thread."""


class AirportSupervisor:
    def __init__(self, airport_ID, ticket_quantities):
        # server instance
        # "" means to listen on all available interfaces
        print('Creating supervisor. Airport_ID: {0}\nTicket_quantities: {1}'.format(airport_ID, ticket_quantities))
        self.data_manager = TicketDataManager(airport_ID, ticket_quantities)
        self.handler_class = make_handler_class_from_argv(self.data_manager)

        # Check if airport ID is defined in dict.
        if airport_ID in self.data_manager.airport_ID_to_port_map:
            self.http_server = ThreadedHTTPServer(('', self.data_manager.airport_ID_to_port_map[airport_ID]), self.handler_class)
        else:
            raise Exception("Airport ID not found")
        self.should_shutdown = False


        # server thread, for easier cleanup
        self.server_thread = threading.Thread(target=self.http_server.serve_forever)

    def start_server(self):
        self.server_thread.start()
        self.should_shutdown = False
        self.handle_requests_until_shutdown()

    def stop_server(self):
        self.should_shutdown = True
        self.http_server.shutdown()
        self.server_thread.join()

    def handle_requests_until_shutdown(self):
        while not self.should_shutdown:
            for transaction in self.data_manager.registered_transactions:
                #print(transaction.ID)
                time.sleep(2)



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
        my_ticket_list = []

        for key, value in self.ticket_ID_to_airport_ID_map.items():
            if value == ID:
                my_ticket_list.append(key)
                print(key)

        self.ticket_reserved_to_transaction_list_map = {x: [] for x in my_ticket_list}
        self.ticket_completed_to_transaction_list_map = {x: [] for x in my_ticket_list}
        self.registered_transactions = []


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

        # POST method override, used to communicate with servers / clients
        def do_POST(self):
            print('received POST')
            content_length = 0
            for header in self.headers:
                if header == 'Content-Length':
                    content_length = int(self.headers[header])
            print(content_length)
            received_data = self.rfile.read(content_length)

            if self.path == '/register_transaction':
                self.register_transaction(received_data)

            # decoded_object = json.loads(received_bytes)
            # print(decoded_object)
            self.send_response(200)
            self.end_headers()
            self.wfile.write('POST accepted'.encode())

        # GET method override, used to print data through browser
        def do_GET(self):
            self.send_response(200)
            self.end_headers()
            return_string = 'Dane serwera {0}\n'.format(data_manager.ID)
            for transaction in self.data_manager.registered_transactions:
                return_string += str(transaction)
            self.wfile.write(return_string.encode())

        def register_transaction(self, received_data):
            received_json = json.loads(received_data)
            incoming_transaction = Transaction(ID=received_json['transaction_ID'],
                                               ticket_ID_list=received_json['required_tickets'],
                                               home_server_ID=self.data_manager.ID)
            self.data_manager.registered_transactions.append(incoming_transaction)

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
