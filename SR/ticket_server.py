import datetime
from http.server import SimpleHTTPRequestHandler, HTTPServer
import threading
import json
import time
from socketserver import ThreadingMixIn

from Transaction import Transaction
from Transaction import TransactionStatus


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
            self.http_server = ThreadedHTTPServer(('', self.data_manager.airport_ID_to_port_map[airport_ID]),
                                                  self.handler_class)
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

    # Loop that iterates over all transactions and handles them accordingly
    def handle_requests_until_shutdown(self):
        while not self.should_shutdown:
            for transaction in self.data_manager.registered_transactions:
                # print(transaction.ID)
                if transaction.status == TransactionStatus.REGISTERED:
                    self.handle_registered_transaction(transaction)
                time.sleep(2)

    def handle_registered_transaction(self, transaction):
        # Setting transaction status to let outer loop know it is being processed
        transaction.status = TransactionStatus.PENDING_RESERVATION

        # Extracting tickets that belong to local server
        local_tickets = list(set(transaction.ticket_ID_list) & set(self.data_manager.my_ticket_list))

        # Extracting tickets that need to be acquired from remote servers
        remote_tickets = [ticket for ticket in transaction.ticket_ID_list if ticket not in local_tickets]

        # Counting if local server has enough tickets START
        has_enough_local_tickets = True
        for local_ticket in local_tickets:
            reserved_tickets = len(self.data_manager.ticket_reserved_to_transaction_list_map[local_ticket])
            purchased_tickets = len(self.data_manager.ticket_completed_to_transaction_list_map[local_ticket])
            # Doesn't work with deadlocks TODO FIX THIS
            if reserved_tickets + purchased_tickets >= self.data_manager.ticket_quantities[local_ticket]:
                has_enough_local_tickets = False
        # Counting if local server has enough tickets END

        # Reserving local tickets
        if has_enough_local_tickets:
            for local_ticket in local_tickets:
                # reserving local tickets
                self.data_manager.ticket_reserved_to_transaction_list_map[local_ticket].append(transaction)
            transaction.status = TransactionStatus.RESERVED
        else:
            # Case when there is not enough local tickets
            transaction.status = TransactionStatus.OUT_OF_TICKETS
            return

        # Case when there is no tickets to be acquired remotely, jump straight to completion
        if len(remote_tickets) == 0:
            for local_ticket in local_tickets:
                self.data_manager.ticket_reserved_to_transaction_list_map[local_ticket].remove(transaction)
                self.data_manager.ticket_completed_to_transaction_list_map[local_ticket].append(transaction)
            transaction.status = TransactionStatus.COMPLETED
            return


        # Reserving remote tickets
        # No need for 'else' since if there are no remote tickets the method will return
        successfully_reserved_remote_tickets = []
        for remote_ticket in remote_tickets:
            result = self.contact_remote_server('reserve', remote_tickets)
            # Case when ticket was reserved successfully
            if result == 'success':
                successfully_reserved_remote_tickets.append(remote_ticket)

            # Case when ticket could not be reserved, aborting
            else:
                transaction.status = TransactionStatus.ERROR
                for local_ticket in local_tickets:
                    self.data_manager.ticket_reserved_to_transaction_list_map[local_ticket].remove(transaction)

                for remote_ticket in successfully_reserved_remote_tickets:
                    self.contact_remote_server('abort', remote_tickets)
                return

        # All tickets are reserved
        transaction.status = TransactionStatus.RESERVED

        # Committing all tickets
        # Committing local tickets
        for local_ticket in local_tickets:
            # Removing from reserved list
            self.data_manager.ticket_reserved_to_transaction_list_map[local_ticket].remove(transaction)
            # Adding to completed list
            self.data_manager.ticket_completed_to_transaction_list_map[local_ticket].append(transaction)

        # Committing remote tickets
        successfully_committed_remote_tickets = []
        for remote_ticket in remote_tickets:
            result = self.contact_remote_server('commit', remote_tickets)
            # Case when ticket was reserved successfully
            if result == 'success':
                successfully_committed_remote_tickets.append(remote_ticket)

            # Case when ticket could not be committed, aborting
            else:
                transaction.status = TransactionStatus.ERROR
                for local_ticket in local_tickets:
                    self.data_manager.ticket_completed_to_transaction_list_map[local_ticket].remove(transaction)

                for remote_ticket in successfully_reserved_remote_tickets:
                    self.contact_remote_server('abort', remote_tickets)
                return

        transaction.status = TransactionStatus.COMPLETED

    # IMPLEMENT THIS
    def contact_remote_server(self, action, ticket_ID):
        return 'fail'


    '''
    TODO: 
    - obsługa rezerwacji i comita po stronie serwera
    - wysyłka rezerwacji i comita do remota
    - deadlock
    '''


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
        self.my_ticket_list = []

        for key, value in self.ticket_ID_to_airport_ID_map.items():
            if value == ID:
                self.my_ticket_list.append(key)
                print(key)

        self.ticket_reserved_to_transaction_list_map = {x: [] for x in self.my_ticket_list}
        self.ticket_completed_to_transaction_list_map = {x: [] for x in self.my_ticket_list}
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
            return_message = 'POST accepted'
            if self.path == '/register_transaction':
                return_message = self.register_transaction(received_data)
            elif self.path == '/ping':
                return_message = self.ping(received_data)

            # decoded_object = json.loads(received_bytes)
            # print(decoded_object)
            self.send_response(200)
            self.end_headers()
            self.wfile.write(return_message.encode())

        # GET method override, used to print data through browser
        def do_GET(self):
            self.send_response(200)
            self.end_headers()
            return_string = None
            return_string = '<html><body><pre>Dane serwera {0}\n'.format(data_manager.ID)
            return_string += 'Zarejestrowane transakcje:\n<hr>'
            for transaction in self.data_manager.registered_transactions:
                return_string += str(transaction)
                return_string += '<hr/>'
            return_string += '\nZarezerwowane bilety:\n'
            for ticket_ID in self.data_manager.ticket_reserved_to_transaction_list_map.keys():
                return_string += '\n{0}:'.format(ticket_ID)
                for transaction in self.data_manager.ticket_reserved_to_transaction_list_map[ticket_ID]:
                    return_string += '\n{0}'.format(transaction.ID)
            return_string += '\n<hr>Kupione bilety:\n'
            for ticket_ID in self.data_manager.ticket_completed_to_transaction_list_map.keys():
                return_string += '\n{0}:'.format(ticket_ID)
                for transaction in self.data_manager.ticket_completed_to_transaction_list_map[ticket_ID]:
                    return_string += '\n{0}'.format(transaction.ID)
            return_string += '</pre></body></html>'

            self.wfile.write(return_string.encode())

        def register_transaction(self, received_data):
            received_json = json.loads(received_data)
            incoming_transaction = Transaction(ID=received_json['transaction_ID'],
                                               ticket_ID_list=received_json['required_tickets'],
                                               home_server_ID=self.data_manager.ID)
            self.data_manager.registered_transactions.append(incoming_transaction)
            return 'Registration complete'

        def ping(self, received_data):
            received_json = json.loads(received_data)
            pinged_transaction = next(transaction for transaction in self.data_manager.registered_transactions if
                                      transaction.ID == received_json['transaction_ID'])
            pinged_transaction.last_ping_timestamp = datetime.datetime.now()

            if pinged_transaction.status == TransactionStatus.COMPLETED:
                pinged_transaction.status = TransactionStatus.ACKNOWLEDGED

            return str(pinged_transaction.status)

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
