import urllib.request
import json
import uuid
import threading
import time
import random

safety_lock = threading.RLock()

ticket_list = ['Ticket0',
               'Ticket1',
               'Ticket2',
               'Ticket3',
               'Ticket4',
               'Ticket5',
               'Ticket6',
               'Ticket7'
               ]

port_list = [8000,8001,8002,8003]

tickets_to_buy = {
    'Ticket0': 0,
    'Ticket1': 0,
    'Ticket2': 0,
    'Ticket3': 0,
    'Ticket4': 0,
    'Ticket5': 0,
    'Ticket6': 0,
    'Ticket7': 0,
}

tickets_bought = {'Ticket0': 0,
                  'Ticket1': 0,
                  'Ticket2': 0,
                  'Ticket3': 0,
                  'Ticket4': 0,
                  'Ticket5': 0,
                  'Ticket6': 0,
                  'Ticket7': 0,
                  }


def create_random_ticket_list():
    how_many_tickets = random.randint(1, 7)
    random_ticket_list = random.sample(set(ticket_list), how_many_tickets)
    return random_ticket_list


def send_request(my_ticket_list, server_port):
    # Creating request object with servers URL
    request_to_send = urllib.request.Request('http://localhost:{0}/register_transaction'.format(server_port))

    # Preparing data of the request
    # UUID4 ot ensure no collisions
    transaction_ID = str(uuid.uuid4())
    requried_tickets = my_ticket_list
    data_to_json = {"transaction_ID": transaction_ID,
                    "required_tickets": requried_tickets
                    }

    encoded_data = json.dumps(data_to_json).encode()
    request_to_send.data = encoded_data

    with urllib.request.urlopen(request_to_send) as response:
        print(response.read().decode())
    counter = 20

    while True:
        request_to_send = urllib.request.Request('http://localhost:{0}/ping'.format(server_port))
        data_to_json = {"transaction_ID": transaction_ID}
        request_to_send.data = json.dumps(data_to_json).encode()
        with urllib.request.urlopen(request_to_send) as response:
            received_response_string = response.read().decode()

            if received_response_string == 'TransactionStatus.ACKNOWLEDGED':
                for ticket in my_ticket_list:
                    with safety_lock:
                        tickets_bought[ticket] += 1
                break

            if received_response_string in ['TransactionStatus.DEADLOCKED', 'TransactionStatus.OUT_OF_TICKETS',
                                            'TransactionStatus.ERROR']:
                break

        time.sleep(3)


def main():
    thread_pool = []

    for i in range(20):
        ticket_list_to_thread= create_random_ticket_list()
        for ticket in ticket_list_to_thread:
            tickets_to_buy[ticket] += 1
        port_to_thread= random.choice(port_list)
        thread_pool.append(threading.Thread(target=send_request, args=[ticket_list_to_thread,port_to_thread]))

    for thread in thread_pool:
        thread.start()

    for thread in thread_pool:
        thread.join()

    print(tickets_to_buy)
    print(tickets_bought)

if __name__ == "__main__":
    main()
