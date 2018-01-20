import urllib.request
import json
import uuid
import threading
import time
import random

safety_lock = threading.RLock()

ticket_list = ['Ticket0',
               'Ticket2',
               'Ticket5',
               ]

port_list = [8000, 8001, 8002, 8003]

tickets_to_buy = {
    'Ticket0': 0,
    'Ticket2': 0,
    'Ticket5': 0,
}

tickets_bought = {'Ticket0': 0,
                  'Ticket2': 0,
                  'Ticket5': 0,
                  }


def create_random_permutation():
    safe_construct_fresh_list = []
    for ticket in ticket_list:
        safe_construct_fresh_list.append(ticket)
    random.shuffle(safe_construct_fresh_list)
    print(safe_construct_fresh_list)
    return safe_construct_fresh_list


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

def diagnostic_loop():
    while threading.active_count() > 2:
        # -2 because main thread, and diagnostic thread
        print('Active worker threads: {0}'.format(threading.active_count()-2))
        time.sleep(1)

def main():
    thread_pool = []
    diagnostic_thread = threading.Thread(target=diagnostic_loop)
    for i in range(21):
        ticket_list_to_thread = create_random_permutation()
        for ticket in ticket_list_to_thread:
            tickets_to_buy[ticket] += 1
        port_to_thread = random.choice(port_list)
        thread_pool.append(threading.Thread(target=send_request, args=[ticket_list_to_thread, port_to_thread]))

    for thread in thread_pool:
        thread.start()

    diagnostic_thread.start()

    for thread in thread_pool:
        thread.join()

    diagnostic_thread.join()

    print(tickets_to_buy)
    print(tickets_bought)


if __name__ == "__main__":
    main()
