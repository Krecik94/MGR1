from threading import Thread
import ticket_server


def main():
    server_threads = []

    airport_a_ticket_quantity_dict = {
        'Ticket0': 8,
        'Ticket1': 8
    }

    airport_b_ticket_quantity_dict = {
        'Ticket2': 1,
        'Ticket3': 1,
        'Ticket4': 1
    }

    airport_c_ticket_quantity_dict = {
        'Ticket5': 2
    }

    airport_d_ticket_quantity_dict = {
        'Ticket6': 1,
        'Ticket7': 1
    }

    airport_data = {'AirportA': airport_a_ticket_quantity_dict,
                    'AirportB': airport_b_ticket_quantity_dict,
                    'AirportC': airport_c_ticket_quantity_dict,
                    'AirportD': airport_d_ticket_quantity_dict}

    for airport_ID, airport_data in airport_data.items():
        server_threads.append(Thread(target=ticket_server.main, args=[airport_ID, airport_data]))

    for server_thread in server_threads:
        server_thread.start()


if __name__ == '__main__':
    main()
