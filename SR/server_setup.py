from threading import Thread
import ticket_server


def main():
    server_threads = []

    airport_a_ticket_quantity_dict = {
        'Ticket0': 200,
        'Ticket1': 200
    }

    airport_b_ticket_quantity_dict = {
        'Ticket2': 200,
        'Ticket3': 200,
        'Ticket4': 200
    }

    airport_c_ticket_quantity_dict = {
        'Ticket5': 200
    }

    airport_d_ticket_quantity_dict = {
        'Ticket6': 200,
        'Ticket7': 200
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
