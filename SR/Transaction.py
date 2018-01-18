from enum import Enum

import datetime


class TransactionStatus(Enum):
    # Initial state of transaction.
    REGISTERED = 1

    # State after all reservationr requests have been sent out
    PENDING_RESERVATION = 2

    # State after all reservations have been confirmed
    RESERVED = 3

    # State after all confirmation messages have been sent out
    PENDING_COMPLETION = 4

    # State after all confirmations have been completed with success - transaction finished
    COMPLETED = 5

    # Transaction rejected because it was causing a deadlock
    DEADLOCKED = 6

    # There are no more tickets available for the transaction to purchase
    OUT_OF_TICKETS = 7

    # Something went wrong, rollback
    ERROR = 8


class Transaction:
    """
    Class representing single transaction.
    All tickets from ticket ID list must be reserved or purchased, or whole transaction should be rejected.
    """

    def __init__(self, ID, ticket_ID_list, home_server_ID, status=TransactionStatus.REGISTERED):
        # ID of the transaction
        self.ID = ID

        # IDs of all tickets transaction wants to buy
        self.ticket_ID_list = ticket_ID_list

        # ID of initial server that transaction was registered on
        # Client will ping that server until transaction isn't resolved
        self.home_server_ID = home_server_ID

        # Transaction status, should be REGISTERED unless constructed in special circumstances
        self.status = status

        # Timestamp to keep track of client activity. Used to detect disconnects
        self.last_ping_timestamp = datetime.datetime.now()

    def __str__(self):
        return_string = "\nID: {0}\n".format(self.ID)
        return_string += "Ticket list: {0}\n".format(self.ticket_ID_list)
        return_string += "Home server: {0}\n".format(self.home_server_ID)
        return_string += "Status: {0}\n".format(self.status)
        return_string += "Last ping:\n\n".format(self.last_ping_timestamp)
        return return_string
