from enum import Enum

import datetime


class TransactionStatus(Enum):
    # Initial state of transaction.
    REGISTERED = 0

    # State after all reservation requests have been sent out
    PENDING_RESERVATION = 1

    # State after all reservations have been confirmed
    RESERVED = 2

    # State after all confirmations have been completed with success - transaction finished
    COMPLETED = 3

    # Transaction rejected because it was causing a deadlock
    DEADLOCKED = 4

    # There are no more tickets available for the transaction to purchase
    OUT_OF_TICKETS = 5

    # Something went wrong, rollback
    ERROR = 6

    # When transaction was completed but client never found out and disconnected. Abort transaction.
    ACKNOWLEDGED = 7


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
        return_string += "Last ping: {0}\n\n".format(self.last_ping_timestamp)
        return return_string
