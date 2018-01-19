
public enum TransactionStatus {
    // Initial state of transaction.
    REGISTERED,

    // State after all reservation requests have been sent out
    PENDING_RESERVATION,

    // State after all reservations have been confirmed
    RESERVED,

    // State after all confirmations have been completed with success - transaction finished
    COMPLETED,

    // Transaction rejected because it was causing a deadlock
    DEADLOCKED,

    // There are no more tickets available for the transaction to purchase
    OUT_OF_TICKETS,

    //Something went wrong, rollback
    ERROR,

    // When transaction was completed but client never found out and disconnected. Abort transaction.
    ACKNOWLEDGED
}