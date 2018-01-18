import urllib.request
import json
import uuid


'''
TODO:
- rezerwacja biletów po otrzymaniu transakcji
- finalizacja transakcji
- obsługa pingów
- obsługa deadlocków
'''

def main():
    # Creating request object with servers URL
    request_to_send = urllib.request.Request('http://localhost:8000/register_transaction')

    # Preparing data of the request
    # UUID4 ot ensure no collisions
    transaction_ID = str(uuid.uuid4())
    requried_tickets = ["Ticket0", "Ticket5"]
    data_to_json = {"transaction_ID": transaction_ID,
                    "required_tickets": requried_tickets
                    }

    print(json.dumps(data_to_json))
    encoded_data = json.dumps(data_to_json).encode()
    request_to_send.data = json.dumps(data_to_json).encode()
    with urllib.request.urlopen(request_to_send) as response:
        print(response.read().decode())


if __name__ == "__main__":
    main()
