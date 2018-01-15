import urllib.request
import json

def main():
    request_to_send = urllib.request.Request('http://localhost:8000/doesnt_matter?what/goes:here')
    # Uncomment to generate POST request
    # request_to_send.data = "test".encode()
    test={'a':['test', 'test1'], 'b':'test2'}
    request_to_send.data = json.dumps(test).encode()
    with urllib.request.urlopen(request_to_send) as response:
        print(response.read().decode())


if __name__ == "__main__":
    main()
