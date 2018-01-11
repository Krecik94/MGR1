import urllib.request


def main():
    request_to_send = urllib.request.Request('http://localhost:8000/doesnt_matter?what/goes:here')
    # Uncomment to generate POST request
    # request_to_send.data = "test".encode()
    with urllib.request.urlopen(request_to_send) as response:
        print(response.read().decode())


if __name__ == "__main__":
    main()
