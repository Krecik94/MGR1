from http.server import SimpleHTTPRequestHandler, HTTPServer
import threading
import time
""""
Test handler that returns requestline
After launching the server call localhost:8000 in browser to test if it's working
"""

class AirportServer():

    def __init__(self):
        # Table of available tickets linked to transactions
        self.IMPLEMENT_THIS = "test"
        # TODO: think about data structure

        #TODO: initialize handler thread for easier shutdown
        self.http_server = HTTPServer(("", 8000), TicketServerRequestHandler)
        self.server_thread = threading.Thread(target=self.http_server.serve_forever)

    def start_server(self):
        self.server_thread.start()

    def stop_server(self):
        self.http_server.shutdown()
        self.server_thread.join()

class TicketServerRequestHandler(SimpleHTTPRequestHandler):

    def do_method(self, caller_method_name):
        print(caller_method_name)


        self.send_response(200)
        self.end_headers()
        self.wfile.write(self.requestline.encode())

    def do_POST(self):
        self.do_method("POST")

    # GET method override
    def do_GET(self):
        self.do_method("GET")


def main():

    test=AirportServer()

    test.start_server()
    time.sleep(4)
    test.stop_server()

if __name__=="__main__":
    main()