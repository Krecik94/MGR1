from http.server import SimpleHTTPRequestHandler, HTTPServer

""""
Test handler that returns requestline
After launching the server call localhost:8000 in browser to test if it's working
"""


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


# Creating server instance
httpd = HTTPServer(("", 8000), TicketServerRequestHandler)

# Launching the server (no option to stop, to stop call 'shutdown' from another thread)
httpd.serve_forever()
