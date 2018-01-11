from http.server import SimpleHTTPRequestHandler, HTTPServer

""""
Test handler that returns requestline
After launching the server call localhost:8000 in browser to test if it's working
"""


class MyHandler(SimpleHTTPRequestHandler):
    # GET method override
    def do_GET(self):
        self.send_response(200)
        self.end_headers()
        self.wfile.write(self.requestline.encode())


# Creating server instance
httpd = HTTPServer(("", 8000), MyHandler)

# Launching the server (no option to stop, to stop call 'shutdown' from another thread)
httpd.serve_forever()
