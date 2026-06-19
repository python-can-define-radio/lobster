#!/usr/bin/env python3
"""
Serve with no caching. Useful while developing -- changes take effect immediately
Source: MS Copilot"""
from http.server import HTTPServer, SimpleHTTPRequestHandler

class NoCacheHTTPRequestHandler(SimpleHTTPRequestHandler):
    def end_headers(self):
        # Add no-cache headers to every response
        self.send_header("Cache-Control", "no-cache, no-store, must-revalidate")
        self.send_header("Pragma", "no-cache")
        self.send_header("Expires", "0")
        super().end_headers()

if __name__ == "__main__":
    server = HTTPServer(("localhost", 8000), NoCacheHTTPRequestHandler)
    print("Serving on http://localhost:8000 with no-cache headers")
    server.serve_forever()
