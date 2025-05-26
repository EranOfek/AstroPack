// javac SimpleHttpLogger.java
import com.sun.net.httpserver.*;

import java.io.*;
import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;
import java.time.LocalDateTime;

public class SimpleHttpLogger {

    public static void start(int port, String logFilePath) throws IOException {
        HttpServer server = HttpServer.create(new InetSocketAddress(port), 0);
        server.createContext("/", new MyHandler(logFilePath));
        server.setExecutor(null);
        server.start();
        System.out.println("HTTP server started on port " + port);
    }

    static class MyHandler implements HttpHandler {
        private final String logFile;

        public MyHandler(String logFilePath) {
            this.logFile = logFilePath;
        }

        @Override
        public void handle(HttpExchange exchange) throws IOException {
            String uri = exchange.getRequestURI().toString();
            String path = exchange.getRequestURI().getPath();
            String query = exchange.getRequestURI().getQuery();
            String method = exchange.getRequestMethod();
            String time = LocalDateTime.now().toString();

            StringBuilder log = new StringBuilder();
            log.append(time).append(" ").append(method).append(" ").append(path);

            if (query != null) {
                log.append("?");
		String[] pairs = query.split("&");
		for (int i = 0; i < pairs.length; i++) {
		    String[] kv = pairs[i].split("=");
		    if (kv.length == 2) {
			String key = java.net.URLDecoder.decode(kv[0], "UTF-8");
			String value = java.net.URLDecoder.decode(kv[1], "UTF-8");
			log.append(key).append("=").append(value);
			if (i < pairs.length - 1) {
			    log.append("&");  // only between pairs
			}
		    }
		}
            }

            log.append("\n");

            try (FileWriter fw = new FileWriter(logFile, true)) {
                fw.write(log.toString());
            }

            String response = "OK";
            exchange.sendResponseHeaders(200, response.length());
            OutputStream os = exchange.getResponseBody();
            os.write(response.getBytes(StandardCharsets.UTF_8));
            os.close();
        }
    }

    public static void main(String[] args) {
        if (args.length < 2) {
            System.out.println("Usage: java SimpleHttpLogger <port> <logFilePath>");
            return;
        }
        int port = Integer.parseInt(args[0]);
        String logFilePath = args[1];

        try {
            start(port, logFilePath);
        } catch (IOException e) {
            System.err.println("Failed to start HTTP server: " + e.getMessage());
            e.printStackTrace();
        }
    }
}
