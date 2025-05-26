from flask import Flask, request, Response, jsonify
import clickhouse_connect
import argparse
import logging
from datetime import datetime, timezone
import base64
from concurrent.futures import ThreadPoolExecutor
import threading

# Initialize Flask app
app = Flask(__name__)
client = None
cli_args = None  # holds parsed args
executor = ThreadPoolExecutor(max_workers=10)
lock = threading.Lock()

def extract_db_and_table(path):
    """Extract database and table name from the URL path."""
    try:
        db, table = path.strip('/').split('.', 1)
        return db, table
    except ValueError:
        return None, None

def now_julian_day():
    """Return current UTC time as Julian Day (float64)."""
    dt = datetime.now(timezone.utc)
    unix_epoch = dt.timestamp()
    return 2440587.5 + (unix_epoch / 86400.0)

def get_authenticated_user_and_password():
    """Extract username and password from HTTP Basic Auth."""
    auth_header = request.headers.get('Authorization')
    if not auth_header or not auth_header.startswith('Basic '):
        return None, None
    encoded = auth_header.split(' ')[1]
    decoded = base64.b64decode(encoded).decode('utf-8')
    username, password = decoded.split(':', 1)
    return username, password

def insert_row(db, table, row_dict):
    try:
        with lock:
            result = client.query(f"EXISTS TABLE {db}.{table}")
        if result.result_rows[0][0] == 0:
            return f"Table {db}.{table} doesn't exist", 404

        columns = list(row_dict.keys())
        values = [row_dict[k] for k in columns]
        with lock:
            client.insert(f'{db}.{table}', [values], column_names=columns)
        logging.info(f"Inserted into {db}.{table}: {row_dict}")
        return f"Inserted into {db}.{table}", 200
    except Exception as e:
        return f"Insert error: {e}", 500

@app.route('/<path:req_path>', methods=['POST', 'GET'])
def handle_request(req_path):
    """
    Handle incoming POST or GET request:
    - Parse DB/table
    - Accept JSON body (POST) or query parameters (GET)
    - Add ingestion time if configured
    - Add authenticated username
    - Validate and insert each row in parallel (for POST) or single row (for GET)
    """
    db, table = extract_db_and_table(req_path)
    if not db or not table:
        return "Invalid path format. Use /db.table\n", 400

    auth_user, auth_pass = get_authenticated_user_and_password()
    if not auth_user or not auth_pass:
        return Response("Unauthorized\n", status=401, headers={'WWW-Authenticate': 'Basic'})
    if auth_pass != cli_args.userPassword:
        return Response("Forbidden: invalid password\n", status=403)

    if request.method == 'GET':
        args_dict = request.args.to_dict()
        if not args_dict and not cli_args.ingestTime:
            return "No query parameters provided\n", 400

        if cli_args.ingestTime:
            jd = now_julian_day()
            args_dict[cli_args.ingestTime] = f"{jd:.10f}"

        user_col = cli_args.userNameColumn if cli_args.userNameColumn else 'user'
        args_dict[user_col] = auth_user

        msg, code = insert_row(db, table, args_dict)
        return msg + "\n", code

    # POST method
    try:
        data = request.get_json()
    except Exception:
        return "Invalid JSON body\n", 400

    if not data:
        return "Empty JSON body\n", 400

    if not isinstance(data, list):
        data = [data]

    futures = []
    for row in data:
        if not isinstance(row, dict):
            return "Invalid row format; must be dict\n", 400

        if cli_args.ingestTime:
            jd = now_julian_day()
            row[cli_args.ingestTime] = f"{jd:.10f}"

        user_col = cli_args.userNameColumn if cli_args.userNameColumn else 'user'
        row[user_col] = auth_user

        futures.append(executor.submit(insert_row, db, table, row))

    results = [f.result() for f in futures]
    response = [{"status": msg, "code": code} for msg, code in results]
    return jsonify(response), 207  # Multi-Status

def main():
    """Parse CLI arguments, connect to ClickHouse, and start Flask server."""
    global client, cli_args

    parser = argparse.ArgumentParser(description="HTTP listener that writes to ClickHouse.")
    parser.add_argument('--host', required=True, help='ClickHouse hostname')
    parser.add_argument('--port', type=int, default=8123, help='ClickHouse port')
    parser.add_argument('--user', required=True, help='ClickHouse username')
    parser.add_argument('--password', required=True, help='ClickHouse password')
    parser.add_argument('--listen_port', type=int, default=8080, help='Port to run the HTTP server on')
    parser.add_argument('--ingestTime', help='Column name to store ingestion time as Julian Day (Float64)')
    parser.add_argument('--userNameColumn', default='user', help='Column name to store the authenticated username')
    parser.add_argument('--userPassword', default='MyPassword', help='Password required from HTTP clients')

    cli_args = parser.parse_args()

    logging.basicConfig(level=logging.INFO)

    # Establish connection to ClickHouse
    client = clickhouse_connect.get_client(
        host=cli_args.host,
        port=cli_args.port,
        username=cli_args.user,
        password=cli_args.password
    )

    print(f"Connected to ClickHouse at {cli_args.host}:{cli_args.port} as {cli_args.user}")
    app.run(port=cli_args.listen_port)

if __name__ == '__main__':
    main()
