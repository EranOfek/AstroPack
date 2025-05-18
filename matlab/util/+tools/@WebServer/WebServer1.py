"""
HTTP to ClickHouse Ingestion Server

This Python script launches a lightweight HTTP server using Flask. It accepts GET requests of the form:
    http://<host>:<port>/<database>.<table>?key1=value1&key2=value2...

Functionality:
- Parses the database and table name from the URL path.
- Parses query parameters as column names and values.
- Optionally adds a Julian Day timestamp to the row using --ingestTime argument.
- Adds the authenticated HTTP username as a column (--userNameColumn, default is 'user').
- Requires all HTTP requests to authenticate with a password (--userPassword, default is 'MyPassword').
- Does not create tables; returns an error if the target table doesn't exist.
- Inserts the data into ClickHouse using clickhouse-connect.

Usage Example:
    python3 WebServer.py --host socsrv --port 8123 --user default --password passRoot --listen_port 8090 --ingestTime ingestion_jd --userNameColumn user --userPassword MyPassword

Dependencies:
- Flask
- clickhouse-connect

Author: Eran Ofek
"""

from flask import Flask, request, Response
import clickhouse_connect
import argparse
import logging
from datetime import datetime, timezone
import base64

# Initialize Flask app
app = Flask(__name__)
client = None
cli_args = None  # holds parsed args

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

@app.route('/<path:req_path>', methods=['GET'])
def handle_request(req_path):
    """
    Handle incoming GET request:
    - Parse DB/table
    - Parse query args
    - Optionally add ingestion time
    - Add authenticated username to row
    - Check that table exists (do not create)
    - Insert row into ClickHouse
    """
    db, table = extract_db_and_table(req_path)
    if not db or not table:
        return "Invalid path format. Use /db.table\n", 400

    args_dict = request.args.to_dict()
    if not args_dict and not cli_args.ingestTime:
        return "No query parameters provided\n", 400

    # Add ingestion timestamp if requested
    if cli_args.ingestTime:
        jd = now_julian_day()
        args_dict[cli_args.ingestTime] = f"{jd:.10f}"

    # Authenticate user and validate password
    auth_user, auth_pass = get_authenticated_user_and_password()
    if not auth_user or not auth_pass:
        return Response("Unauthorized\n", status=401, headers={'WWW-Authenticate': 'Basic'})

    if auth_pass != cli_args.userPassword:
        return Response("Forbidden: invalid password\n", status=403)

    user_col = cli_args.userNameColumn if cli_args.userNameColumn else 'user'
    args_dict[user_col] = auth_user

    # Check if table exists
    try:
        result = client.query(f"EXISTS TABLE {db}.{table}")
        if result.result_rows[0][0] == 0:
            return f"Table {db}.{table} doesn't exist\n", 404
    except Exception as e:
        return f"Error checking table existence: {e}\n", 500

    # Prepare and insert row
    columns = list(args_dict.keys())
    values = [args_dict[k] for k in columns]
    client.insert(f'{db}.{table}', [values], column_names=columns)

    logging.info(f"Inserted into {db}.{table}: {args_dict}")
    return f"Inserted into {db}.{table}\n"

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
