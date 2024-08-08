# Example how to use Python function from MATLAB
# Chen Tishler, 04/08/2024

import clickhouse_driver
from datetime import datetime

def select(conn, query):
    client = conn
    
    def convert_datetimes_to_string(rows):
        converted = []
        for row in rows:
            new_row = []
            for value in row:
                if isinstance(value, datetime):
                    value = value.replace(tzinfo=timezone(timedelta(seconds=10800)))
                    new_row.append(value)
                    #new_row.append(value.isoformat())
                else:
                    new_row.append(value)
            converted.append(tuple(new_row))
        return converted
    
    result = client.execute(query)
    result = convert_datetimes_to_string(result)
    return result
