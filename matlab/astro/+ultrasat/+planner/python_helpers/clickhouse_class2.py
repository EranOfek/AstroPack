# Example how to use Python class from MATLAB
# Chen Tishler, 04/08/2024

import clickhouse_driver
from datetime import datetime, timezone, timedelta

class DBHelper:
    def __init__(self, client):
        self.client = client
        return

    
    def select(self, query):
        def add_timezone_info(rows):
            converted = []
            for row in rows:
                new_row = []
                for value in row:
                    if isinstance(value, datetime):
                        # Add timezone information, replace timedelta with your actual offset
                        value = value.replace(tzinfo=timezone(timedelta(seconds=10800)))
                        new_row.append(value)
                    else:
                        new_row.append(value)
                converted.append(tuple(new_row))
            return converted
        
        result = self.client.execute(query)
        result = add_timezone_info(result)
        return result
