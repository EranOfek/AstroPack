#
# gcsdb.py - Data structures and Database connectivity
#
#
# Classes in this file:
#
#   DbConnetion
#   DbQuery
#   Database
#   EventData
#
#   ImageData
#   TelemetryData
#

import os, time, pytz
from datetime import datetime
import psycopg2

from gcsbase import Component

# GCS Data Types


# ===========================================================================

class Database(Component):

    # Constructor
    def __init__(self):
        super().__init__()
        self.name = 'Database'
        self.dbcon = DbConnetion()

    # Destructor
    def __del__(self):
        # Deleted
        pass

    def new_query(self):
        query = DbQuery(self.dbcon)
        return query

    # -----------------------------------------------------------------------
    #                                   Insert
    # -----------------------------------------------------------------------
    # Insert log record
    def insert_log(self, data):
        pass

    # Insert new event record
    def insert_event(self, data):
        pass

    # Insert incoming image
    def insert_image(self, data):
        pass

    # Insert incoming image
    def insert_telemetry(self, data):
        pass

    # -----------------------------------------------------------------------
    #                                   Select
    # -----------------------------------------------------------------------

    #
    def select_next_oper(self):
        pass

    #
    def update_oper_status(self, oper):
        pass

    # Select
    def select_pending_plan(self):
        pass

    #
    def update_plan_status(self):
        pass


    # -----------------------------------------------------------------------
    #                                   State
    # -----------------------------------------------------------------------

    #
    def save_state(self, state):
        pass

    #
    def load_state(self):
        query = DbQuery()
        result = query.query("SELECT * FROM gcs_state WHERE key='state'")
        if len(result) > 0:
            rec = result[0]
            yml = rec['value']

        pass



# ===========================================================================
#
# ===========================================================================
# Database Connection
class DbConnetion(Component):

    def __init__(self, host='gauss'):
        super().__init__()
        self.name = 'DbConnetion'
        self.con = psycopg2.connect(database='socgcs', user='postgres', password='PassRoot', host='gauss', port='5432')
        #self.con = psycopg2.connect(database='socgcs', user='admin', password='Passw0rd', host='gauss', port='5432')
        self.log('Database connected successfully')


    def __del__(self):
        self.con.close()


# Database Query
class DbQuery(Component):
    def __init__(self, con=None):
        super().__init__()
        self.name = 'DbQuery'
        self.dbcon = con
        self.cur = self.dbcon.con.cursor()
        self.columns = []

    def __del__(self):
        #self.cur.close()
        pass

    #
    # cur.execute("INSERT INTO test (num, data) VALUES (%s,%s);", (74, u))
    def exec(self, sql_text, vars=None):
        self.cur.execute(sql_text, vars)
        self.con.commit()
        result = True
        return result

    #
    # https://stackoverflow.com/questions/21158033/query-from-postgresql-using-python-as-dictionary
    def query(self, sql_text, vars=None, fetch=True):
        self.cur.execute(sql_text, vars)
        self.columns = list(self.cur.description)
        if fetch:
            result = self.cur.fetchall()

            # make dict
            results = []
            for row in result:
                row_dict = {}
                for i, col in enumerate(self.columns):
                    row_dict[col.name] = row[i]
                results.append(row_dict)

            return results
        else:
            result = True
            return result

# ===========================================================================
#
# ===========================================================================
# GCS Event Data

class EventData:

    # Constructor
    def __init__(self):
        self.uuid = ''
        self.time = 0
        self.params = {}


# ===========================================================================

# ===========================================================================



class ImageData:

    def __init__(self):
        self.uuid = ''
        self.time = 0
        self.size = 0
        self.type = 0
        self.filename = ''
        self.params = {}


# ===========================================================================

# ===========================================================================

class TelemetryData:

    def __init__(self):
        self.uuid = ''
        self.time = 0
        self.size = 0
        self.type = 0
        self.filename = ''
        self.params = {}


# ===========================================================================
#
# ===========================================================================

# Generate reports?
class Reports(Component):

    # Constructor
    def __init__(self):
        self.interface_name = ''


# ===========================================================================
#
# ===========================================================================
