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
import psycopg2, sqlite3
from psycopg2.extensions import AsIs

from gcsbase import Component

# GCS Data Types

POSTGRES = 'postgres'
SQLITE = 'sqlite'

# ===========================================================================

class DbConfig:

    def __init__(self):
        self.dbtype = ''
        self.host = ''
        self.port = ''
        self.database = ''
        self.user = ''
        self.password = ''
        self.set_postgres()


    # Setup for Postgres
    def set_postgres(self):
        self.dbtype = POSTGRES
        self.host = 'gauss'
        self.port = '5432'
        self.database = 'socgcs'
        self.user = 'postgres'          # 'admin'
        self.password = 'PassRoot'      # 'Passw0rd'

    # Setup for SQLite
    def set_sqlite(self):
        self.dbtype = SQLITE
        self.host = ''
        self.port = ''
        self.database = 'socgcs.sqlite'
        self.user = ''
        self.password = ''


class Database(Component):

    # Constructor
    def __init__(self, conf=DbConfig()):
        super().__init__()
        self.name = 'Database'
        self.dbcon = DbConnection(conf=conf)

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
class DbConnection(Component):

    def __init__(self, conf):
        super().__init__()
        self.name = 'DbConnetion'
        self.conf = conf
        if self.conf.dbtype != '':
            self.open()


    def __del__(self):
        self.con.close()


    def open(self):
        if self.conf.dbtype == POSTGRES:
            self.con = psycopg2.connect(database=self.conf.database, user=self.conf.user, password=self.conf.password, host=self.conf.host, port=self.conf.port)
            self.log('POSTGRES: Database connected successfully')
        elif self.conf.dbtype == SQLITE:
            self.con = sqlite3.connect(self.conf.database)
            self.log('SQLITE: Database connected successfully')


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


    #
    def insert(self, table_name, fields_dict):

        columns = fields_dict.keys()
        values = [fields_dict[column] for column in columns]

        sql_text = 'INSERT INTO ' + table_name + ' (%s) VALUES %s'

        sql = self.cur.mogrify(sql_text, (AsIs(','.join(columns)), tuple(values)))

        self.exec(sql)


# ===========================================================================
#
# ===========================================================================
# GCS Event Data

class EventData:

    # Constructor
    def __init__(self):
        self.uuid = ''
        self.start_time = 0
        self.end_time = 0
        self.ack_time = 0
        self.reset_time = 0
        self.object_name = ''
        self.event_type = ''
        self.description = ''
        self.new_event = True

        self.params = {}

        #event = EventData()
        #event....
        #fields = event.__dict__
        #query.insert('gcs_events', fields)


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
