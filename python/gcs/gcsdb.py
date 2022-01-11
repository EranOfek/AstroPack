#
# gcsdb.py - Data structures and Database connectivity
#

import psycopg2
import datetime
from gcsbase import Component

# GCS Data Types


# ===========================================================================

class Database(Component):

    # Constructor
    def __init__(self):
        self.interface_name = ''
        self.dbcon = DbConnetion()

    # Destructor
    def __del__(self):
        # Deleted
        pass

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



# ===========================================================================
#
# ===========================================================================
# Database Connection
class DbConnetion:

    def __init__(self, host='gauss'):
        self.con = psycopg2.connect(database='db1', user='postgres', password='pass', host='gauss', port='5432')
        print("Database opened successfully")


    def __del__(self):
        self.con.close()


# Database Query
class DbQuery:
    def __init__(self, con=None):
        self.con = con
        self.cur = con.cursor()

    def __del__(self):
        self.cur.close()

    def exec(self, sql_text):
        self.cur.execute(sql_text)
        self.con.commit()
        #print("Record inserted successfully:", i)

    def query(self, sql_text):
        self.cur.execute(sql_text)
        # mobile_records = self.cur.fetchall()
        #print("Record inserted successfully:", i)


# ===========================================================================
#
# ===========================================================================
# GCS Event Data

class EventBase:
    # Constructor
    def __init__(self):
        self.uuid = ''
        self.time = 0

    # Destructor
    def __del__(self):
        # Deleted
        pass


# ===========================================================================
#
# ===========================================================================
class EventData:

    # Constructor
    def __init__(self):
        self.uuid = ''
        self.time = 0
        self.params = {}

    # Destructor
    def __del__(self):
        # Deleted
        pass



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

class GcsInterface:

    # Constructor
    def __init__(self):
        self.interface_name = ''

    # Destructor
    def __del__(self):
        # Deleted
        pass


# ===========================================================================
#
# ===========================================================================

class History(Component):

    # Constructor
    def __init__(self):
        self.interface_name = ''

    # Destructor
    def __del__(self):
        # Deleted
        pass

    # Validate the specified task
    def validate_task(self, task):
        pass

# ===========================================================================
#
# ===========================================================================

class Reports(Component):

    # Constructor
    def __init__(self):
        self.interface_name = ''

    # Destructor
    def __del__(self):
        # Deleted
        pass

    # Validate the specified task
    def validate_task(self, task):
        pass


    #


# ===========================================================================
#
# ===========================================================================
