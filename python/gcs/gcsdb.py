import psycopg2
import datetime
from gcsbase import Component

# GCS Data Types


#============================================================================

# GCS Observasion Plan Validator

# Run IAI validator?

class Database(Component):

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
#
class DbConnetion:

    def __init__(self):
        self.con = psycopg2.connect(database="db1", user="postgres", password="pass", host="gauss", port="5432")
        print("Database opened successfully")


    def __del__(self):
        self.con.close()

#
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

    # Destructor
    def __del__(self):
        # Deleted
        pass




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


