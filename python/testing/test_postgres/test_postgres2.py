# https://www.postgresqltutorial.com/postgresql-python/
# https://kb.objectrocket.com/category/postgresql

# import the psycopg2 database adapter for PostgreSQL
from psycopg2 import connect, extensions, sql

# declare a new PostgreSQL connection object
conn = connect(
dbname = "python_test",
user = "objectrocket",
host = "localhost",
password = "mypass"
)

# object type: psycopg2.extensions.connection
print ("\ntype(conn):", type(conn))

# string for the new database name to be created
DB_NAME = "some_new_database"

# get the isolation leve for autocommit
autocommit = extensions.ISOLATION_LEVEL_AUTOCOMMIT
print ("ISOLATION_LEVEL_AUTOCOMMIT:", extensions.ISOLATION_LEVEL_AUTOCOMMIT)

"""
ISOLATION LEVELS for psycopg2
0 = READ UNCOMMITTED
1 = READ COMMITTED
2 = REPEATABLE READ
3 = SERIALIZABLE
4 = DEFAULT
"""

# set the isolation level for the connection's cursors
# will raise ActiveSqlTransaction exception otherwise
conn.set_isolation_level( autocommit )

# instantiate a cursor object from the connection
cursor = conn.cursor()

# use the execute() method to make a SQL request
#cursor.execute('CREATE DATABASE ' + str(DB_NAME))

# use the sql module instead to avoid SQL injection attacks
cursor.execute(sql.SQL(
"CREATE DATABASE {}"
).format(sql.Identifier( DB_NAME )))

# close the cursor to avoid memory leaks
cursor.close()

# close the connection to avoid memory leaks
conn.close()






def bulkInsert(records):
    try:
        connection = psycopg2.connect(user="sysadmin",
                                      password="pynative@#29",
                                      host="127.0.0.1",
                                      port="5432",
                                      database="postgres_db")
        cursor = connection.cursor()
        sql_insert_query = """ INSERT INTO mobile (id, model, price) 
                           VALUES (%s,%s,%s) """

        # executemany() to insert multiple rows rows
        result = cursor.executemany(sql_insert_query, records)
        connection.commit()
        print(cursor.rowcount, "Record inserted successfully into mobile table")

    except (Exception, psycopg2.Error) as error:
        print("Failed inserting record into mobile table {}".format(error))

    finally:
        # closing database connection.
        if (connection):
            cursor.close()
            connection.close()
            print("PostgreSQL connection is closed")

records_to_insert = [ (4,'LG', 800) , (5,'One Plus 6', 950)]
bulkInsert(records_to_insert)



def updateInBulk(records):
    try:
        ps_connection = psycopg2.connect(user="sysadmin",
                                         password="pynative@#29",
                                         host="127.0.0.1",
                                         port="5432",
                                         database="postgres_db")
        cursor = ps_connection.cursor()

        # Update multiple records
        sql_update_query = """Update mobile set price = %s where id = %s"""
        cursor.executemany(sql_update_query, records)
        ps_connection.commit()

        row_count = cursor.rowcount
        print(row_count, "Records Updated")

    except (Exception, psycopg2.Error) as error:
        print("Error while updating PostgreSQL table", error)

    finally:
        # closing database connection.
        if (ps_connection):
            cursor.close()
            ps_connection.close()
            print("PostgreSQL connection is closed")

tuples = [(750, 4), (950, 5)]
updateInBulk(tuples)


