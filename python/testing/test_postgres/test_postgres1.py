import psycopg2
import datetime

con = psycopg2.connect(database="db1", user="postgres", password="pass", host="127.0.0.1", port="5432")

print("Database opened successfully")


cur = con.cursor()

start = str(datetime.datetime.now())
data = bytes(1000*100*1)

for i in range(100000, 100999):
    pkey = start + '_' + str(i)
    cur.execute("INSERT INTO blobs(pkey, blob1) VALUES(%s,%s)",
                    (pkey, psycopg2.Binary(data)))

    con.commit()
    print("Record inserted successfully:", i)

con.close()



# declare a new PostgreSQL connection object
conn = connect(
dbname = "python_test",
user = "objectrocket",
host = "localhost",
password = "mypass"
)

# object type: psycopg2.extensions.connection
print ("\ntype(conn):", type(conn))


