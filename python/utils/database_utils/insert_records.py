# Insert records into database tables
#
# Author: Chen Tishler (June 2021)
#
#
# See: https://gist.github.com/antivanov/59e00f6129725e9b4404

import os, glob, time, argparse, shutil, csv, json, yaml, psycopg2, uuid, random
from datetime import datetime


# Log message to file
LOG_PATH = 'c:/temp/'
logfile = open(os.path.join(LOG_PATH, 'insert_records.log'), 'a')
def log(msg, dt = False):
    global logfile
    if msg == '': dt = False
    if dt: msg = datetime.now().strftime('%d/%m/%y %H:%M:%S.%f')[:-3] + ' ' + msg
    print(msg)
    if logfile:
        logfile.write(msg)
        logfile.write("\n")
        logfile.flush()


def insert_records():

    # Connect
    conn = psycopg2.connect(
        host="localhost",
        database="pipeline",
        user="postgres",
        password="pass")

    # create a cursor
    cur = conn.cursor()

    # Get the PostgreSQL database server version
    print('PostgreSQL database version:')
    cur.execute('SELECT version()')
    db_version = cur.fetchone()
    print(db_version)
    cur.close()

    cur = conn.cursor()
    cur.execute('SELECT * FROM master_table limit 1')
    rec = cur.fetchone()
    print(rec)

    try:

        cur = conn.cursor()
        rand_start = random.randint(1, 10000000)

        # Insert records
        for i in range(0, 10000):

            # Generate UUID
            RecID = str(uuid.uuid1())
            InsertTime = datetime.now()
            UpdateTime = InsertTime
            FInt = rand_start + i
            FBigInt = 100000000000 + FInt
            FBool = True
            FDouble = FInt / 100000000000.0
            FTimestamp = InsertTime
            FString = 'String: ' + FTimestamp.strftime('%d/%m/%y %H:%M:%S.%f')[:-3]

            # Insert record
            sql = 'INSERT INTO master_table(RecID, InsertTime, UpdateTime, FInt, FBigInt, FBool, FDouble, FTimestamp, FString) VALUES(%s,%s);'

            cur.execute(sql, (RecID, InsertTime, UpdateTime, FInt, FBigInt, FBool, FDouble, FTimestamp, FString, ))

        #cur.executemany(sql, (pk,))
        #id = cur.fetchone()[0

        conn.commit()

        # close the communication with the PostgreSQL
        cur.close()

    except (Exception, psycopg2.DatabaseError) as error:
        print(error)
    finally:
        if conn is not None:
            conn.close()


def main():

    # Read command line options
    parser = argparse.ArgumentParser()

    # Arguments
    #parser.add_argument('-d',           dest='dir',         default=None,                                   help='pcap folder')
    #parser.add_argument('-s',           dest='subdirs',     action='store_true',    default=True,   help='Process pcap files in subfolders')
    #args = parser.parse_args()


    insert_records()


if __name__ == '__main__':
    main()
