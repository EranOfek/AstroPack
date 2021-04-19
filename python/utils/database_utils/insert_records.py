#
# See: https://gist.github.com/antivanov/59e00f6129725e9b4404

# Instructions

import os, glob, time, argparse, shutil, csv, json, yaml, psycopg2, uuid
from datetime import datetime


# Log message to file
LOG_PATH = 'c:/temp/'
logfile = open(os.path.join(LOG_PATH, 'trim_trailing_spaces.log'), 'a')
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

    # execute a statement
    print('PostgreSQL database version:')
    cur.execute('SELECT version()')

    # display the PostgreSQL database server version
    db_version = cur.fetchone()
    print(db_version)
    cur.close()

    cur = conn.cursor()
    cur.execute('SELECT "ImageID" FROM raw_images')
    rec = cur.fetchone()
    print(rec)

    try:

        cur = conn.cursor()
        x = 0.1

        for i in range(0, 100):
            pk = str(uuid.uuid1())
            sql = 'INSERT INTO raw_images("ImageID", "RA_Center") VALUES(%s,%s);'

            cur.execute(sql, (pk, x, ))
            x = x + 1

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
