import pymysql.cursors
from configparser import ConfigParser
import logging


def connect():
    config = ConfigParser(inline_comment_prefixes=';')
    config.read('config.ini')

    conn = pymysql.connect(host=config.get('DB', 'HOST'),
                           user=config.get('DB', 'USER'),
                           passwd=config.get('DB', 'PASSWD'),
                           db=config.get('DB', 'DB'),
                           unix_socket=config.get('DB', 'SOCKET'))
    return conn


def insert_values(table, dict_to_insert, log=None):
    if log is None:
        log = logging.getLogger(__name__)

    keys = dict_to_insert.keys()
    vals = [str(dict_to_insert[key]) for key in keys]
    query = 'INSERT INTO {} (`{}`) VALUES ({});'.format(table, '`, `'.join(keys), ', '.join('{}'.format('"{}"'.format(v) if "SELECT" not in v else v) for v in vals))
    conn = connect()
    try:
        cursor = conn.cursor(pymysql.cursors.DictCursor)
        cursor.execute(query)
        conn.commit()
        cursor.close()
    except pymysql.err.InternalError as e:
        code, msg = e.args
        log.error("Failed to insert values into table {}.".format(table))
        log.error("Error code = {}".format(code))
        log.error(msg)
    finally:
        conn.close()


def get_columns(table, log=None):
    if log is None:
        log = logging.getLogger(__name__)

    query = "SELECT `COLUMN_NAME` FROM `INFORMATION_SCHEMA`.`COLUMNS` WHERE `TABLE_NAME`='{}';".format(table)
    conn = connect()
    try:
        cursor = conn.cursor(pymysql.cursors.DictCursor)
        cursor.execute(query)
        col = cursor.fetchall()
        cursor.close()
        cols = [x['COLUMN_NAME'] for x in col if 'COLUMN_NAME' in x]
    except pymysql.err.InternalError as e:
        code, msg = e.args
        log.error("Failed to retrieve columns from table {}.".format(table))
        log.error("Error code = {}".format(code))
        log.error(msg)
        cols = []
    finally:
        conn.close()

    return cols


def insert_voevent(table, params, log=None):
    # Remove keys not included in the table
    cols = get_columns(table)
    dict_to_insert = dict(params)
    for key in params.keys():
        if key not in cols:
            dict_to_insert.pop(key, None)
    # Insert values to table
    insert_values(table, dict_to_insert, log)
