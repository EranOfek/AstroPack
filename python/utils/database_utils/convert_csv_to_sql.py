#
# See: https://gist.github.com/antivanov/59e00f6129725e9b4404

# Instructions

import os, glob, time, argparse, shutil, csv
from datetime import datetime

FILE_EXT = ['.csv']


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


'''
CREATE TABLE public."table" (
  "RawImageID" INTEGER NOT NULL,
  "RA_Center" DOUBLE PRECISION,
  str1 VARCHAR(250),
  CONSTRAINT table_pkey PRIMARY KEY("RawImageID")
) ;

ALTER TABLE public."table"
  ALTER COLUMN "RawImageID" SET STATISTICS 0;

ALTER TABLE public."table"
  ALTER COLUMN "RA_Center" SET STATISTICS 0;

ALTER TABLE public."table"
  ALTER COLUMN str1 SET STATISTICS 0;

ALTER TABLE public."table"
  OWNER TO postgres;

'''


def field_type(fieldname, text):
    ftype = ''
    text = text.split(' ')[0]
    text = text.replace('Enumeration:', '').strip()
    if text == 'int8' or text == 'int16' or text == 'int32':
        ftype = 'INTEGER'
    elif text == 'int64':
        ftype = 'BIGINT'
    elif text == 'single' or text == 'double':
        ftype = 'DOUBLE PRECISION'
    elif text == 'bool':
        ftype = 'BOOLEAN'
    elif text == 'string':
        ftype = 'VARCHAR(250)'
    else:
        ftype = '???'
        if fieldname == 'HTM_ID':
            ftype = 'INTEGER'
        else:
            log('Unknown field type: ' + text)

    return ftype


def process_csv_file(filename):
    path, fname = os.path.split(filename)
    fn, ext = os.path.splitext(fname)
    tablename = fn
    out_filename = os.path.join(path, tablename + '.sql')
    outf = open(out_filename, 'w')
    outf.write('CREATE TABLE public."{}" (\n'.format(tablename))

    import csv
    f =  open(filename, newline='')
    rdr = csv.DictReader(f) # reader(f) #, delimiter=',', quotechar='"')

    pkey = ''
    field_list = []
    index_list = []
    for row in rdr:
        try:
            field = row['Field Name']
            description = row['Description']
            datatype = row['Data Type']
            comments = row['Comments']

            if field.find('*') > -1:
                field = field.replace('*', '')
                pkey = field

            ftype = field_type(field, datatype)
            fdef = ftype
            if pkey == field:
                fdef += ' NOT NULL'

            outf.write('"{}" {}\n'.format(field, fdef))

            field_list.append(field)
        except:
            print('ex')


    if pkey != '':
        outf.write('\n,CONSTRAINT {} PRIMARY KEY("{}")\n'.format(tablename + '_pkey', pkey))

    outf.write(');\n\n')


    for field in field_list:
        outf.write('ALTER TABLE public."{}"\n  ALTER COLUMN "{}" SET STATISTICS 0;\n\n'.format(tablename, field))

    outf.write('ALTER TABLE public."{}"\n  OWNER TO postgres;\n'.format(tablename, field))

    outf.flush()
    outf.close()



def process_folder(fpath, ext_list, subdirs = True):
    if subdirs:
        flist = glob.glob(os.path.join(fpath, '**/*.*'), recursive=True)
    else:
        flist = glob.glob(os.path.join(fpath, '*.*'), recursive=False)

    for fname in flist:
        fnlower = fname.lower()
        for ext in ext_list:
            ext = ext.lower()
            if fnlower.endswith(ext):
                process_csv_file(fname)


def main():

    # Read command line options
    parser = argparse.ArgumentParser()

    # Arguments
    #parser.add_argument('-d',           dest='dir',         default=None,                                   help='pcap folder')
    #parser.add_argument('-s',           dest='subdirs',     action='store_true',    default=True,   help='Process pcap files in subfolders')
    #args = parser.parse_args()

    folder = '.'
    process_folder(folder, FILE_EXT, True)


if __name__ == '__main__':
    main()
