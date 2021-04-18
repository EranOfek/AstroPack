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


def get_field_type(fieldname, text):
    ftype = ''
    text = text.split(' ')[0]
    text = text.replace('Enumeration:', '').strip().lower()
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

    # Special field: UUID
    elif text == 'uuid':
        ftype = 'VARCHAR(80)'
    else:
        ftype = '???'

        # Special case: Hierarchical Triangular Mesh
        if fieldname == 'HTM_ID':
            ftype = 'INTEGER'
        else:
            log('Unknown field type: ' + text)

    return ftype


def process_csv_file(filename, file_list):
    path, fname = os.path.split(filename)
    fn, ext = os.path.splitext(fname)
    table_name = fn
    out_filename = os.path.join(path, table_name + '.sql')
    outf = open(out_filename, 'w')
    outf.write('CREATE TABLE public."{}" (\n'.format(table_name))

    primary_key = ''
    field_list = []
    index_list = []

    if len(file_list) == 0:
        file_list.append(filename)

    # Scan all input files, and prepare list of fields and indexes
    for fn in file_list:

        f =  open(fn, newline='')
        rdr = csv.DictReader(f) # reader(f) #, delimiter=',', quotechar='"')


        for row in rdr:
            try:
                field_name = row['Field Name'].strip()
                description = row['Description'].strip()
                field_type = row['Data Type'].strip()
                comments = row['Comments'].strip()

                if field_name == '':
                    continue

                if field_name.find('**') > -1:
                    field_name = field_name.replace('**', '')
                    primary_key = field_name

                if field_name.find('*') > -1:
                    field_name = field_name.replace('*', '')
                    index_list.append(field_name)

                # Check if field already seen
                if field_name in field_list:
                    log('Field already defined: ' + field_name)
                    continue


                ftype = get_field_type(field_name, field_type)
                field_def = ftype
                if primary_key == field_name:
                    field_def += ' NOT NULL'

                outf.write('"{}" {}\n'.format(field_name, field_def))

                field_list.append(field_name)
            except:
                print('ex')


    if primary_key != '':
        outf.write('\n,CONSTRAINT {} PRIMARY KEY("{}")\n'.format(table_name + '_pkey', primary_key))

    outf.write(');\n\n')


    for field_name in field_list:
        outf.write('ALTER TABLE public."{}"\n  ALTER COLUMN "{}" SET STATISTICS 0;\n\n'.format(table_name, field_name))


    for field_name in index_list:
        index_name = table_name + '_idx_' + field
        outf.write('CREATE INDEX {} ON public."{}"\n  USING btree ("{}"})\n\n'.format(index_name, table_name, field_name));


    outf.write('ALTER TABLE public."{}"\n  OWNER TO postgres;\n'.format(table_name))

    outf.flush()
    outf.close()



def process_folder(fpath, ext_list, subdirs = True):
    if subdirs:
        flist = glob.glob(os.path.join(fpath, '**/*.*'), recursive=True)
    else:
        flist = glob.glob(os.path.join(fpath, '*.*'), recursive=False)

    # Step 1:
    for fname in flist:
        fnlower = fname.lower()
        path, fname = os.path.split(fnlower)
        common_files = []

        # Prepare list of common fields
        for ext in ext_list:
            ext = ext.lower()
            if fnlower.endswith(ext):
                if fname.find('_common') > -1:
                    common_files.append(fname)

    # Step 2:
    for fname in flist:
        fnlower = fname.lower()
        path, fname = os.path.split(fnlower)
        common_files = []

        # Process file by file
        for ext in ext_list:
            ext = ext.lower()
            if fnlower.endswith(ext):
                if fname.find('_common') == -1:
                    process_csv_file(fname, common_files)



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
