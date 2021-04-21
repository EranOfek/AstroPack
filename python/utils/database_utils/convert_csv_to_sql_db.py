#
# See: https://gist.github.com/antivanov/59e00f6129725e9b4404

# Instructions

import os, glob, time, argparse, shutil, csv, json, yaml
from datetime import datetime

FILE_EXT = ['.csv']


# Log message to file
LOG_PATH = 'c:/temp/'
logfile = open(os.path.join(LOG_PATH, 'convert_csv_to_sql_db.log'), 'a')
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


def get_field_type(field_name, text):
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
    elif text == 'string' or text == 'text':
        ftype = 'VARCHAR'

    # Special field: UUID
    elif text == 'uuid':
        ftype = 'VARCHAR'
    else:
        ftype = '???'

        # Special case: Hierarchical Triangular Mesh
        if field_name.startswith('HTM_ID'):
            ftype = 'VARCHAR'
        else:
            log('Unknown field type: ' + text)

    return ftype


class Field:

    def __init__(self):
        self.field_name = ''
        self.field_type = ''
        self.data_type = ''
        self.comments = ''
        self.metadata = ''
        self.json = None
        self.yaml = None
        self.primary_key = False
        self.index = False
        self.index_method = 'btree'
        self.is_common = False


def get_csv(row, column, _default = '', _strip = True):
    if column in row:
        if row[column]:
            value = row[column]
        else:
            value = ''
        if _strip:
            value = value.strip()
    else:
        value = _default

    return value


class TableDef:

    #
    def __init__(self):
        self.field_list = []
        self.field_dict = {}
        self.db_name = ''
        self.table_name = ''
        self.sql_filename = ''
        self.outf = None


    #
    def set_db(self, filename: str) -> None:
        # Split file name to database name and table name, i.e.
        # 'image_tables - processed_cropped_images.csv'
        path, fname = os.path.split(filename)
        fn, ext = os.path.splitext(fname)
        db = fn.split('-')
        self.db_name = db[0].strip()
        self.table_name = db[1].strip()

        self.sql_filename = os.path.join(path, self.db_name + '.sql')
        self.outf = open(self.sql_filename, 'a')
        self.write('\n\n\n\n')

        #self.sql_filename = os.path.join(path, self.table_name + '.sql')
        #self.outf = open(self.sql_filename, 'w')


    def get_include_filename(self, filename: str, include: str) -> str:
        path, fname = os.path.split(filename)
        fn, ext = os.path.splitext(fname)
        db = fname.split('-')
        include_filename = os.path.join(path, db[0].strip() + ' - ' + include + '.csv')
        return include_filename


    #
    def load_csv(self, filename: str) -> None:

        log('Loading: ' + filename)

        is_common = False
        if filename.find('[') > -1:
            is_common = True

        f = open(filename, newline='')
        rdr = csv.DictReader(f)

        for row in rdr:
            try:
                field = Field()

                field.field_name = get_csv(row, 'Field Name')
                if field.field_name == '':
                    continue

                # Load include file (common fields)
                if field.field_name.find('[') > -1:
                    include_filename = self.get_include_filename(filename, field.field_name)
                    if include_filename != '' and os.path.exists(include_filename):
                        self.load_csv(include_filename)
                    continue


                field.description = get_csv(row, 'Description')
                field.field_type = get_csv(row, 'Data Type')
                field.comments = get_csv(row, 'Comments')
                field.metadata = get_csv(row, 'Metadata')
                field.is_common = is_common

                #field.json = json.loads('{' + field.metadata + '}')
                field.yaml = yaml.load('{' + field.metadata + '}')

                # Parse metadata
                if 'index_method' in field.yaml:
                    field.index_method = field.yaml['index_method']


                # Field type
                field.data_type = get_field_type(field.field_name, field.field_type)
                if field.data_type == '':
                    field.data_type = '???'

                # Primary key
                if field.field_name.find('**') > -1:
                    field.field_name = field.field_name.replace('**', '')
                    field.primary_key = True

                # Index
                if field.field_name.find('*') > -1:
                    field.field_name = field.field_name.replace('*', '')
                    field.index = True

                # Check if field already seen
                if field.field_name in self.field_dict:
                    log('Field already defined, ignored: ' + field.field_name)
                    continue


                self.field_list.append(field)
                self.field_dict[field.field_name] = field
            except:
                print('ex')

        log('Loading done: ' + filename)


    #
    def create_table(self):
        self.write('CREATE TABLE public.{} (\n'.format(self.table_name))
        #self.write('CREATE TABLE public."{}" (\n'.format(self.table_name))

        primary_key = []

        for field in self.field_list:

            # Debug only
            prefix = ''
            #if field.is_common:
            #    prefix = 'Common_'

            field_def = field.data_type

            if field.primary_key:
                primary_key.append(field.field_name)
                field_def += ' NOT NULL'

            field_def += ','

            self.write('{}{} {}\n'.format(prefix, field.field_name, field_def))
            #self.write('"{}{}" {}\n'.format(prefix, field.field_name, field_def))


        # Primary key
        if len(primary_key) > 0:
            self.write('\nCONSTRAINT {} PRIMARY KEY({})\n'.format(self.table_name + '_pkey', ', '.join(primary_key)))
            #self.write('\nCONSTRAINT {} PRIMARY KEY("{}")\n'.format(self.table_name + '_pkey', ', '.join(primary_key)))

        self.write(');\n\n')

        # SET STATISTICS 0
        for field in self.field_list:
            self.write('ALTER TABLE public.{}\n  ALTER COLUMN {} SET STATISTICS 0;\n\n'.format(self.table_name, field.field_name))
            #self.write('ALTER TABLE public."{}"\n  ALTER COLUMN "{}" SET STATISTICS 0;\n\n'.format(self.table_name, field.field_name))

        # Index
        for field in self.field_list:
            if field.index:
                index_name = self.table_name + '_idx_' + field.field_name
                self.write('CREATE INDEX {} ON public.{}\n  USING {} ({});\n\n'.format(index_name, self.table_name, field.index_method, field.field_name))
                #self.write('CREATE INDEX {} ON public."{}"\n  USING {} ("{}");\n\n'.format(index_name, self.table_name, field.index_method, field.field_name))

        # OWNER
        self.write('ALTER TABLE public.{}\n  OWNER TO postgres;\n'.format(self.table_name))
        #self.write('ALTER TABLE public."{}"\n  OWNER TO postgres;\n'.format(self.table_name))

        self.outf.close()


    def write(self, text):
        #print(text)
        self.outf.write(text)
        self.outf.flush()




def process_folder(fpath, ext_list, subdirs = True):
    if subdirs:
        flist = glob.glob(os.path.join(fpath, '**/*.*'), recursive=True)
    else:
        flist = glob.glob(os.path.join(fpath, '*.*'), recursive=False)

    # Step 1:
    for filename in flist:
        filename_lower = filename.lower()
        path, fname = os.path.split(filename_lower)

        # Prepare list of common fields
        for ext in ext_list:
            ext = ext.lower()
            if filename_lower.endswith(ext):
                if fname.find('[') == -1:

                    table = TableDef()
                    table.set_db(filename_lower)
                    table.load_csv(filename_lower)
                    table.create_table()


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
