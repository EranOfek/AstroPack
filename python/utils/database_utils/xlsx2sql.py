#
# Generate SQL scripts from CSV files downloaded from Google Sheets tabs
#
#
# See: https://gist.github.com/antivanov/59e00f6129725e9b4404

# Instructions
#
#
# Unit-Test:
#
# Use unittest__tables from GDrive to test
#
# Requirements:
#
#       sudo apt install python3-pip
#       pip3 install pyyaml openpyxl psycopg2
#

import os, glob, time, argparse, shutil, csv, json, yaml, openpyxl
from datetime import datetime

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
    if text == 'int' or text == 'int8' or text == 'int16' or text == 'int32':
        ftype = 'INTEGER'
    elif text == 'int64' or text == 'bigint':
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

        # Special case: Hierarchical Triangular Mesh
        if field_name.startswith('HTM_ID'):
            ftype = 'VARCHAR'

    if ftype == '':
        #ftype = '???'
        ftype = 'DOUBLE PRECISION'
        log('Unknown field type, using default DOUBLE: ' + text)

    return ftype


class Field:

    def __init__(self):
        self.field_name = ''
        self.field_type = ''
        self.data_type = ''
        self.comments = ''
        self.metadata = ''
        self.source = ''
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


class DatabaseDef:

    #
    def __init__(self):
        self.field_list = []
        self.field_dict = {}
        self.def_path = ''
        self.db_name = ''
        self.table_name = ''
        self.source_filename = ''
        self.set_statistics = False
        self.set_owner = False
        self.sql_filename = ''
        self.outf = None


    #
    def set_db(self, filename: str) -> None:

        # Split file name to database name and table name, i.e.
        # 'image_tables - processed_cropped_images.csv'
        log('set_db: ' + filename)

        self.source_filename = filename
        path, fname = os.path.split(filename)
        fn, ext = os.path.splitext(fname)
        self.def_path = path

        # Get database name from last part of folder name
        self.db_name = os.path.split(path)[1]
        log('database name from filename: ' + self.db_name)

        # Get table name from tab name
        tab_name = fn.split('-')
        prefix = tab_name[0].strip()
        if len(tab_name) > 1:
            self.table_name = tab_name[1].strip()
        else:
            self.table_name = ''

        log('table name: ' + self.table_name)

        # Prepare output SQL file name
        self.sql_filename = os.path.join(path, '__' + self.db_name + '.sql')
        log('sql output file: ' + self.sql_filename)

        # Field does not exist yet, need to add 'create database' commands
        need_create = False
        if not os.path.exists(self.sql_filename):
            log('sql output file does not exist, creating new database: ' + self.db_name)
            need_create = True

        # Open file for append
        self.outf = open(self.sql_filename, 'a')
        self.write('\n')

        # @Todo: call it later after processing metadata such as $Database
        if need_create:
            self.create_db()


    def create_db(self):

        log('create_db started : ' + self.db_name)

        # Check if we have specific file for our database
        fname = os.path.join(self.def_path, 'create_database.sql')

        # Not found, use general file
        if not os.path.exists(fname):

            script_path = os.path.dirname(os.path.realpath(__file__))
            fname = os.path.join(script_path, 'create_database.sql')

            if not os.path.exists(fname):
                log('create_db: database definition file not found: ' + fname)
                return

        log('using database file: ' + fname)

        self.write('--\n')
        self.write('-- Automatic Generated File by convert_csv_to_sq_db.py\n')
        self.write('-- Source file: ' + fname + '\n')
        self.write('--\n')
        self.write('\n\n\n')

        with open(fname) as f:
            lines = f.read().splitlines()

        for line in lines:
            line = line.rstrip()

            # Replace database name macro
            if line.find('$DatabaseName$') > -1:
                line = line.replace('$DatabaseName$', self.db_name)
                log('replaced $DatabaseName$: ' + line)
                # @Todo: need to call create_db() later with the metadata

            self.write(line)
            self.write('\n')

        log('create_db done: ' + self.db_name)
        log('')


    #
    def get_include_filename(self, filename: str, include: str) -> str:
        path, fname = os.path.split(filename)
        fn, ext = os.path.splitext(fname)
        db = fname.split('-')
        include_filename = os.path.join(path, db[0].strip() + ' - ' + include + '.csv')
        return include_filename


    # Load table definition from specified csv file
    def load_table_csv(self, filename: str) -> None:

        log('load_table_csv started: ' + filename)
        self.source_filename = filename

        # [Tab Name] in brackets indicates common fields (not a table)
        is_common = False
        if filename.find('(') > -1:
            is_common = True

        f = open(filename, newline='')
        rdr = csv.DictReader(f)
        field_count = 0

        for row in rdr:
            try:
                field = Field()
                field.field_name = get_csv(row, 'Field Name')

                # Skip empty rows
                if field.field_name == '':
                    continue

                # Skip comment rows
                if field.field_name.startswith('#') or field.field_name.startswith('%'):
                    continue

                # Parse meta data
                if field.field_name.startswith('$Database'):
                    tokens = field.field_name.split(' ')
                    if len(tokens) > 1:
                        dbname = tokens[1].strip()
                        log('found $Database: ' + dbname)
                        self.db_name = dbname

                # Load include file (common fields)
                # Note: Recursive call
                if field.field_name.find('(') > -1:
                    include_filename = self.get_include_filename(filename, field.field_name)
                    if include_filename != '' and os.path.exists(include_filename):
                        log('loading include file: ' + include_filename)
                        self.load_table_csv(include_filename)
                    continue


                field.description = get_csv(row, 'Description')
                field.field_type = get_csv(row, 'Data Type')
                field.comments = get_csv(row, 'Comments')
                field.metadata = get_csv(row, 'Metadata')
                field.source = get_csv(row, 'Source')
                field.is_common = is_common

                # Load field metadata as YAML data
                field.yaml = yaml.load('{' + field.metadata + '}', Loader=yaml.FullLoader)

                # Parse metadata
                if 'index_method' in field.yaml:
                    field.index_method = field.yaml['index_method']

                # Field type
                field.data_type = get_field_type(field.field_name, field.field_type)
                if field.data_type == '':
                    field.data_type = '???'
                    log('WARNING: unknown field type, field ignored: ' + field.field_name)
                    continue

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
                    log('field already defined, ignored: ' + field.field_name)
                    continue


                self.field_list.append(field)
                self.field_dict[field.field_name] = field
                field_count += 1

            except:
                print('ex')

        log('load_table_csv done: ' + filename + ' - fields loaded: ' + str(field_count))
        log('')


    # Create table from self.field_list
    def create_table(self):

        if len(self.field_list) == 0:
            return

        log('create_table started: ' + self.table_name + ' - fields: ' + str(len(self.field_list)))

        self.write('--\n')
        self.write('-- Automatic Generated Table Definition\n')
        self.write('-- Source file: ' + self.source_filename + '\n')
        self.write('--\n')
        self.write('\n')

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
            log('primary key: ' + str(primary_key))
            self.write('\nCONSTRAINT {} PRIMARY KEY({})\n'.format(self.table_name + '_pkey', ', '.join(primary_key)))
            #self.write('\nCONSTRAINT {} PRIMARY KEY("{}")\n'.format(self.table_name + '_pkey', ', '.join(primary_key)))

        self.write(');\n\n')

        # SET STATISTICS 0
        if self.set_statistics:
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
        if self.set_owner:
            self.write('ALTER TABLE public.{}\n  OWNER TO postgres;\n'.format(self.table_name))
            #self.write('ALTER TABLE public."{}"\n  OWNER TO postgres;\n'.format(self.table_name))

        self.outf.close()
        log('create_table done: ' + self.table_name)
        log('')


    def write(self, text):
        #print(text)
        self.outf.write(text)
        self.outf.flush()

#============================================================================

# Extract all sheets from xlsx file to output folder
def extract_xlsx(filename):
    log('extract_csv started: ' + filename)
    path, fname = os.path.split(filename)
    fn, ext = os.path.splitext(fname)

    db = fn.split('__')[0]
    out_path = os.path.join(path, db)
    log('output folder: ' + out_path)
    if not os.path.exists(out_path):
        log('creating folder: ' + out_path)
        os.makedirs(out_path)

    # Open XLSX file
    wb = openpyxl.load_workbook(filename)
    log('sheet count: ' + str(len(wb.sheetnames)))
    log('sheets: ' + str(wb.sheetnames))

    # Scan sheets, save each sheet as .csv file
    csv_count = 0
    for i, sheet_name in enumerate(wb.sheetnames):
        sheet = wb.worksheets[i]
        csv_fname = os.path.join(out_path, db + ' - ' + sheet_name + '.csv')
        log('write csv file: ' + csv_fname)
        with open(csv_fname, 'w', newline="") as f:
            c = csv.writer(f)
            for r in sheet.rows:
                c.writerow([cell.value for cell in r])

        csv_count += 1

    log('extract_csv done: ' + filename)
    log('csv files created: ' + str(csv_count))

    return out_path


# Process XLSX file with database definitions
def process_xlsx_file(filename):
    log('process_xlsx_file: ' + filename)
    filename_lower = filename.lower()
    out_path = extract_xlsx(filename_lower)
    process_folder(out_path, ['.csv'], False)
    log('process_xlsx_file done: ' + filename)


# Process CSV file with database definitions
def process_csv_file(filename):
    log('processing csv: ' + filename)
    filename_lower = filename.lower()
    path, fname = os.path.split(filename_lower)

    # Skip [common fields csv files]
    if fname.find('(') > -1:
        log('ignoring csv file: ' + filename)

    else:
        # Set database from file name
        log('')
        db = DatabaseDef()
        db.set_db(filename_lower)
        db.load_table_csv(filename_lower)
        if len(db.field_list) > 0:
            db.create_table()


# Process folder with CSV database definition files
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

                if ext == '.xlsx':
                    process_xlsx_file(filename)

                elif ext == '.csv':
                    process_csv_file(filename)


#============================================================================

def main():

    # Read command line options
    parser = argparse.ArgumentParser()

    # Arguments
    parser.add_argument('-f', dest='xlsx',      default=None,           help='input xlsx file')
    parser.add_argument('-d', dest='dir',       default=None,           help='input folder, all .xlsx files will be processed')
    parser.add_argument('-s', dest='subdirs',   action='store_true',    default=False,   help='Process xlsx files in subfolders')
    args = parser.parse_args()

    if args.dir:
        process_folder(args.dir, ['.xlsx'], args.subdirs)

    elif args.xlsx:
        process_xlsx_file(args.xlsx)


if __name__ == '__main__':
    main()
