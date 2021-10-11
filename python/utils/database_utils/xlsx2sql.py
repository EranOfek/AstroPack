# Generate SQL scripts from CSV files downloaded from Google Sheets tabs
# See: https://gist.github.com/antivanov/59e00f6129725e9b4404
# Use unittest__tables from GDrive to test
#
# Requirements:
#
#   Ubutnu:
# 	sudo apt-get install libpq-dev
# 	sudo pip3 install Psycopg2
#
#   pip3 install pyyaml openpyxl psycopg2
#
# psql -V
#
# Create databse from SQL file:
# psql -U postgres -f __unittest.sql
#
# Chen Windows: postgres/pass
# Linux default:
#
# Use lazarus/pipeline/pipeline_utils GUI to run this script.
#
# xlsx2sql.py -f D:\Ultrasat\AstroPack.git\database\xlsx\lastdb__tables.xlsx
#

import os, glob, time, argparse, shutil, csv, json, yaml, openpyxl
from datetime import datetime
from sys import platform

DEBUG = True

# SQL
GEN_POSTGRES = True         # PostgreSQL v13
GEN_FIREBIRD = False        # FirebirdSQL v2.5,3
GEN_SQLITE = False          # SQLite v3

# Languages
GEN_PYTHON = False          # Python v3
GEN_MATLAB = False          # MATLAB 2020b
GEN_CPP = False             # C++ 0x03
GEN_DELPHI = False          # Delphi/Lazarus
GEN_DART = False            # Flutter

#
GEN_DESTRUCTOR = False
OUTPUT_PATH = ''

if DEBUG:
    GEN_POSTGRES, GEN_FIREBIRD, GEN_SQLITE, GEN_PYTHON, GEN_MATLAB, GEN_CPP, GEN_DELPHI, GEN_DART = True, True, True, True, True, True, True, True

# Log message to file
if platform == "win32":
    LOG_PATH = 'c:/temp/'
else:
    LOG_PATH = '/tmp/'


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


XLSX_FILENAME = ''

# Field types
field_lang_dict = { \
    'int': {
        'postgres': 'INTEGER',
        'firebird': 'INTEGER',
        'sqlite': 'INTEGER',
        'python': 'int',
        'cpp': 'int',
        'delphi': 'Integer',
        'matlab': 'int32',
        'dart': 'int',
    },

    'uint': {
        'postgres': 'INTEGER',
        'firebird': 'INTEGER',
        'sqlite': 'INTEGER',
        'python': 'int',
        'cpp': 'int',
        'delphi': 'Integer',
        'matlab': 'uint32',
        'dart': 'int',
    },

    'bigint': {
        'postgres': 'BIGINT',
        'firebird': 'BIGINT',
        'sqlite': 'BIGINT',
        'python': 'int',
        'cpp': 'int64',
        'delphi': 'LongInt',
        'matlab': 'int64',
        'dart': 'int',
    },

    'single': {
        'postgres': 'DOUBLE PRECISION',
        'firebird': 'FLOAT',
        'sqlite': 'REAL',
        'python': 'float',
        'cpp': 'float',
        'delphi': 'Single',
        'matlab': 'single',
        'dart': 'double',
    },

    'double': {
        'postgres': 'DOUBLE PRECISION',
        'firebird': 'DOUBLE PRECISION',
        'sqlite': 'REAL',
        'python': 'float',
        'cpp': 'double',
        'delphi': 'Double',
        'matlab': 'double',
        'dart': 'double',
    },

    'bool': {
        'postgres': 'BOOLEAN',
        'firebird': 'BOOLEAN',
        'sqlite': 'INTEGER',
        'python': 'bool',
        'cpp': 'bool',
        'delphi': 'Boolean',
        'matlab': 'bool',
        'dart': 'double',
    },

    'string': {
        'postgres': 'VARCHAR',
        'firebird': 'VARCHAR(256)',
        'sqlite': 'TEXT',
        'python': 'string',
        'cpp': 'string',
        'delphi': 'String',
        'matlab': 'string',
        'dart': 'String',
    },

    'uuid': {
        'postgres': 'VARCHAR',
        'firebird': 'VARCHAR',
        'sqlite': 'TEXT',
        'python': 'string',
        'cpp': 'string',
        'delphi': 'String',
        'matlab': 'string',
        'dart': 'String',
    },

    'timestamp': {
        'postgres': 'TIMESTAMP',
        'firebird': 'TIMESTAMP',
        'sqlite': 'REAL',
        'python': 'float',
        'cpp': 'double',
        'delphi': 'Double',
        'matlab': 'double',
        'dart': 'double',
    },

    # @Todo
    'blob': {
        'postgres': 'BLOB',
        'firebird': 'BLOB SEGMENT SIZE 1',
        'sqlite': 'BLOB',
        'python': '?',
        'cpp': '?',
        'delphi': '?',
        'matlab': '?',
        'dart': '?',
    },

    '#comment': {
        'postgres': '--',
        'firebird': '--',
        'sqlite': '--',
        'python': '#',
        'cpp': '//',
        'delphi': '//',
        'matlab': '%',
        'dart': '//',
    },
    }


# Get field type (language specific)
def get_field_type_lang(field_name, text, lang):

    # Split text to tokens
    text = text.split(' ')[0]

    # Remove 'Enumeration:'
    text = text.replace('Enumeration:', '').strip()

    # Remove whitespace and convert to lower case
    text = text.strip().lower()

    ftype = ''
    default_value = '0'

    # Convert text to general data type
    # Default field type is double - @Todo: Do we want deafult or throw error?
    if text == '':
        text = 'double'
    elif text == 'bool' or text == 'logical':
        text = 'bool'
    elif text == 'int' or text == 'int8' or text == 'int16' or text == 'int32':
        text = 'int'
    elif text == 'uint' or text == 'uint8' or text == 'uint16' or text == 'uint32':
        text = 'uint'
    elif text == 'bigint' or text == 'int64' or text == 'uint64':
        text = 'bigint'
    elif text == 'string' or text == 'text' or text == 'uuid' or text == 'varchar' or text == 'charvar':
        text = 'string'
    elif text == 'timestamp' or text == 'date' or text == 'time':
        text = 'timestamp'
    elif text == 'htm_id':
        text = 'string'

    # Type not specified, try from field name
    else:

        # UUID
        if field_name.lower().endswith('uuid'):
            text = 'string'

        # HTM_ID (Hierarchical Triangular Mesh)
        elif field_name.lower().startswith('htm_id'):
            text = 'string'


    # Look for language specific field type in dictionary
    if text in field_lang_dict:
        ld = field_lang_dict[text]
        if lang in ld:
            ftype = ld[lang]
        else:
            log('ERROR! get_field_type_lang: lang not found in field_lang_dict: ' + lang)
    else:
        log('get_field_type_lang: text not found in field_lang_dict: ' + text)

    # Set default value for string
    if ftype.lower() == 'string':
        if lang == 'cpp':
            default_value = '""'
        else:
            default_value = "''"

    return ftype, default_value

#----------------------------------------------------------------------------
# Field class
class Field:

    def __init__(self):
        self.field_name = ''
        self.field_type = ''
        self.comments = ''
        self.metadata = ''
        self.source = ''
        self.json = None
        self.yaml = None
        self.primary_key = False
        self.index = False
        self.index_method = 'btree'
        self.is_common = False
        self.display_name = ''
        self.display_format = ''


# Get value from specified column in csv line
# row is from csv.DictReader()
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


# Write SQL table definition or class/struct
class DatabaseDef:

    #
    def __init__(self):
        self.field_list = []
        self.field_dict = {}
        self.def_path = ''
        self.db_name = ''
        self.table_name = ''
        self.class_name = ''
        self.origin_filename = ''
        self.source_filename = ''
        self.set_statistics = False
        self.set_owner = False
        self.base_filename = ''
        self.out_filename = ''
        self.outf = None


    # Set database name
    def set_db(self, filename, db_name):

        # Split file name to database name and table name, i.e.
        # 'image_tables - processed_cropped_images.csv'
        log('set_db: ' + filename)

        self.source_filename = filename
        path, fname = os.path.split(filename)
        fn, ext = os.path.splitext(fname)
        self.def_path = path

        # Get database name from last part of folder name
        if db_name != '':
            self.db_name = db_name
        else:
            self.db_name = os.path.split(path)[1]

        log('database name from filename: ' + self.db_name)

        # Get table name from tab name
        tab_name = fn.split('-')
        prefix = tab_name[0].strip()
        if len(tab_name) > 1:
            self.table_name = tab_name[1].strip()
            self.class_name = tab_name[1].strip()
        else:
            self.table_name = ''
            self.class_name = ''

        log('table name: ' + self.table_name)

        # Prepare output SQL file name
        if OUTPUT_PATH != '':
            self.base_filename = os.path.join(OUTPUT_PATH, self.db_name)
        else:
            self.base_filename = os.path.join(path, self.db_name)

        log('output file base name: ' + self.base_filename)


    # Generate SQL to create database
    def create_db(self, lang):

        log('create_db started : ' + self.db_name)

        # Check if we have specific file for our database
        fname = os.path.join(self.def_path, 'create_database_' + lang + '.sql')

        # Not found, use general file from source code folder
        if not os.path.exists(fname):

            script_path = os.path.dirname(os.path.realpath(__file__))
            fname = os.path.join(script_path, 'create_database_' + lang + '.sql')

            if not os.path.exists(fname):
                log('create_db: database definition file not found: ' + fname)
                return

        log('using database file: ' + fname)
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

        self.write('\n\n')

        log('create_db done: ' + self.db_name)
        log('')


    # Generate file name of include files (sheets with name in paranthesis)
    def get_include_filename(self, filename, include):
        path, fname = os.path.split(filename)
        fn, ext = os.path.splitext(fname)
        db = fname.split('-')
        include_filename = os.path.join(path, db[0].strip() + ' - ' + include + '.csv')
        return include_filename


    # Load table definition from specified csv file
    def load_table_csv(self, filename):

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

                if field.field_name.find('History of proc_steps') > -1:
                    log('debug')

                # Skip comment rows
                if field.field_name.startswith('#') or field.field_name.startswith('%') or \
                        field.field_name.startswith(';') or field.field_name.startswith('//'):
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
                        save_source = self.source_filename
                        self.load_table_csv(include_filename)
                        self.source_filename = save_source
                    continue

                # Set field properties
                field.description = get_csv(row, 'Description')
                field.field_type = get_csv(row, 'Data Type')
                field.comments = get_csv(row, 'Comments')
                field.metadata = get_csv(row, 'Metadata')
                field.source = get_csv(row, 'Source')
                field.is_common = is_common
                field.display_name = get_csv(row, 'Display Name')
                field.display_format = get_csv(row, 'Display Format')

                # Load field metadata as YAML data
                field.yaml = yaml.load('{' + field.metadata + '}', Loader=yaml.FullLoader)

                # Parse metadata
                if 'index_method' in field.yaml:
                    field.index_method = field.yaml['index_method']

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

    #---------------------------------------------------------------- Postgres SQL
    # Create table from self.field_list
    def create_table_postgres(self):

        log('create_table_postgres started: ' + self.table_name + ' - fields: ' + str(len(self.field_list)))

        if len(self.field_list) == 0:
            return

        self.open_out('.sql')
        if self.write_file_header('postgres'):
            self.create_db('postgres')

        self.write_source_header('postgres')
        self.write('CREATE TABLE public.{} (\n'.format(self.table_name))
        primary_key = []

        for i, field in enumerate(self.field_list):

            # Debug only - mark common fields
            prefix = ''
            #if field.is_common:
            #    prefix = 'Common_'

            field_def, field_value = get_field_type_lang(field.field_name, field.field_type, 'postgres')

            if field.primary_key:
                primary_key.append(field.field_name)
                field_def += ' NOT NULL'

            if i < len(self.field_list)-1  or len(primary_key) > 0:
                field_def += ','

            self.write('{}{} {}\n'.format(prefix, field.field_name, field_def))

        # Primary key
        if len(primary_key) > 0:
            log('primary key: ' + str(primary_key))
            self.write('\nCONSTRAINT {} PRIMARY KEY({})\n'.format(self.table_name + '_pkey', ', '.join(primary_key)))

        self.write(');\n\n')

        # SET STATISTICS 0
        if self.set_statistics:
            for field in self.field_list:
                self.write('ALTER TABLE public.{}\n  ALTER COLUMN {} SET STATISTICS 0;\n\n'.format(self.table_name, field.field_name))

        # Index
        for field in self.field_list:
            if field.index:
                index_name = self.table_name + '_idx_' + field.field_name
                self.write('CREATE INDEX {} ON public.{}\n  USING {} ({});\n\n'.format(index_name, self.table_name, field.index_method, field.field_name))

        # OWNER
        if self.set_owner:
            self.write('ALTER TABLE public.{}\n  OWNER TO postgres;\n'.format(self.table_name))

        self.outf.close()
        log('create_table_postgres done: ' + self.table_name)
        log('')

    #---------------------------------------------------------------- Firebird SQL
    # Create table from self.field_list
    def create_table_firebird(self):

        log('create_table_firebird started: ' + self.table_name + ' - fields: ' + str(len(self.field_list)))

        if len(self.field_list) == 0:
            return

        self.open_out('_firebird.sql')
        if self.write_file_header('firebird'):
            self.create_db('firebird')

        self.write_source_header('firebird')
        self.write('CREATE TABLE {} (\n'.format(self.table_name))
        primary_key = []

        for i, field in enumerate(self.field_list):

            # Debug only
            prefix = ''
            #if field.is_common:
            #    prefix = 'Common_'

            field_def, field_value = get_field_type_lang(field.field_name, field.field_type, 'firebird')

            if field.primary_key:
                primary_key.append(field.field_name)
                field_def += ' NOT NULL'

            if i < len(self.field_list)-1:
                field_def += ','

            self.write('{}{} {}\n'.format(prefix, field.field_name, field_def))

        self.write(');\n\n')

        # Primary key
        if len(primary_key) > 0:
            log('primary key: ' + str(primary_key))
            self.write('\nALTER TABLE {} ADD PRIMARY KEY({});\n'.format(self.table_name + '_pkey', ', '.join(primary_key)))


        # Index
        for field in self.field_list:
            if field.index:
                index_name = self.table_name + '_idx_' + field.field_name
                self.write('CREATE INDEX {} ON {}({});\n'.format(index_name, self.table_name, field.field_name))

        self.write('\n')
        self.outf.close()

        log('create_table_firebird done: ' + self.table_name)
        log('')



    #---------------------------------------------------------------- SQLite SQL
    # Create table from self.field_list
    def create_table_sqlite(self):

        log('create_table_sqlite started: ' + self.table_name + ' - fields: ' + str(len(self.field_list)))

        if len(self.field_list) == 0:
            return

        self.open_out('_sqlite.sql')
        if self.write_file_header('sqlite'):
            self.create_db('sqlite')

        self.write_source_header('sqlite')
        self.write('CREATE TABLE {} (\n'.format(self.table_name))
        primary_key = []

        for i, field in enumerate(self.field_list):

            # Debug only
            prefix = ''
            #if field.is_common:
            #    prefix = 'Common_'

            field_def, field_value = get_field_type_lang(field.field_name, field.field_type, 'sqlite')

            if field.primary_key:
                primary_key.append(field.field_name)
                field_def += ' NOT NULL'

            if i < len(self.field_list)-1:
                field_def += ','

            self.write('{}{} {}\n'.format(prefix, field.field_name, field_def))

        self.write(');\n\n')

        # Primary key
        if len(primary_key) > 0:
            log('primary key: ' + str(primary_key))
            self.write('\nALTER TABLE {} ADD PRIMARY KEY({});\n'.format(self.table_name + '_pkey', ', '.join(primary_key)))


        # Index
        for field in self.field_list:
            if field.index:
                index_name = self.table_name + '_idx_' + field.field_name
                self.write('CREATE INDEX {} ON {}({});\n'.format(index_name, self.table_name, field.field_name))

        self.write('\n')
        self.outf.close()

        log('create_table_sqlite done: ' + self.table_name)
        log('')


    #---------------------------------------------------------------- Python
    # Create python class from self.field_list
    '''
    class MyData:

    # Constructor
    def __init__(self):
        self.myInt = 0


    # Destructor
    def __del__(self):
        # Deleted
        pass
        
    '''
    def create_class_python(self):
        log('create_class_python started: ' + self.class_name + ' - fields: ' + str(len(self.field_list)))

        self.open_out('.py')
        self.write_file_header('python')

        self.write_source_header('python')
        self.wrln('class {}:\n'.format(self.class_name))
        #self.wrln('    # Constructor')
        self.wrln('    def __init__(self):')


        for field in self.field_list:
            ftype, field_value = get_field_type_lang(field.field_name, field.field_type, 'python')
            field_name = field.field_name
            comment = '# ' + ftype
            self.wrln('        self.{:20} = {:8} {}'.format(field_name, field_value, comment))


        self.wrln('\n')
        if GEN_DESTRUCTOR:
            #self.wrln('    # Destructor')
            self.wrln('    def __del__(self):')
            self.wrln('        pass\n\n')

        log('create_class_python done: ' + self.table_name)
        log('')

    #---------------------------------------------------------------- Malab
    # Create Matlab class from self.field_list
    '''   
    class MyData < handle
    
        properties (SetAccess = public)
            myInt int
        end
    
        
        methods  
            % Constructor    
            function Obj = MyData()
                Obj.myInt = 0;
            end
        end
        
    end    
    
    '''

    def create_class_matlab(self):
        log('create_class_matlab started: ' + self.class_name + ' - fields: ' + str(len(self.field_list)))

        self.open_out('.m')
        self.write_file_header('matlab')

        self.write_source_header('matlab')
        self.wrln('class {} < handle'.format(self.class_name))
        self.wrln('    properties (SetAccess = public)')

        for field in self.field_list:

            ftype, field_value = get_field_type_lang(field.field_name, field.field_type, 'matlab')
            field_name = field.field_name
            comment = '% ' + ftype
            self.wrln('        {:20} = {:8} {}'.format(field_name, field_value, comment))

        self.wrln('    end\n')

        self.wrln('    methods ')
        self.wrln('        function Obj = {}()'.format(self.class_name))
        self.wrln('            % Constructor')
        self.wrln('        end')

        if GEN_DESTRUCTOR:
            self.wrln('        function delete(Obj)')
            self.wrln('            % Destructor')
            self.wrln('        end')

        self.wrln('    end')
        self.wrln('end\n')

        log('create_class_matlab done: ' + self.class_name)
        log('')

    #---------------------------------------------------------------- C++
    # Create C++ class from self.field_list
    '''
    class MyData {
    public:
    
        int myInt;
        
        
        // Constructor
        MyData();
        
        // Destructor
        ~MyData();
    
    };
    
    
    inline MyData::MyData()
    {
        myInt = 0;
    }
    
        
    inline MyData::~MyData 
    {
    }
    '''
    def create_class_cpp(self):
        log('create_class_cpp started: ' + self.class_name + ' - fields: ' + str(len(self.field_list)))

        self.open_out('.h')
        self.write_file_header('cpp')

        self.write_source_header('cpp')
        self.wrln('class {} {{'.format(self.class_name))
        self.wrln('public:')

        for field in self.field_list:

            ftype, field_value = get_field_type_lang(field.field_name, field.field_type, 'cpp')
            field_name = field.field_name
            comment = '//'
            self.wrln('    {:9} {:30} {}'.format(ftype, field_name + ';', comment))


        #self.wrln('    // Constructor')
        self.wrln('')
        self.wrln('    {}();'.format(self.class_name))

        if GEN_DESTRUCTOR:
            #self.wrln('    // Destructor')
            self.wrln('    ~{}();'.format(self.class_name))
            self.wrln('};\n\n')

        self.wrln('\n};\n\n')

        self.wrln('inline {}::{}()'.format(self.class_name, self.class_name))
        self.wrln('{')

        for field in self.field_list:
            ftype, field_value = get_field_type_lang(field.field_name, field.field_type, 'cpp')
            field_name = field.field_name
            self.wrln('    {:20} = {};'.format(field_name, field_value))

        self.wrln('}\n')

        if GEN_DESTRUCTOR:
            self.wrln('inline {}::~{}()'.format(self.class_name, self.class_name))
            self.wrln('{')
            self.wrln('}\n')

        log('create_class_cpp done: ' + self.class_name)
        log('')


    #---------------------------------------------------------------- Delphi / Lazarus
    # Create Delphi class from self.field_list
    '''
    interface

    type 
      MyClass = class
      public
        // Constructor
        constructor Create();
    
        // Destructor   
        destructor Destroy();
          
        // Data
        myInt: Integer;
         
    end;
         
    implementation
    
      
    constructor MyClass.Create();
    begin
      // Initialize data
      
    end; 
    
    
    destructor MyClass.Destroy();
    begin
    end;
  
    '''
    def create_class_delphi(self):
        log('create_class_delphi started: ' + self.class_name + ' - fields: ' + str(len(self.field_list)))

        self.open_out('_ifc.pas')
        self.write_source_header('delphi')
        self.wrln('type')
        self.wrln('  {} = class'.format(self.class_name))
        self.wrln('  public')
        #self.wrln('  // Constructor')
        self.wrln('    constructor Create();')

        if GEN_DESTRUCTOR:
            #self.wrln('  // Destructor')
            self.wrln('    destructor Destroy();\n')

        self.wrln('    // Data')

        for field in self.field_list:

            ftype, field_value = get_field_type_lang(field.field_name, field.field_type, 'delphi')
            field_name = field.field_name
            comment = '//'
            self.wrln('    {:20}: {:30} {}'.format(field_name, ftype + ';', comment))

        # myInt: Integer;

        self.wrln('  end;\n')

        self.open_out('_imp.pas')
        self.wrln('constructor {}.Create();'.format(self.class_name))
        self.wrln('begin')
        self.wrln('  // Initialize data')

        for field in self.field_list:
            ftype, field_value = get_field_type_lang(field.field_name, field.field_type, 'delphi')
            field_name = field.field_name
            self.wrln('  {:20} := {};'.format(field_name, field_value))

        self.wrln('end;\n');

        if GEN_DESTRUCTOR:
            self.wrln('destructor {}.Destroy();'.format(self.class_name))
            self.wrln('begin');
            self.wrln('end;\n');

        # Merge interface and implementation files to single file
        self.merge_delphi()

        log('create_class_delphi done: ' + self.class_name)
        log('')


    # Merge interface and implementation files to single file
    def merge_delphi(self):

        self.open_out('.pas')
        if self.write_file_header('delphi'):
            self.wrln('interface\n')
            self.wrln('uses')
            self.wrln('  Classes;\n\n')
            self.wrln('implementation\n')
            self.wrln('\nend.')

        # Read current output file
        file_lines = self.read_out('.pas')
        self.open_out('.pas', True)

        # Output all existing lines until 'implementation'
        imp_line = 0
        for i, line in enumerate(file_lines):
            if line == 'implementation':
                imp_line = i
                break
            self.wrln(line, False)

        # Load _ifc file and write at the end of interface section
        ifc_filename = self.base_filename + '_ifc.pas'
        with open(ifc_filename) as f:
            lines = f.read().splitlines()
        for line in lines:
            self.wrln(line, False)

        self.wrln('\nimplementation\n')

        # Output all existing implementation lines until 'end.'
        for i in range(imp_line+1, len(file_lines)):
            line = file_lines[i]
            if line == 'end.':
                break
            self.wrln(line, False)

        # Load _imp fieland write at end of file
        imp_filename = self.base_filename + '_imp.pas'
        with open(imp_filename) as f:
            lines = f.read().splitlines()
        for line in lines:
            self.wrln(line, False)

        self.wrln('\nend.\n')

        os.remove(ifc_filename)
        os.remove(imp_filename)

    # ---------------------------------------------------------------- Dart (Flutter)
    # Create C++ class from self.field_list
    '''
    class Spacecraft {
        String name;
        DateTime? launchDate;
        
        
        // Constructor, with syntactic sugar for assignment to members.
        Spacecraft(this.name, this.launchDate) {
        // Initialization code goes here.
        }
        
        // Named constructor that forwards to the default one.
        Spacecraft.unlaunched(String name) : this(name, null);
        
        // Method.
        void describe() {
        print('Spacecraft: $name');
        var launchDate = this.launchDate; // Type promotion doesn't work on getters.
        if (launchDate != null) {
          int years = DateTime.now().difference(launchDate).inDays ~/ 365;
          print('Launched: $launchYear ($years years ago)');
        } else {
          print('Unlaunched');
        }
    }
    }
    '''

    def create_class_dart(self):
        log('create_class_dart started: ' + self.class_name + ' - fields: ' + str(len(self.field_list)))

        self.open_out('.dart')
        self.write_file_header('dart')

        self.write_source_header('dart')
        self.wrln('class {} {{'.format(self.class_name))

        for field in self.field_list:
            ftype, field_value = get_field_type_lang(field.field_name, field.field_type, 'dart')
            field_name = field.field_name
            comment = '//'
            self.wrln('    {:9} {:30} {}'.format(ftype, field_name + ';', comment))

        # self.wrln('    // Constructor')
        self.wrln('')
        self.wrln('    {}() {{'.format(self.class_name))

        for field in self.field_list:
            ftype, field_value = get_field_type_lang(field.field_name, field.field_type, 'dart')
            field_name = field.field_name
            self.wrln('        {:20} = {};'.format(field_name, field_value))

        self.wrln('    }\n')

        if GEN_DESTRUCTOR:
            # Note: Dart does not have destructors!
            pass

        self.wrln('}\n')

        log('create_class_dart done: ' + self.class_name)
        log('')

    #----------------------------------------------------------------

    # Create class from self.field_list for all selected languages
    def create_classes(self):

        self.class_name = self.table_to_class_name(self.table_name)

        if GEN_PYTHON:
            self.create_class_python()

        if GEN_MATLAB:
            self.create_class_matlab()

        if GEN_CPP:
            self.create_class_cpp()

        if GEN_DELPHI:
            self.create_class_delphi()

        if GEN_DART:
            self.create_class_dart()


    # Write comment header at top of file
    def write_file_header(self, lang):
        # Get file size
        self.outf.seek(0, 2)
        size = self.outf.tell()
        if size == 0:
            comment, _ = get_field_type_lang('', '#comment', lang)
            self.write(comment + '\n')
            self.write(comment + ' Automatic generated file by xlsx2sql.py\n')
            self.write(comment + ' Origin file: ' + self.origin_filename + '\n')
            self.write(comment + '\n')
            self.write('\n')
            return True

        return False

    # Write comment header at top of file
    def write_source_header(self, lang):
        comment, _ = get_field_type_lang('', '#comment', lang)
        self.wrln(comment + ' Source file: ' + self.source_filename)


    # Convert table name to valid class name
    def table_to_class_name(self, tname):
        cname = ''
        words = tname.split('_')
        for w in words:
            cname = cname + w.title()
        return cname


    # Open output file
    def open_out(self, ext, create_new = False):
        self.out_filename = self.base_filename + ext
        log('output file: ' + self.out_filename)

        # Open file for append
        if self.outf:
            self.outf.close()

        if create_new:
            self.outf = open(self.out_filename, 'wt')
        else:
            self.outf = open(self.out_filename, 'a')


    # Read output file as lines
    def read_out(self, ext):
        filename = self.base_filename + ext
        lines = []
        if os.path.exists(filename):
            with open(filename) as f:
                lines = f.read().splitlines()

        return lines


    # Write text to output file, optionally flush
    def write(self, text, flush = True):
        #print(text)
        self.outf.write(text)
        if flush:
            self.outf.flush()


    # Write line to output file, optionally flush
    def wrln(self, text, flush = True):
        #print(text)
        self.outf.write(text)
        self.outf.write('\n')
        if flush:
            self.outf.flush()



#============================================================================

# Extract all sheets from xlsx file to output folder
def extract_xlsx(filename):
    log('extract_csv started: ' + filename)
    path, fname = os.path.split(filename)
    fn, ext = os.path.splitext(fname)
    db_name = fn.split('__')[0].lower()
    out_path = os.path.join(path, db_name, 'csv')
    global OUTPUT_PATH
    OUTPUT_PATH = os.path.join(path, db_name)
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
        csv_fname = os.path.join(out_path, db_name + ' - ' + sheet_name.lower() + '.csv')
        log('write csv file: ' + csv_fname)
        with open(csv_fname, 'w', newline="") as f:
            c = csv.writer(f)
            for r in sheet.rows:
                c.writerow([cell.value for cell in r])

        csv_count += 1

    log('extract_csv done: ' + filename)
    log('csv files created: ' + str(csv_count))

    return out_path, db_name


# Process XLSX file with database definitions
def process_xlsx_file(filename):
    log('process_xlsx_file: ' + filename)
    if not os.path.exists(filename):
        log('file not found: ' + filename)
        return

    # Extract all sheets from xlsx file to output folder
    global XLSX_FILENAME
    XLSX_FILENAME = filename
    csv_path, db_name = extract_xlsx(filename)

    # Process all sheets in folder
    process_folder(csv_path, ['.csv'], db_name, False)
    log('process_xlsx_file done: ' + filename)


# Process CSV file with database definitions
def process_csv_file(filename, db_name):
    log('processing csv: ' + filename)
    if not os.path.exists(filename):
        log('file not found: ' + filename)
        return

    path, fname = os.path.split(filename)

    # Skip [common fields csv files]
    if fname.find('(') > -1:
        log('ignoring csv file: ' + filename)

    else:
        # Set database from file name
        log('')
        db = DatabaseDef()
        db.origin_filename = XLSX_FILENAME
        db.set_db(filename, db_name)
        db.load_table_csv(filename)
        if len(db.field_list) > 0:

            if GEN_POSTGRES:
                db.create_table_postgres()

            if GEN_FIREBIRD:
                db.create_table_firebird()

            if GEN_SQLITE:
                db.create_table_sqlite()

            # Create classes for languages
            db.create_classes()


# Process folder with CSV database definition files
def process_folder(fpath, ext_list, db_name, subdirs = True):

    # Get list of files in folder
    if subdirs:
        flist = glob.glob(os.path.join(fpath, '**/*.*'), recursive=True)
    else:
        flist = glob.glob(os.path.join(fpath, '*.*'), recursive=False)

    # Step 1:
    for filename in flist:

        # Prepare list of common fields
        for ext in ext_list:
            if filename.lower().endswith(ext.lower()):

                if ext == '.xlsx':
                    process_xlsx_file(filename)

                elif ext == '.csv':
                    process_csv_file(filename, db_name)


#============================================================================

def main():

    # Read command line options
    parser = argparse.ArgumentParser()

    # Arguments
    parser.add_argument('-f', dest='xlsx',      default='unittest.xlsx', help='input xlsx file')
    parser.add_argument('-d', dest='dir',       default=None,            help='input folder, all .xlsx files will be processed')
    parser.add_argument('-s', dest='subdirs',   action='store_true',     default=False,   help='Process xlsx files in subfolders')
    args = parser.parse_args()

    astro_path = os.getenv('ASTROPACK_PATH')
    if args.dir:
        process_folder(args.dir, ['.xlsx'], args.subdirs)

    elif args.xlsx:
        filename = args.xlsx
        path, fname = os.path.split(filename)
        if path == '':
            filename = os.path.join(astro_path, 'database', 'xlsx', fname)

        process_xlsx_file(filename)

    else:
        print('No input operation specified')

if __name__ == '__main__':
    main()
