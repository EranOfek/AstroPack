# Convert Delphi/Lazarus forms (.DFM/.LFM) files to cvui source code

import os, glob, time, argparse, shutil
from datetime import datetime
from sys import platform

DEBUG = True

OUTPUT_PATH = ''

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
def wrln(text, flush = True):
    print(text)
    global outf
    outf.write(text)
    outf.write('\n')
    if flush:
        outf.flush()



#============================================================================

#============================================================================


'''
object Form3: TForm3
  Left = 319
  Height = 343
  Top = 312
  Width = 575
  Caption = 'Testing Window (Preview)'
  ClientHeight = 343
  ClientWidth = 575

'''



def process_dfm(filename):

    log('process_dfm started : ' + filename)

    with open(filename) as f:
        lines = f.read().splitlines()

    object_name = ''
    object_type = ''
    left, top, width, height = 0
    caption = ''

    for line in lines:
        line = line.rstrip()

        if line.strip().startswith('object '):
            object_name = line.split(' ').strip.replace(':', '')
            object_type = line.split(':')[1].strip()

        if line.find(' = ') > -1:
            key, value = line.split('=').strip()

            if key == 'Left':
                left = int(value)
            elif key == 'Height':
                height = int(value)
            elif key == 'Top':
                top = int(value)
            elif key == 'Width':
                width = int(value)
            elif key == 'Caption':
                caption = value;

        if line.strip() == 'end':

            # Generate
            if left != 0 and top != 0 and width != 0 and height != 0:
                wrln('#')
                wrln(object_name + '_Left   = ' + str(left))
                wrln(object_name + '_Top    = ' + str(left))
                wrln(object_name + '_Width  = ' + str(left))
                wrln(object_name + '_Height = ' + str(left))

                # Generate code
                # cvui.text(main_gui, 40, 100, 'Pressure:', 0.4)
                if object_type == 'TButton':
                    wrln('')
                elif object_type == 'TLabel':
                    wrln('')
                elif  object_type == 'TEdit':
                    wrln('')
                elif object_type == 'TCheckBox':
                    wrln('')

            object_name = ''
            object_type = ''
            left, top, width, height = 0
            caption = ''

        wrln(line)
        wrln('\n')

    wrln('\n\n')

    log('process_dfm: ' + filename)
    log('')

#============================================================================

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
                if ext == '.dfm' or ext == '.lfm':
                    process_dfm(filename)

#============================================================================

def main():

    # Read command line options
    parser = argparse.ArgumentParser()

    # Arguments
    parser.add_argument('-f', dest='xlsx',      default='unittest.xlsx', help='input xlsx file')
    parser.add_argument('-d', dest='dir',       default=None,            help='input folder, all .xlsx files will be processed')
    parser.add_argument('-s', dest='subdirs',   action='store_true',     default=False,   help='Process xlsx files in subfolders')
    args = parser.parse_args()

    astro_path = os.getenv('ASTROPACKPATH')
    if args.dir:
        process_folder(args.dir, ['.xlsx'], args.subdirs)

    elif args.xlsx:
        filename = args.xlsx
        path, fname = os.path.split(filename)
        if path == '':
            filename = os.path.join(astro_path, 'database', 'xlsx', fname)

        process_dfm(filename)

    else:
        print('No input operation specified')

if __name__ == '__main__':
    main()
