# Convert Delphi/Lazarus forms (.DFM/.LFM) files to cvui source code
#
# Author: Chen Tishler (Aug 2021)
#

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


def_lines = []
code_lines = []

def wrdef(text, flush = True):
    print(text)
    global def_lines
    def_lines.append(text)


def wrcode(text, flush = True):
    print(text)
    global code_lines
    code_lines.append(text)

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

    form_name = ''
    window_name = ''
    object_name = ''
    object_type = ''
    parent_left = 0
    parent_top = 0
    left, top, width, height = 0, 0, 0, 0
    caption = ''
    canvas = ''
    generate = False
    type_list = ['TPanel', 'TButton', 'TLabel', 'TEdit', 'TCheckBox']
    ctrl_count = 0

    for line in lines:
        line = line.strip()

        # Key = Value
        if line.find(' = ') > -1:
            key, value = line.split('=')
            key = key.strip()
            value = value.strip()

            # Left, Top, Width, Height, Caption
            if key == 'Left':
                left = int(value) + parent_left
                if object_type == 'TPanel':
                    parent_left = left
            elif key == 'Top':
                top = int(value) + parent_top
                if object_type == 'TPanel':
                    parent_top = top
            elif key == 'Width':
                width = int(value)

                if ctrl_count == 1:
                    window_name = caption
                    wrcode('')
                    wrcode('# Create canvas and window')
                    wrcode('%s = np.zeros((%d, %d, 3), np.uint8)' % (canvas, height, width))
                    wrcode('cvui.init(%s_Caption)' % window_name)
                    wrcode('text_width = 0.4')
                    wrcode('')
                    wrcode('# cvui loop')
                    wrcode('while True:')
                    wrcode('    # Fill canvas with background color')
                    wrcode('    %s[:] = (49, 52, 49)' % canvas)
                    wrcode('')

            elif key == 'Height':
                height = int(value)
            elif key == 'Caption':
                caption = value;

        # 'end' or child object
        if (line == 'end' or line.startswith('object')) and generate:

            # Generate
            if width != 0 and height != 0:
                wrdef('')
                wrdef('# ' + object_type + ': ' + caption)
                prefix = form_name + '_' + object_name
                wrdef(prefix + '_Left   = ' + str(left))
                wrdef(prefix + '_Top    = ' + str(top))
                wrdef(prefix + '_Width  = ' + str(width))
                wrdef(prefix + '_Height = ' + str(height))
                wrdef(prefix + '_Caption = ' + caption)

                # Generate code
                # cvui.text(main_gui, 40, 100, 'Pressure:', 0.4)
                if object_type == 'TPanel':
                    wrcode('    # ' + object_type + ': ' + caption)
                    wrcode('')
                elif object_type == 'TButton':
                    wrcode('    # ' + object_type + ': ' + caption)
                    wrcode('    if cvui.button(%s, %s_Left, %s_Top, %s_Caption):' % (canvas, prefix, prefix, prefix))
                    wrcode('        print(1)')
                    wrcode('')
                elif object_type == 'TLabel':
                    wrcode('    # ' + object_type + ': ' + caption)
                    wrcode('    cvui.text(%s, %s_Left, %s_Top, %s_Caption, text_width)' % (canvas, prefix, prefix, prefix))
                    wrcode('')
                elif  object_type == 'TEdit':
                    wrcode('    # ' + object_type + ': ' + caption)
                    wrcode('')
                elif object_type == 'TCheckBox':
                    wrcode('    # ' + object_type + ': ' + caption)
                    wrcode('    if cvui.checkbox(%s, %s_Left, %s_Top, %s_Caption, %s):' % (canvas, prefix, prefix, prefix, object_name))
                    wrcode('        print(1)')
                    wrcode('')

            object_name = ''
            object_type = ''
            left, top, width, height = 0, 0, 0, 0
            caption = ''

        # Object, including TForm, TFrame
        if line.startswith('object '):
            ctrl_count = ctrl_count + 1
            object_name = line.split(' ')[1].strip().replace(':', '')
            object_type = line.split(':')[1].strip()

            # First object is TForm or TFrame
            if form_name == '':
                form_name = object_name
                canvas = form_name + '_gui'
                generate = True

            generate = object_type in type_list

    #
    wrcode('')
    wrcode('    # Update form')
    wrcode('    cvui.update()')
    wrcode("    cv2.imshow('%s', %s)" % (window_name, canvas))
    wrcode('')
    wrcode('    # Hold for a key')
    wrcode('    keyCode = cv2.waitKey(2) & 0xFF')
    wrcode('')
    wrcode('    # Stop the program on the ESC key')
    wrcode('    if keyCode == 27:')
    wrcode('        break')
    wrcode('')

    fname = filename[:-4]

    #
    with open(fname + '_def.py', 'wt') as outf:
        outf.write('# Generated by dfm2cvui from: ' + filename)
        outf.write('\n\n')
        for line in def_lines:
            outf.write(line)
            outf.write('\n')

    #
    with open(fname + '_code.py', 'wt') as outf:
        outf.write('# Generated by dfm2cvui from: ' + filename)
        outf.write('\n\n')
        outf.write('from ' + os.path.split(fname)[1] + '_def import *\n')
        for line in code_lines:
            outf.write(line)
            outf.write('\n')


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
    parser.add_argument('-f', dest='dfm',       default='test.lfm',      help='input dfm/lfm file')
    parser.add_argument('-d', dest='dir',       default=None,            help='input folder, all dfm/lfm files will be processed')
    parser.add_argument('-s', dest='subdirs',   action='store_true',     default=False,   help='Process xlsx files in subfolders')
    args = parser.parse_args()

    if args.dir:
        process_folder(args.dir, ['.dfm', '.lfm'], args.subdirs)

    elif args.dfm:
        filename = args.dfm
        process_dfm(filename)

    else:
        print('No input operation specified')

if __name__ == '__main__':
    main()
