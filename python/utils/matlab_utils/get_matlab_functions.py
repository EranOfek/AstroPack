# Automatic tool to extract functions from matlab source files
# with their H1 comments.

#
# Outputs:
# For each .m file - txt file with function list



import os, glob, argparse
from datetime import datetime

UPDATE_M = False


FILE_EXT = ['.m']
out_path = 'c:/temp/funcs/'

cur_folder = ''
class_name = ''

# Log message to file
LOG_PATH = 'c:/temp/'
logfile = open(os.path.join(LOG_PATH, 'get_matlab_functions.log'), 'a')
def log(msg, dt = False):
    global logfile
    if msg == '': dt = False
    if dt: msg = datetime.now().strftime('%d/%m/%y %H:%M:%S.%f')[:-3] + ' ' + msg
    print(msg)
    if logfile:
        logfile.write(msg)
        logfile.write("\n")
        logfile.flush()


# Extract H1 comment from comment lines below the function line
# function Result = openConn(Obj)
#    % Open connection, throw exception on failure
def get_comment(lines, idx):
    comment = ''

    # Look for comment line below the function line
    count = 1
    while lines[idx+count].strip().startswith('%'):
        line = lines[idx+count].strip().replace('\t', ' ')
        line = line.replace('%', '').strip()
        line = line.replace('--', '').strip()
        line = line.replace('==', '').strip()
        words = line.split(' ')
        words_lower = line.lower().split(' ')
        if 'example' in words_lower:
            break
        comment_line = ' '.join(words)
        comment = (comment + ' ' + comment_line).strip()
        count = count + 1
        if count >= 5 or len(comment) > 120:
            break

    return comment



def update_m_file(fname, func_list_comments):

    # Read source file
    with open(fname) as f:
        lines = f.read().splitlines()

    # Check if we already have comment block

    for i, line in enumerate(lines):
        pass

    outf = open(fname, 'wt')
    for line in lines:

        # Remove trailing spaces
        line = line.rstrip()
        outf.write(line)
        outf.write('\n')

    outf.close()


# Process single .m file
def process_file(fname):
    global cur_folder, class_name

    cur_folder = os.path.split(fname)[0]
    cur_last = cur_folder.path.split(fname)[0]

    # Folder name is class
    if cur_last.startswith('@'):
        class_name = cur_last

    func_list_comments = []

    # Read source file
    with open(fname) as f:
        lines = f.read().splitlines()

    # Add empty lines at beginning and end to allow +/-1 indexing without exceptions
    lines.insert(0, '')
    lines.append('')

    # Create output file
    path, fn = os.path.split(fname)
    out_fname = os.path.join(out_path, fn + '.txt')
    outf = open(out_fname, 'wt')

    # Process line by line
    class_name = ''
    methods_type = ''
    for line_num, line in enumerate(lines):
        try:
            # Skip comment lines
            if line.strip().startswith('%'):
                continue

            # Skip empty lines
            line = line.replace('/t', '    ').strip()
            if line == '':
                continue

            # Add spaces for easy split
            line = line.replace('=', ' = ')
            line = line.replace('(', ' ( ')
            line = line.replace(')', ' ) ')
            tokens = line.split(' ')

            # methods
            if 'methods' in tokens:
                new_methods_type = ''
                if 'Static' in tokens:
                    new_methods_type = 'Static'

                # swtiched type
                if new_methods_type != methods_type:
                    methods_type = new_methods_type
                    #outf.write('\n% methods ' + methods_type + '\n%\n')

            # classdef
            if tokens[0] == 'classdef':
                class_name = tokens[1]
                outf.write('% class: {}\n%\n'.format(class_name))
                continue

            # function
            if 'function' in tokens:

                # Get function name
                if '=' in line:
                    func_name = line.split('=')[1].strip().split('(')[0].strip()
                else:
                    func_name = line.split('function')[1].strip().split('(')[0].strip()

                outline = '% ' + func_name
                if methods_type != '':
                    outline = outline + ' (' + methods_type + ')'

                if func_name != '':
                    # Get comment
                    comment = get_comment(lines, line_num)
                    outline = outline + ' - ' + comment
                    outf.write(outline + '\n')
        except:
            log('exception parsing line: ' + line)

    # Done
    outf.close()

    if UPDATE_M:
        update_m_file(fname, func_list_comments)



# Process folder with recursion
def process_folder(fpath, subdirs = True):
    ext_list = ['.m']
    if subdirs:
        flist = glob.glob(os.path.join(fpath, '**/*.*'), recursive=True)
    else:
        flist = glob.glob(os.path.join(fpath, '*.*'), recursive=False)

    for fname in flist:
        fnlower = fname.lower()
        for ext in ext_list:
            ext = ext.lower()
            if fnlower.endswith(ext):
                try:
                    log('Processing file: ' + fname)
                    process_file(fname)
                except:
                    log('Exception processing file: ' + fname)


def main():

    # Read command line options
    parser = argparse.ArgumentParser()

    # Arguments
    parser.add_argument('-d',           dest='dir',         default=None,                                   help='pcap folder')
    parser.add_argument('-s',           dest='subdirs',     action='store_true',    default=True,   help='Process pcap files in subfolders')

    parser.add_argument('-o', dest='outdir', default=None, help='Output folder')

    #process_file('c:/temp/DbQuery.m')
    process_folder('D:/Ultrasat/AstroPack.git/matlab')

    #args = parser.parse_args()

    folder = '.'
    #process_folder(folder, True)


if __name__ == '__main__':
    main()
