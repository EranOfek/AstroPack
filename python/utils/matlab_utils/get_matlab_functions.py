# Automatic tool to extract functions from matlab source files
# with their H1 comments.

#
# Outputs:
# For each .m file - txt file with function list



import os, glob, argparse
from datetime import datetime

ASTROPACK_PATH = os.getenv('ASTROPACK_PATH')

UPDATE_M = True #False


FILE_EXT = ['.m']
out_path = os.path.join(ASTROPACK_PATH, 'matlab/doc/autogen/all_classes')

cur_folder = ''
cur_class = ''
cur_package = ''
cur_package_class = ''

package_list = []
class_list = []
func_list = []


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


def update_m_file(fname, func_list):

    # Read source file
    with open(fname) as f:
        lines = f.read().splitlines()

    start_idx = lines.index('% #functions') if '% #functions' in lines else -1
    end_idx = lines.index('% #/functions', start_idx) if '% #/functions' in lines else -1

    if start_idx > -1 and end_idx > -1:
        lines = lines[:start_idx] + lines[start_idx + 1:]
    else:
        start_idx = 0

        #
        for i, line in enumerate(lines):
            if not line.startswith('%'):
                start_idx = i+1
                break

    # Insert functions list at to of file
    lines.insert(start_idx, '% #AutoGen')
    lines.insert(start_idx+1, '% #functions')
    for i, func in enumerate(func_list):
        lines.insert(start_idx+2, func)

    lines.insert(start_idx + 2 + len(func_list), '% #/functions')
    lines.insert(start_idx + 3, '%')

    # Write output file
    out_fname = fname + '_out.m'
    with open(out_fname, 'wt') as f:
        for line in lines:
            line = line.rstrip()
            f.write(line + '\n')

    log('update_m_file done: ')


def get_package_from_path(path):
    path = path.replace('\\', '/')
    names = path.split('/')
    pkg_name = ''
    for fn in names:
        if fn.startswith('+'):
            if pkg_name != '':
                pkg_name = pkg_name + '.'
            pkg_name = pkg_name + fn

    return pkg_name


# Process single .m file
def process_file(fname):
    global cur_folder, cur_class, cur_package, cur_package_class, class_list, func_list

    update_m = False

    fname = fname.replace('\\', '/')
    cur_folder, cur_file = os.path.split(fname)
    cur_last = os.path.split(cur_folder)[1]
    cur_f = os.path.splitext(cur_file)

    # Folder name is class
    is_class_file = False
    is_class_folder = False
    if cur_last.startswith('@'):

        is_class_folder = True
        class_name = cur_last[1:]
        if class_name != cur_class:
            cur_class = class_name
            func_list = []
    else:
        cur_class = ''
        func_list = []

    # Check if we have packages
    pkg = get_package_from_path(cur_folder)
    if pkg != cur_package:
        cur_package = pkg

    if pkg != '' and not pkg in package_list:
        package_list.append(pkg)


    # Read source file
    with open(fname) as f:
        lines = f.read().splitlines()

    # Add empty lines at beginning and end to allow +/-1 indexing without exceptions
    lines.insert(0, '')
    lines.append('')

    # Create output file
    path, fn = os.path.split(fname)

    # Process line by line
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
                is_class_file = True

                cur_class = tokens[1]

                if cur_package != '':
                    cur_package_class = cur_package + '.' + cur_class
                else:
                    cur_package_class = cur_class

                if not cur_package_class in class_list:
                    class_list.append(cur_package_class)

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
                    func_list.append(outline)

        except:
            log('exception parsing line: ' + line)


    # Sort the function list
    func_list.sort()

    if is_class_folder:
        out_fname = os.path.join(out_path, cur_class + '.txt')
    else:
        out_fname = os.path.join(out_path, fn + '.txt')


    # Write function list file
    with open(out_fname, 'wt') as f:
        f.write('% class: {}\n%\n'.format(cur_class))
        for line in func_list:
            f.write(line + '\n')

    if is_class_file:
        update_m = True

    if update_m and UPDATE_M:
        update_m_file(fname, func_list)



# Process folder with recursion
def process_folder(fpath, subdirs = True):
    ext_list = ['.m']
    if subdirs:
        flist = glob.glob(os.path.join(fpath, '**/*.*'), recursive=True)
    else:
        flist = glob.glob(os.path.join(fpath, '*.*'), recursive=False)

    '''
    fpath = fpath.replace('\\', '/')
    folder, file = os.path.split(fpath)
    last = os.path.split(folder)[1]

    # Folder name is class
    is_class_folder = False
    if last.startswith('@'):
        is_class_folder = True
    '''

    for fname in flist:
        fname = fname.replace('\\', '/')
        fnlower = fname.lower()

        # Skip files in doc/autogen folder
        if 'doc/autogen' in fnlower:
            continue

        #
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

    package_list_filename = os.path.join(ASTROPACK_PATH, 'package_list.txt')
    class_list_filename = os.path.join(ASTROPACK_PATH, 'package_list.txt')

    global package_list, class_list

    # Load current class_list
    # Read source file
    if os.path.exists(package_list_filename):
        with open(package_list_filename) as f:
            package_list = f.read().splitlines()

    if os.path.exists(class_list_filename):
        with open(class_list_filename) as f:
            class_list = f.read().splitlines()

    #process_file('c:/temp/DbQuery.m')
    process_folder('D:/Ultrasat/AstroPack.git/matlab/base/@Base')

    #args = parser.parse_args()

    folder = '.'
    #process_folder(folder, True)

    package_list.sort()
    class_list.sort()

    with open(package_list_filename) as f:
        for line in package_list:
            f.write(line)
            f.write('\n')

    with open(class_list_filename) as f:
        for line in class_list:
            f.write(line)
            f.write('\n')



if __name__ == '__main__':
    main()
