# Automatic tool to extract functions from matlab source files
# with their H1 comments.

#
# Outputs:
# For each .m file - txt file with function list

import os, glob, argparse
from datetime import datetime

ASTROPACK_PATH = os.getenv('ASTROPACK_PATH')
UPDATE_M = True #False


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

# ---------------------------------------------------------------------------
class MatlabProcessor:

    def __init__(self):
        self.out_path = os.path.join(ASTROPACK_PATH, 'matlab/doc/autogen/all_classes')
        self.cur_folder = ''
        self.cur_class = ''
        self.cur_package = ''
        self.cur_package_class = ''
        self.package_list = []
        self.class_list = []
        self.func_list = []
        self.is_class_folder = False

    # -----------------------------------------------------------------------
    def get_package_from_path(self, path):
        path = path.replace('\\', '/')
        names = path.split('/')
        pkg_name = ''
        for fn in names:
            if fn.startswith('+'):
                if pkg_name != '':
                    pkg_name = pkg_name + '.'
                pkg_name = pkg_name + fn

        return pkg_name

    # -----------------------------------------------------------------------
    # Extract H1 comment from comment lines below the function line
    # function Result = openConn(Obj)
    #    % Open connection, throw exception on failure
    def get_comment(self, lines, idx):
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

    # -----------------------------------------------------------------------
    def update_m_file(self, fname, func_list):

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

    # -----------------------------------------------------------------------
    def update_files(self, fname):

        # Create output file
        path, fn = os.path.split(fname)

        # Sort the function list
        self.func_list.sort()

        if self.is_class_folder:
            out_fname = os.path.join(self.out_path, self.cur_class + '.txt')
        else:
            out_fname = os.path.join(self.out_path, fn + '.txt')

        # Write function list file
        with open(out_fname, 'wt') as f:
            f.write('% class: {}\n%\n'.format(self.cur_class))
            for line in self.func_list:
                f.write(line + '\n')

        if self.is_class_file:
            update_m = True

        if update_m and UPDATE_M:
            self.update_m_file(fname, self.func_list)

    # -----------------------------------------------------------------------
    # Process single .m file
    def process_file(self, fname):

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
            if class_name != self.cur_class:
                cur_class = class_name
                func_list = []
        else:
            cur_class = ''
            func_list = []

        # Check if we have packages
        pkg = self.get_package_from_path(cur_folder)
        if pkg != self.cur_package:
            cur_package = pkg

        # Update package list
        if pkg != '' and not pkg in self.package_list:
            self.package_list.append(pkg)

        # Read source file
        with open(fname) as f:
            lines = f.read().splitlines()

        # Add empty lines at beginning and end to allow +/-1 indexing without exceptions
        lines.insert(0, '')
        lines.append('')

        # Process .m source file line by line
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

                    # package.class
                    if cur_package != '':
                        cur_package_class = cur_package + '.' + cur_class
                    else:
                        cur_package_class = cur_class

                    # Update class list
                    if not cur_package_class in self.class_list:
                        self.class_list.append(cur_package_class)

                    continue

                # Get function name
                if 'function' in tokens:

                    # Get name
                    if '=' in line:
                        func_name = line.split('=')[1].strip().split('(')[0].strip()
                    else:
                        func_name = line.split('function')[1].strip().split('(')[0].strip()

                    if func_name != '':
                        # Get comment
                        comment = self.get_comment(lines, line_num)

                        # Prepare output line
                        outline = '% ' + func_name
                        if methods_type != '':
                            outline = outline + ' (' + methods_type + ')'

                        outline = outline + ' - ' + comment
                        func_list.append(outline)

            except:
                log('exception parsing line: ' + line)

        # Update output only if not class folder
        if not is_class_folder:
            self.update_files(fname)

    # -----------------------------------------------------------------------
    # Process folder files (without recursion)
    def process_folder(self, fpath):
        fpath = fpath.replace('\\', '/')
        log('process_folder: ' + fpath)

        self.is_class_folder = False
        flist = glob.glob(os.path.join(fpath, '*.*'), recursive=False)
        for fname in flist:
            fname = fname.replace('\\', '/')
            fnlower = fname.lower()

            # Skip files in doc/autogen folder
            if 'doc/autogen' in fnlower:
                continue

            #
            if fnlower.endswith('.m'):
                try:
                    log('Processing file: ' + fname)
                    self.process_file(fname)
                except:
                    log('Exception processing file: ' + fname)


        # Update output only if class folder
        if self.is_class_folder:
            self.update_files('')

    # -----------------------------------------------------------------------
    # Process folder with recursion
    def process_tree(self, fpath, subdirs = True):
        folders = glob.glob(os.path.join(fpath, '*/'))
        folders.insert(0, fpath)
        for folder in folders:

            # Skip files in doc/autogen folder
            if 'doc/autogen' in folder.lower():
                continue

            self.process_folder(folder, False)

    # -----------------------------------------------------------------------
    def process(self, path):

        package_list_filename = os.path.join(ASTROPACK_PATH, 'package_list.txt')
        class_list_filename = os.path.join(ASTROPACK_PATH, 'package_list.txt')

        # Load current package _list
        if os.path.exists(package_list_filename):
            with open(package_list_filename) as f:
                package_list = f.read().splitlines()

        # Load current class_list
        if os.path.exists(class_list_filename):
            with open(class_list_filename) as f:
                class_list = f.read().splitlines()

        # Process folders tree
        self.process_tree('D:/Ultrasat/AstroPack.git/matlab/base/@Base')
        # process_file('c:/temp/DbQuery.m')

        # Update package list files
        package_list.sort()
        with open(package_list_filename) as f:
            for line in package_list:
                f.write(line)
                f.write('\n')

        # Update class list files
        class_list.sort()
        with open(class_list_filename) as f:
            for line in class_list:
                f.write(line)
                f.write('\n')


# ---------------------------------------------------------------------------
def main():

    # Read command line options
    parser = argparse.ArgumentParser()

    # Arguments
    parser.add_argument('-d',           dest='dir',         default=None,                                   help='pcap folder')
    parser.add_argument('-s',           dest='subdirs',     action='store_true',    default=True,   help='Process pcap files in subfolders')
    parser.add_argument('-o', dest='outdir', default=None, help='Output folder')
    args = parser.parse_args()

    #
    proc = MatlabProcessor()
    proc.process('D:/Ultrasat/AstroPack.git/matlab/base/@Base')


if __name__ == '__main__':
    main()
