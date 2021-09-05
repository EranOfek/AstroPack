# Automatic tool to extract functions from matlab source files
# with their H1 comments.

# Todo:
#    Handle correctly static class functions that are defined in separate file
#    Handle non-class functions
#    Generate unitTest() skeleton with functions list in comments (by function order in file)
#    Generate mlx skeleton (if possible), check if we can generate HTML and import it, or just text?
#    MLX is Open Packaging Conventions, there are Python packages to manipulate it

#
# Outputs:
# For each .m file - txt file with function list

import os, glob, argparse
from datetime import datetime

ASTROPACK_PATH = os.getenv('ASTROPACK_PATH')
AUTOGEN_PATH = os.path.join(ASTROPACK_PATH, 'matlab/doc/autogen')
UPDATE_M = False #True
UPDATE_M_OUT_FILE = False
TRIM_TRAILING_SPACES = True


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


def log_line(msg, line_num, line):
    log(msg + ' (line ' + str(line_num) + '): ' + line)

# ===========================================================================

class PackageData:

    def __init__(self):
        self.package_name = ''
        self.class_list = {}
        self.func_list = {}

# ===========================================================================

class ClassData:

    def __init__(self):
        self.class_name = ''
        self.func_list = {}
        self.prop_list = []

# ===========================================================================

class FunctionData:

    def __init__(self):
        self.function_name = ''
        self.type = ''              # Static
        self.file_name = ''         # Implementation file name
        self.params = ''

# ===========================================================================


# ---------------------------------------------------------------------------
class MatlabProcessor:

    def __init__(self):
        self.out_path = os.path.join(AUTOGEN_PATH, 'all_classes')
        self.cur_fname = ''
        self.cur_folder = ''
        self.cur_class = ''
        self.cur_package = ''
        self.cur_package_class = ''
        self.package_list = []
        self.class_list = []
        self.func_list = []
        self.pkg_func_list = []
        self.func_type = {}
        self.is_class_folder = False
        self.is_class_file = False
        self.class_fname = ''
        self.unitTest_lines = []
        self.mex_lines = []

    # -----------------------------------------------------------------------
    def get_package_from_path(self, path):
        path = path.replace('\\', '/')
        names = path.split('/')
        pkg_name = ''
        for fn in names:
            if fn.startswith('+'):
                if pkg_name != '':
                    pkg_name = pkg_name + '.'
                pkg_name = pkg_name + fn[1:]

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

            # Stop on special cases
            if 'example' in words_lower:
                break
            if len(words) > 0 and words[0] == 'Input':
                break

            comment_line = ' '.join(words)
            comment = (comment + ' ' + comment_line).strip()

            # Stop if comment is too long
            count = count + 1
            if count >= 5 or len(comment) > 300:
                break

        return comment

    # -----------------------------------------------------------------------
    def update_m_file(self, fname):

        # Read source file
        with open(fname) as f:
            lines = f.read().splitlines()

        start_idx = lines.index('% #functions') if '% #functions' in lines else -1
        end_idx = lines.index('% #/functions', start_idx) if '% #/functions' in lines else -1

        if start_idx > -1 and end_idx > -1:
            lines = lines[:start_idx] + lines[end_idx + 2:]
        else:
            start_idx = 0

            #
            for i, line in enumerate(lines):
                if not line.startswith('%'):
                    start_idx = i+1
                    break

        self.func_list.sort()

        # Insert functions list at to of file
        #lines.insert(start_idx, '% #AutoGen')
        lines.insert(start_idx+0, '% #functions')  # (auto-generated list python script)
        for i, func in enumerate(self.func_list):
            lines.insert(start_idx+i+1, func)

        lines.insert(start_idx + 1 + len(self.func_list), '% #/functions')
        lines.insert(start_idx + 2 + len(self.func_list), '%')

        # Write output file
        if UPDATE_M_OUT_FILE:
            out_fname = fname + '$out.m'
        else:
            out_fname = fname
            bkp_fname = fname + '$bkp.m'
            if os.path.exists(bkp_fname):
                os.remove(bkp_fname)
            os.rename(fname, bkp_fname)


        with open(out_fname, 'wt') as f:
            for line in lines:
                if TRIM_TRAILING_SPACES:
                    line = line.rstrip()
                f.write(line + '\n')

        log('update_m_file done: ')

    # -----------------------------------------------------------------------
    def update_files(self, fname):

        # Create output file
        path, fn = os.path.split(fname)

        # Sort the function list
        self.func_list.sort()
        self.pkg_func_list.sort()

        out_func_list = self.func_list

        # Todo
        #if len(self.func_list) > 0:
        #    out_func_list = self.func_list
        #else:
        #    out_func_list = self.pkg_func_list

        if self.is_class_folder:
            out_fname = os.path.join(self.out_path, self.cur_package_class + '.txt')
        else:
            pre, ext = os.path.splitext(fn)
            out_fname = os.path.join(self.out_path, pre + '.txt')

        # Write function list file
        with open(out_fname, 'wt') as f:
            f.write('% class: {}\n%\n'.format(self.cur_class))
            for line in out_func_list:
                f.write(line + '\n')

        if UPDATE_M:
            if self.is_class_folder:
                self.update_m_file(self.class_fname)
            else:
                self.update_m_file(fname)

    # -----------------------------------------------------------------------
    def write_mlx(self):
        log('write_mlx')

    # -----------------------------------------------------------------------
    def write_unitTest(self):
        log('write_unitTest')

    # -----------------------------------------------------------------------
    # Process single .m file
    def process_file(self, fname):

        if 'LogLevel' in fname:
            log('LogLevel')

        fname = fname.replace('\\', '/')
        self.cur_fname = fname
        self.cur_folder, self.cur_file = os.path.split(fname)
        cur_last = os.path.split(self.cur_folder)[1]
        #cur_f = os.path.splitext(self.cur_file)

        # Folder name is class
        self.is_class_file = False
        self.is_class_folder = False
        if cur_last.startswith('@'):

            self.is_class_folder = True
            class_name = cur_last[1:]

            # Set main class file name
            self.class_fname = os.path.join(self.cur_folder, class_name + '.m')

            if class_name != self.cur_class:
                cur_class = class_name
                self.func_list = []
                self.func_type = {}
        else:
            self.cur_class = ''
            self.func_list = []
            self.func_type = {}

        # Check if we have packages
        pkg = self.get_package_from_path(self.cur_folder)
        if pkg != self.cur_package:
            self.cur_package = pkg

        # Update package list
        if pkg != '' and not pkg in self.package_list:
            self.package_list.append(pkg)

        # Read source file
        with open(fname) as f:
            lines = f.read().splitlines()

        # Add empty lines at beginning and end to allow +/-1 indexing without exceptions
        lines.insert(0, '')
        lines.append('')

        # -------------------------------------------------------------------
        # Look for classdef
        self.is_class_file = False
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

                # classdef
                if tokens[0] == 'classdef':
                    self.is_class_file = True
                    self.cur_class = tokens[1]
                    log_line('found classdef', line_num, line)

                    self.func_list = []

                    # package.class
                    if self.cur_package != '':
                        self.cur_package_class = self.cur_package + '.' + self.cur_class
                    else:
                        self.cur_package_class = self.cur_class

                    # Update class list
                    if not self.cur_package_class in self.class_list:
                        self.class_list.append(self.cur_package_class)

            except:
                log('exception parsing line: ' + line)

        # -------------------------------------------------------------------

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


                # Get function name
                if 'function' in tokens:

                    # Get name
                    if '=' in line:
                        func_name = line.split('=')[1].strip().split('(')[0].strip()
                    else:
                        func_name = line.split('function')[1].strip().split('(')[0].strip()

                    if func_name != '':

                        #log_line('found function', line_num, line)

                        # Get comment
                        comment = self.get_comment(lines, line_num)

                        # Prepare output line
                        outline = '% ' + func_name
                        if methods_type != '':
                            outline = outline + ' (' + methods_type + ')'

                        outline = outline + ' - ' + comment
                        if self.cur_class != '' or self.is_class_folder:
                            self.func_list.append(outline)
                        else:
                            self.pkg_func_list.append(outline)

            except:
                log('exception parsing line: ' + line)

        # Update output only if not class folder
        if not self.is_class_folder and self.cur_class != '':
            self.update_files(fname)

    # -----------------------------------------------------------------------
    # Process folder files (without recursion)
    def process_folder(self, path):
        path = path.replace('\\', '/')
        log('process_folder: ' + path)

        self.is_class_folder = False
        flist = glob.glob(os.path.join(path, '*.*'), recursive=False)
        files_to_process = []
        for fname in flist:
            fname = fname.replace('\\', '/')
            fnlower = fname.lower()

            # Skip files in doc/autogen folder
            if 'doc/autogen' in fnlower:
                continue

            #
            if fnlower.endswith('.m') and not fnlower.endswith('$out.m') and not fnlower.endswith('bkp.m'):
                files_to_process.append(fname)

        # Found files
        if len(files_to_process) > 0:

            # Check if this is a class folder
            self.cur_folder = path
            cur_last = os.path.split(self.cur_folder)[1]
            self.is_class_folder = False
            if cur_last.startswith('@'):
                self.is_class_folder = True
                class_name = cur_last[1:]

                # Set main class file name
                self.class_fname = os.path.join(self.cur_folder, class_name + '.m').replace('\\', '/')

                if class_name != self.cur_class:
                    cur_class = class_name
                    self.func_list = []
                    self.func_type = {}

                if os.path.exists(self.class_fname):
                    for idx, fname in enumerate(files_to_process):
                        _, fn = os.path.split(fname)
                        if fn.lower() == class_name.lower() + '.m':
                            files_to_process.pop(idx)
                            files_to_process.insert(0, fname)
                            break
                else:
                    log('Missing class file: '  + self.class_fname)

            # Process files
            for fname in files_to_process:
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
    def process_tree(self, path):
        path = path.replace('\\', '/')
        folders = glob.glob(os.path.join(path, '**/'), recursive=True)
        #folders.insert(0, path)
        for folder in folders:
            folder = folder.replace('\\', '/')

            # Skip files in doc/autogen folder
            if 'doc/autogen' in folder.lower():
                continue

            tokens = folder.lower().split('/')
            skip_folder = False
            for tok in tokens:
                tok = tok.replace('_', '')
                if 'unused' in tok or 'obsolete' in tok:
                    skip_folder = True
                    break

            if not skip_folder:
                self.process_folder(folder)

    # -----------------------------------------------------------------------
    def process(self, path):

        package_list_filename = os.path.join(AUTOGEN_PATH, 'package_list.txt')
        class_list_filename = os.path.join(AUTOGEN_PATH, 'class_list.txt')

        # Load current package _list
        if os.path.exists(package_list_filename):
            with open(package_list_filename) as f:
                self.package_list = f.read().splitlines()

        # Load current class_list
        if os.path.exists(class_list_filename):
            with open(class_list_filename) as f:
                self.class_list = f.read().splitlines()

        # Process folders tree
        self.process_tree(path)
        # process_file('c:/temp/DbQuery.m')

        # Update package list files
        self.package_list.sort()
        with open(package_list_filename, 'wt') as f:
            for line in self.package_list:
                f.write(line)
                f.write('\n')

        # Update class list files
        self.class_list.sort()
        with open(class_list_filename, 'wt') as f:
            for line in self.class_list:
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
    proc.process('D:/Ultrasat/AstroPack.git/matlab/')
    #proc.process('D:/Ultrasat/AstroPack.git/matlab/base')

    #proc.process('D:\\Ultrasat\\AstroPack.git\\matlab\\util\\+tools\\+interp')
    #proc.process('D:/Ultrasat/AstroPack.git/matlab/base/@Base')
    #proc.process('D:/Ultrasat/AstroPack.git/matlab/util/+db')



if __name__ == '__main__':
    main()
