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
        self.name = ''
        self.path = ''
        self.class_list = {}
        self.func_list = {}
        self.comment = ''

# ===========================================================================

class ClassData:

    def __init__(self):
        self.name = ''
        self.path = ''
        self.filename = ''
        self.func_dict = {}
        self.prop_dict = {}
        self.comment = ''
        self.unitTest_lines = []

# ===========================================================================

class FunctionData:

    def __init__(self):
        self.name = ''
        self.filename = ''
        self.type = ''              # Static
        self.params = ''
        self.comment = ''


class PropData:

    def __init__(self):
        self.name = ''
        self.type = ''              # Datatype
        self.comment = ''

# ===========================================================================

class MatlabProcessor:

    def __init__(self):
        self.out_path = os.path.join(AUTOGEN_PATH, 'all_classes')
        self.cur_fname = ''
        self.cur_folder = ''
        self.cur_class = ''
        self.package_dict = {}
        self.class_dict = {}
        self.is_class_folder = False
        self.is_class_file = False
        #self.mlx_lines = []

        self.package_list_filename = ''
        self.class_list_filename = ''


    # Read lines from file
    def read_file(self, fname, fail_non_exist = False):
        lines = []
        if os.path.exists(fname):
            with open(fname) as f:
                lines = f.read().splitlines()
        elif fail_non_exist:
            log('File not found: ' + fname)
            raise Exception('File not found: ' + fname)

        return lines


    # Write lines
    def write_file(self, fname, lines):
        with open(fname, 'wt') as f:
            for line in lines:
                f.write(line + '\n')

    # -----------------------------------------------------------------------
    def get_package(self, pkg_name, path = ''):
        if pkg_name in self.package_dict:
            pkg = self.package_dict[pkg_name]
        else:
            pkg = PackageData()
            pkg.name = pkg_name
            pkg.path = path
            self.package_dict[pkg_name] = pkg
            log('adding package: {} - folder: {}'.format(pkg_name, path))

        return pkg

    # -----------------------------------------------------------------------
    def get_class(self, class_name, path = ''):
        if class_name in self.class_dict:
            cls = self.class_dict[class_name]
        else:
            cls = ClassData()
            cls.name = class_name
            cls.path = path
            self.class_dict[class_name] = cls
            log('adding class: {} - folder: {}'.format(class_name, path))

        return cls
    # -----------------------------------------------------------------------
    # Get package name from path
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


    # Get class name from path
    def get_class_from_path(self, path):
        last = os.path.split(path)[1]
        if last.startswith('@'):
            return last[1:]
        else:
            return ''
    # -----------------------------------------------------------------------
    # Extract H1 comment from comment lines below the function/class line
    # function Result = openConn(Obj)
    #    % Open connection, throw exception on failure
    def get_comment(self, lines, idx):
        comment = ''

        # Look for comment line below the function line
        count = 1
        while idx + count < len(lines):
            if not lines[idx+count].strip().startswith('%'):
                break

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

            # Append text to comment
            comment_line = ' '.join(words)
            comment = (comment + ' ' + comment_line).strip()

            # Stop if comment is too long, by number of lines, or by text length
            count = count + 1
            if count >= 5 or len(comment) > 300:
                break

        return comment

    # -----------------------------------------------------------------------
    # Remove current info block and get index to line to insert
    def new_info_block(self, lines):
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

        return lines, start_idx

    # -----------------------------------------------------------------------
    def write_m_file(self, fname, lines):

        # Write output file
        if UPDATE_M_OUT_FILE:
            out_fname = fname + '$out.m'
        else:
            out_fname = fname
            bkp_fname = fname + '$bkp.m'
            if os.path.exists(bkp_fname):
                os.remove(bkp_fname)
            os.rename(fname, bkp_fname)

        log('write_m_file: '+ out_fname)
        with open(out_fname, 'wt') as f:
            for line in lines:
                if TRIM_TRAILING_SPACES:
                    line = line.rstrip()
                f.write(line + '\n')

    # -----------------------------------------------------------------------
    def update_class_m_file(self, fname):

        # Read source file
        lines = self.read_file(fname)
        lines, start_idx = self.new_info_block(lines)

        self.func_list.sort()

        # Insert functions list at to of file
        #lines.insert(start_idx, '% #AutoGen')
        lines.insert(start_idx+0, '% #functions')  # (auto-generated list python script)
        for i, func in enumerate(self.func_list):
            lines.insert(start_idx+i+1, func)

        lines.insert(start_idx + 1 + len(self.func_list), '% #/functions')
        lines.insert(start_idx + 2 + len(self.func_list), '%')

        self.write_m_file(fname, lines)

        log('update_m_file done: ')

    # -----------------------------------------------------------------------
    def write_func_list_file(self, fname, header_lines, func_dict):

        # Sort the function list
        func_list = func_dict.keys()
        func_list.sort()

        # Write function list file
        with open(fname, 'wt') as f:
            for line in header_lines:
                f.write(line + '\n')

            for func_name in func_list:
                func = func_dict[func_name]
                f.write(func.name + '\n')

    # -----------------------------------------------------------------------
    def update_files(self, fname):

        # Create output file
        path, fn = os.path.split(fname)

        # Sort the function list
        self.func_list.sort()
        self.pkg_func_list.sort()

        out_func_list = self.func_list

        if self.is_class_folder:
            out_fname = os.path.join(self.out_path, self.cur_package_class + '.txt')
        else:
            pre, ext = os.path.splitext(fn)
            out_fname = os.path.join(self.out_path, pre + '.txt')

        write_func_list_file

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
    #
    def get_code_line(self, line):
        line = line.replace('/t', '    ').strip()
        if line.startswith('%'):
            line = ''

        # Add spaces for easy tokens split
        line = line.replace('=', ' = ')
        line = line.replace('(', ' ( ')
        line = line.replace(')', ' ) ')

        return line

    # -----------------------------------------------------------------------
    # Process single .m file
    def find_classdef(self, lines):
        class_name = ''
        for line_num, line in enumerate(lines):
            try:
                code_line = self.get_code_line(line)
                tokens = code_line.split(' ')
                if tokens[0] == 'classdef':
                    class_name = tokens[1]
                    log_line('found classdef', line_num, line)
            except:
                log('exception parsing line: ' + line)

        return class_name

    # -----------------------------------------------------------------------
    # Get function name
    # function Result = funcWithRet(Obj, FileName)
    # function funcWithoutRet()
    # Result = classFuncInOtherFile
    def get_function_name(self, code_line):
        func_name = ''
        tokens = code_line.split(' ')

        # Found function keyword
        if len(tokens) > 0 and tokens[0] == 'function':

            # Function with return value(s)
            if '=' in tokens:
                func_name = code_line.split('=')[1].strip().split('(')[0].strip()

            # Function witohut return values
            else:
                func_name = code_line.split('function')[1].strip().split('(')[0].strip()

        return func_name
    # -----------------------------------------------------------------------
    # Process single .m file
    def process_file(self, fname):

        # Debug only
        if 'LogLevel' in fname:
            log('LogLevel')

        fname = fname.replace('\\', '/')
        self.cur_fname = fname
        self.cur_folder, self.cur_file = os.path.split(fname)
        #fn, ext = os.path.splitext(self.cur_file)

        # Check if we are inside a package
        pkg_name = self.get_package_from_path(fname)
        if pkg_name == '':
            pkg_name = '_'
        pkg = self.get_package(pkg_name, self.cur_folder)

        # Check if we are inside class folder
        class_name = self.get_class_from_path(self.cur_folder)

        if class_name != '':
            self.is_class_folder = True
            # Set main class file name
            self.class_fname = os.path.join(self.cur_folder, class_name + '.m')
        else:
            self.is_class_folder = False

        is_class_file = self.is_class_folder

        # Read source file, add empty lines at beginning and end to allow +/-1 indexing without exceptions
        lines = self.read_file(fname)
        lines.insert(0, '')
        lines.append('')

        # Look for classdef
        class_name = self.find_classdef(lines)
        if class_name != '':
            self.cur_class = class_name
            self.process_class_file(lines)
        else:
            if self.is_class_folder:
                self.process_class_func_file(lines)
            else:
                self.process_func_file(lines)

    # -----------------------------------------------------------------------

    # Process single .m file
    def process_class_file(self, lines):

        cls = self.get_class(self.cur_class)

        methods_type = ''
        for line_num, line in enumerate(lines):
            try:
                code_line = self.get_code_line(line)
                tokens = code_line.split(' ')
                if len(tokens) == 0:
                    continue

                # methods
                if tokens[0] == 'methods':
                    new_methods_type = ''
                    if 'Static' in tokens:
                        new_methods_type = 'Static'

                    # swtiched type
                    if new_methods_type != methods_type:
                        methods_type = new_methods_type
                        #outf.write('\n% methods ' + methods_type + '\n%\n')


                # Get function name
                else:
                    func_name = self.get_function_name(code_line)
                    if func_name != '':
                        #log_line('found function', line_num, line)

                    if func_name in cls.func_dict:
                        log_line('Duplicate function definition:', line_num, line)
                        continue

                    func = FunctionData()
                    func.name = func_name
                    func.comment = self.get_comment(lines, line_num)
                    cls.func_dict[func_name] = func
            except:
                log('exception parsing line: ' + line)

        # Update output only if not class folder
        if not self.is_class_folder and self.cur_class != '':
            self.update_files(fname)

    # -----------------------------------------------------------------------
    # Process class function file (file in class folder which is not the main class file)
    def process_class_func_file(self, lines):

        # Get class data
        cls = self.get_class(self.cur_class)

        #
        for line_num, line in enumerate(lines):
            try:
                code_line = self.get_code_line(line)
                func_name = self.get_function_name(code_line)
                if func_name != '':
                    # log_line('found function', line_num, line)
                    if func_name in cls.func_dict:
                        log_line('Already defined:', line_num, line)
                    else:
                        func = FunctionData()
                        func.name = func_name
                        func.comment = self.get_comment(lines, line_num)
                        cls.func_dict[func_name] = func

            except:
                log_line('process_func_file exception', line_num, line)

    # -----------------------------------------------------------------------
    # Process function file
    def process_func_file(self, lines):

        # Get package data
        pkg = self.get_package(self.cur_package)

        for line_num, line in enumerate(lines):
            try:
                code_line = self.get_code_line(line)
                func_name = self.get_function_name(code_line)
                if func_name != '':
                    # log_line('found function', line_num, line)
                    if func_name in pkg.func_dict:
                        log_line('Duplicate function definition:', line_num, line)
                    else:
                        func = FunctionData()
                        func.name = func_name
                        func.comment = self.get_comment(lines, line_num)
                        pkg.func_dict[func_name] = func

            except:
                log_line('process_func_file exception', line_num, line)

    # -----------------------------------------------------------------------
    # Process folder files (without recursion)
    def process_folder(self, path):
        if not self.should_process_folder((path)):
            return

        # Get files list without recursion
        path = path.replace('\\', '/')
        flist = glob.glob(os.path.join(path, '*.*'), recursive=False)
        log('process_folder: {}, files found: {}'.format(path, len(flist)))

        # Prepare list of files to process
        files_to_process = []
        for fname in flist:
            if self.should_process_file((fname)):
                files_to_process.append(fname)

        # Found files
        if len(files_to_process) == 0:
            return

        # Check if this is a class folder
        self.cur_folder = path
        class_name = self.get_class_from_path(path)
        class_fname = ''
        self.is_class_folder = class_name != ''
        if self.is_class_folder:

            # Set main class file name
            class_fname = os.path.join(self.cur_folder, class_name + '.m').replace('\\', '/')

            # Move class_fname to top of list
            if os.path.exists(class_fname):
                for idx, fname in enumerate(files_to_process):
                    _, fn = os.path.split(fname)
                    if fn.lower() == class_name.lower() + '.m':
                        files_to_process.pop(idx)
                        files_to_process.insert(0, fname)
                        break
            else:
                log('Missing class file: '  + class_fname)


        # Process files
        for fname in files_to_process:
            try:
                log('Processing file: ' + fname)
                self.process_file(fname)
            except:
                log('Exception processing file: ' + fname)


        # Update output only if class folder
        if self.is_class_folder:

            self.update_files()

    # -----------------------------------------------------------------------
    def should_process_folder(self, path):
        path = path.replace('\\', '/')
        process = True

        # Skip files in doc/autogen folder
        if 'doc/autogen' in path.lower():
            process = False

        # Skip unused/obsolete/temp files
        skip = ['unused', 'obsolete', 'old', 'temp', 'bkp', 'backup']
        tokens = path.lower().split('/')
        for tok in tokens:
            tok = tok.replace('_', '')
            if tok in skip:
                process = False
                break

        return process

    # -----------------------------------------------------------------------
    #
    def should_process_file(self, fname):
        process = False
        if self.should_process_folder(os.path.split(fname)[0]):
            fnlower = fname.lower()
            if fnlower.endswith('.m') and not fnlower.endswith('$out.m') and not fnlower.endswith('bkp.m'):
                process = True

        return process
    # -----------------------------------------------------------------------
    # Process folder with recursion
    def process_tree(self, path):

        # Get list of folders under path
        path = path.replace('\\', '/')
        folders = glob.glob(os.path.join(path, '**/'), recursive=True)
        log('process_tree: {}, folders found: {}'.format(path, len(folders)))

        for folder in folders:
            if self.should_process_folder(path):
                self.process_folder(folder)

    # -----------------------------------------------------------------------
    def process(self, path):

        self.package_list_filename = os.path.join(AUTOGEN_PATH, 'package_list.txt')
        self.class_list_filename = os.path.join(AUTOGEN_PATH, 'class_list.txt')

        # Load current package and class list
        package_list = self.read_file(self.package_list_filename)
        for pkg in package_list:
            self.get_package(pkg)
        for cls in package_list:
            self.get_class(cls)

        # Process folders tree
        self.process_tree(path)
        # process_file('c:/temp/DbQuery.m')

        # Update package list files
        package_list = self.package_dict.keys()
        package_list.sort()
        self.write_file(self.package_list_filename, package_list)

        # Update class list files
        class_list = self.class_dict.keys()
        class_list.sort()
        self.write_file(self.class_list_filename, class_list)

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
    #proc.process('D:/Ultrasat/AstroPack.git/matlab/')
    #proc.process('D:/Ultrasat/AstroPack.git/matlab/base')

    #proc.process('D:\\Ultrasat\\AstroPack.git\\matlab\\util\\+tools\\+interp')
    proc.process('D:/Ultrasat/AstroPack.git/matlab/base/@Base')
    #proc.process('D:/Ultrasat/AstroPack.git/matlab/util/+db')



if __name__ == '__main__':
    main()
