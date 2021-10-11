# Automatic tool to extract functions from matlab source files
# with their H1 comments.

# Todo:
#    Handle correctly static class functions that are defined in separate file
#    Handle non-class functions
#    Generate unitTest() skeleton with functions list in comments (by function order in file)
#    Generate mlx skeleton (if possible), check if we can generate HTML and import it, or just text?
#    MLX is Open Packaging Conventions, there are Python packages to manipulate it
#    Open Packaging Conventions - https://en.wikipedia.org/wiki/Open_Packaging_Conventions
#    Office Open XML - https://en.wikipedia.org/wiki/Office_Open_XML
#
# Generate HTML - see Eran's page:
#
#   https://webhome.weizmann.ac.il/home/eofek/matlab/FunList.html
#
# Markdown -> HTML:
#   https://www.digitalocean.com/community/tutorials/how-to-use-python-markdown-to-convert-markdown-text-to-html
#   https://www.kite.com/python/examples/2545/markdown-convert-markdown-text-to-html
#   https://pypi.org/project/md-to-html/
#
#
# Outputs:
# For each .m file - txt file with function list
#

import os, glob, argparse, shutil, zipfile
from datetime import datetime

ASTROPACK_PATH = os.getenv('ASTROPACK_PATH')
AUTOGEN_PATH = os.path.join(ASTROPACK_PATH, 'matlab/doc/autogen')
UPDATE_M = True # False #True
UPDATE_M_OUT_FILE = False           # True to write updated output to '$out' file instead of modifying the original file
TRIM_TRAILING_SPACES = True

#
FUNC_BLOCK_BEGIN = '% #functions (autogen)'
FUNC_BLOCK_END = '% #/functions (autogen)'


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
# Data for each package
class PackageData:

    def __init__(self):
        self.name = ''
        self.path = ''
        self.class_dict = {}        # Currently unused
        self.func_dict = {}
        self.comment = ''

# ===========================================================================
# Data for each class
class ClassData:

    def __init__(self):
        self.name = ''
        self.path = ''
        self.filename = ''
        self.func_dict = {}
        self.prop_dict = {}         # Currently unused
        self.comment = ''
        self.long_comment = ''
        self.unitTest_lines = []

# ===========================================================================
# Data for each function (in package or class)
class FunctionData:

    def __init__(self):
        self.name = ''
        self.filename = ''
        self.type = ''              # Static
        self.params = ''
        self.comment = ''
        self.long_comment = ''

# ===========================================================================
# Data for each class property (currently unused)
class PropData:

    def __init__(self):
        self.name = ''
        self.type = ''              # Datatype
        self.comment = ''

# ===========================================================================
# Data for each class property (currently unused)
class BookmarkData:

    def __init__(self):
        self.name = ''
        self.type = ''              # @Todo, @TBD, @Future
        self.comment = ''
        self.filename = ''
        self.line_num = 0

# ===========================================================================
#                           MATLAB Source Code Processor
# ===========================================================================

class MatlabProcessor:

    def __init__(self):
        self.out_path = os.path.join(AUTOGEN_PATH, 'all_classes')
        self.cur_fname = ''
        self.cur_folder = ''
        self.cur_package = ''
        self.cur_class = ''
        self.package_dict = {}
        self.class_dict = {}
        self.is_class_folder = False
        self.is_class_file = False
        self.package_list_filename = ''
        self.class_list_filename = ''
        self.todo_list = []
        #self.mlx_lines = []


    # -----------------------------------------------------------------------
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
    # Get package from dict, add if not exist
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
    # Get class from dict, add if not exist
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

    # -----------------------------------------------------------------------
    # Get class name from path
    def get_class_from_path(self, path):
        if path[-1] == '/':
            path = path[0:-1]
        last = os.path.split(path)[1]
        if last.startswith('@'):
            return last[1:]
        else:
            return ''

    # -----------------------------------------------------------------------
    # Extract H1 comment from comment lines below the function/class line
    # function Result = openConn(Obj)
    #    % Open connection, throw exception on failure
    def get_comment(self, lines, idx, short = True):
        comment = ''

        # Look for comment line below the function line
        count = 1
        while idx + count < len(lines):
            line = lines[idx + count].strip().replace('\t', ' ')

            if short:
                # Stop on non-comment line or empty line
                if not lines[idx+count].strip().startswith('%'):
                    break
            else:
                # Stop on non-comment line (assuming it is a code)
                if not lines[idx+count].strip().startswith('%') and line.strip() != '':
                    break

            line = line.replace('%', '').strip()
            line = line.replace('--', '').strip()
            line = line.replace('==', '').strip()
            words = line.split(' ')
            words_lower = line.lower().split(' ')

            # Stop on special cases
            if short:
                if 'example' in words_lower:
                    break
                if len(words) > 0 and words[0] == 'Input':
                    break

                # Append text to comment
                comment_line = ' '.join(words)
                comment = (comment + ' ' + comment_line).strip()
            else:
                # Append text to comment, use double-space for new line in MD file
                comment_line = line + '  ' #' '.join(words)
                comment = (comment + '\n' + comment_line) #.strip()


            # Stop if comment is too long, by number of lines, or by text length
            count = count + 1
            if short and (count >= 5 or len(comment) > 300):
                break

        return comment

    # -----------------------------------------------------------------------
    # Look for line that starts with specified text
    def index_starts_with(self, lines, text):
        for i, line in enumerate(lines):
            if line.startswith(text):
                return i

        return -1
    # -----------------------------------------------------------------------
    # Remove current info block and get index to line to insert
    def new_info_block(self, lines):
        start_idx = self.index_starts_with(lines, FUNC_BLOCK_BEGIN)
        end_idx   = self.index_starts_with(lines, FUNC_BLOCK_END)

        # Found both strings, cut out the block
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
    # Write source code files
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
    # Update source code file with info block
    def update_class_m_file(self):

        fname = self.cur_fname
        func_list_lines = []

        if not self.cur_class in self.class_dict:
            return

        cls = self.class_dict[self.cur_class]
        func_list = list(cls.func_dict.keys())
        func_list.sort()
        for func_name in func_list:
            func = cls.func_dict[func_name]
            line = func.name + ' - ' + func.comment
            func_list_lines.append(line)

        # Read source file
        lines = self.read_file(fname)
        lines, start_idx = self.new_info_block(lines)

        # Insert functions list at to of file
        lines.insert(start_idx+0, FUNC_BLOCK_BEGIN)  # (auto-generated list python script)
        for i, func in enumerate(func_list_lines):
            lines.insert(start_idx+i+1, func)

        lines.insert(start_idx + 1 + len(func_list_lines), FUNC_BLOCK_END)
        lines.insert(start_idx + 2 + len(func_list_lines), '%')

        self.write_m_file(fname, lines)

        log('update_m_file done: ')

    # -----------------------------------------------------------------------
    # @todo UNUSED
    def write_func_list_file(self, fname, header_lines, func_dict):

        # Sort the function list
        func_list = list(func_dict.keys())
        func_list.sort()

        # Write function list file
        with open(fname, 'wt') as f:
            for line in header_lines:
                f.write(line + '\n')

            for func_name in func_list:
                func = func_dict[func_name]
                f.write(func.name + '\n')

    # -----------------------------------------------------------------------
    # @todo - Currently UNUSED
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

        #write_func_list_file

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
    # @todo
    def write_html(self):
        log('write_html')

    # -----------------------------------------------------------------------
    def update_mlx_document(self, filename):
        log('update_mlx_document: ' + filename)
        lines = self.read_file(filename)
        lines.insert('This is my test!')
        self.write_file(filename, lines)
        return True

    # -----------------------------------------------------------------------
    # MLX files are ZIP files with structured format
    # Open Packaging Conventions - https://en.wikipedia.org/wiki/Open_Packaging_Conventions
    # Office Open XML - https://en.wikipedia.org/wiki/Office_Open_XML
    # Inside the ZIP file, the document is stored in matlab/document.xml file
    # @todo
    def write_mlx(self, mlx_filename):
        log('write_mlx: ' + mlx_filename)

        # Copy template
        if not os.path.exists(mlx_filename):
            mlx_template_filename = os.path.join(ASTROPACK_PATH, '/matlab/help/+manuals/_ClassTemplate.mlx')
            log('copying mlx template: {} to {}'.format(mlx_template_filename, mlx_filename))
            shutil.copy(mlx_template_filename, mlx_filename)

        # Extract mlx
        mlx_temp_folder = 'c:/_mlx/temp' #temp/mlx'
        try:
            shutil.unpack_archive(mlx_filename, mlx_temp_folder)

            #with zipfile.ZipFile(mlx_filename, 'r') as zip_ref:
            #    zip_ref.extractall(mlx_temp_folder)
        except:
            log('error extracting mlx file: ' + mlx_filename)
            return

        # Read document.xml
        doc_filename = os.path.join(mlx_temp_folder, '/matlab/document.xml')

        # Update document.xml
        try:
            updated = self.update_mlx_document(doc_filename)
            if not updated:
                log('not updated')
                return
        except:
            log('not updated')
            return


        # Create new zip file
        mlx_temp_filename = os.path.join('')
        os.remove(mlx_temp_filename)
        try:
            shutil.make_archive(mlx_temp_filename, 'zip', mlx_temp_folder)
        except:
            return

        # Copy new zip file
        try:
            log('copying mlx template: {} to {}'.format(mlx_template_filename, mlx_filename))
            shutil.copy(mlx_template_filename, mlx_filename)
        except:
            log('error copying file: {} to {}'.format(mlx_temp_filename, mlx_filename))
            return



    # -----------------------------------------------------------------------
    # @todo
    def write_unitTest(self):
        log('write_unitTest')

    # -----------------------------------------------------------------------
    # Clean source code line and prepare for token splitting
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
    # Find 'classdef', return class name
    def find_classdef(self, lines):
        class_name = ''
        for line_num, line in enumerate(lines):
            try:
                code_line = self.get_code_line(line)
                tokens = code_line.split(' ')
                if tokens[0] == 'classdef':
                    class_name = tokens[1]
                    log_line('found classdef', line_num, line)
                    break
            except:
                log('exception parsing line: ' + line)

        return class_name

    # -----------------------------------------------------------------------
    # Get function name from line
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

            # Function without return values
            else:
                func_name = code_line.split('function')[1].strip().split('(')[0].strip()

        return func_name

    # -----------------------------------------------------------------------
    # Remove non-package prefix from names
    def unpack_name(self, name):
        if name.startswith('#.'):
            name = name[2:]
        return name

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
            pkg_name = '#'
        self.cur_package = pkg_name
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
            class_name = pkg_name + '.' + class_name
            self.cur_class = class_name
            self.process_class_file(lines)
        else:
            if self.is_class_folder:
                self.process_class_func_file(lines)
            else:
                self.process_func_file(lines)

    # -----------------------------------------------------------------------
    # Process main class file (the file with 'classdef')
    def process_class_file(self, lines):

        cls = self.get_class(self.cur_class)
        cls.long_comment = self.get_comment(lines, 0, False)

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
                        func.long_comment = self.get_comment(lines, line_num, short=False)
                        cls.func_dict[func_name] = func
            except:
                log('exception parsing line: ' + line)

        # Update the class source file with list of functions and other collected data
        if UPDATE_M:
            self.update_class_m_file()

        # Update output only if not class folder
        if not self.is_class_folder and self.cur_class != '':
            pass
            #self.update_files(fname)

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
                        func.long_comment = self.get_comment(lines, line_num, short=False)
                        cls.func_dict[func_name] = func

                        # Stop after the first function, so internal (unexposed) functions will not be listed
                        break

            except:
                log_line('process_func_file exception', line_num, line)

    # -----------------------------------------------------------------------
    # Process function file (non-class)
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
                        func.long_comment = self.get_comment(lines, line_num, short=False)
                        pkg.func_dict[func_name] = func

                        # Stop after the first function, so internal (unexposed) functions will not be listed
                        break

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
            fname = fname.replace('\\', '/')
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
            pass
            #self.update_files()

    # -----------------------------------------------------------------------
    # Check if should process folder
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
    # Check if should process file
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
    def append_indent(self, lines, new_lines):
        for line in new_lines:
            lines.append('    ' + line)

    # -----------------------------------------------------------------------
    # Process all collected data and update output files
    def process_data(self):

        # Generate .txt and .md files
        out_path_txt = os.path.join(AUTOGEN_PATH, 'package_functions')
        out_path_md = os.path.join(AUTOGEN_PATH, 'package_functions_md')
        package_list = list(self.package_dict.keys())
        package_list.sort()
        self.write_file(self.package_list_filename, package_list)
        for pkg_name in package_list:
            pkg = self.get_package(pkg_name)
            func_list = list(pkg.func_dict.keys())
            func_list.sort()
            if len(func_list) == 0:
                continue

            lines = []
            lines.append('Package: ' + self.unpack_name(pkg_name))
            lines.append('')
            md_lines = []
            md_lines.append('# Package: ' + self.unpack_name(pkg_name))
            md_lines.append('\n')
            for func_name in func_list:
                func = pkg.func_dict[func_name]
                line = func.name + ' - ' + func.comment
                lines.append(line + '\n')
                md_lines.append('### ' + self.unpack_name(pkg_name + '.' + func.name) + '\n')
                md_lines.append(func.comment + '\n\n')
                self.append_indent(md_lines, func.long_comment.split('\n'))

            pkg_fname_txt = os.path.join(out_path_txt, pkg_name + '.txt')
            self.write_file(pkg_fname_txt, lines)
            pkg_fname_md = os.path.join(out_path_md, pkg_name + '.md')
            self.write_file(pkg_fname_md, md_lines)

        # Update class list files
        out_path_txt = os.path.join(AUTOGEN_PATH, 'class_functions')
        out_path_md = os.path.join(AUTOGEN_PATH, 'class_functions_md')
        class_list = list(self.class_dict.keys())
        class_list.sort()
        lines = class_list.copy()
        for i, line in enumerate(lines):
            lines[i] = self.unpack_name(line)
        self.write_file(self.class_list_filename, lines)
        for cls_name in class_list:

            #if 'AstroHeader' in cls_name:
            #    log('break')

            cls = self.get_class(cls_name)
            func_list = list(cls.func_dict.keys())
            func_list.sort()
            if len(func_list) == 0:
                continue

            lines = []
            lines.append('Class: ' + self.unpack_name(cls_name))
            lines.append('')
            md_lines = []
            md_lines.append('# Class: ' + self.unpack_name(cls_name))
            md_lines.append('')
            md_lines.append(cls.comment + '\n')
            self.append_indent(md_lines, cls.long_comment.split('\n'))
            for func_name in func_list:
                func = cls.func_dict[func_name]
                line = func.name + ' - ' + func.comment
                lines.append(line + '\n')
                md_lines.append('### ' + func.name + '\n')
                md_lines.append(func.comment + '\n\n')
                self.append_indent(md_lines, func.long_comment.split('\n'))

            cls_fname_txt = os.path.join(out_path_txt, cls_name + '.txt')
            self.write_file(cls_fname_txt, lines)
            cls_fname_md = os.path.join(out_path_md, cls_name + '.md')
            self.write_file(cls_fname_md, md_lines)

            # Update class files
            if UPDATE_M:
                pass

    # -----------------------------------------------------------------------
    # Main function
    def process(self, path):

        self.package_list_filename = os.path.join(AUTOGEN_PATH, 'package_list.txt')
        self.class_list_filename = os.path.join(AUTOGEN_PATH, 'class_list.txt')

        # Load current package and class list
        package_list = self.read_file(self.package_list_filename)
        for pkg in package_list:
            self.get_package(pkg)

        class_list = self.read_file(self.class_list_filename)
        for cls in class_list:
            if not '.' in cls:
                cls = '#.' + cls
            self.get_class(cls)

        # Process folders tree
        self.process_tree(path)
        self.process_data()


# ---------------------------------------------------------------------------
def main():

    # Read command line options
    parser = argparse.ArgumentParser()

    # Arguments
    parser.add_argument('-d', dest='dir',         default=None,                           help='pcap folder')
    parser.add_argument('-s', dest='subdirs',     action='store_true',    default=True,   help='Process pcap files in subfolders')
    parser.add_argument('-o', dest='outdir',      default=None,                           help='Output folder')
    args = parser.parse_args()

    #
    proc = MatlabProcessor()

    mlx_filename = 'c:/temp/_mlx/1.mlx'
    proc.write_mlx(mlx_filename)
    return


    proc.process('c:/_m1')

    #proc.process('D:/Ultrasat/AstroPack.git/matlab') #/image/@AstroHeader/')


    #proc.process('D:/Ultrasat/AstroPack.git/matlab/base')

    #proc.process('D:\\Ultrasat\\AstroPack.git\\matlab\\util\\+tools\\+interp')
    #proc.process('D:/Ultrasat/AstroPack.git/matlab/base/@Base')
    #proc.process('D:/Ultrasat/AstroPack.git/matlab/util/+db')



if __name__ == '__main__':
    main()
