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
from random import randint

# --- Global flags ---
UPDATE_M = True # False #True
UPDATE_M_OUT_FILE = False           # True to write updated output to '$out' file instead of modifying the original file
BACKUP_M_FILE = False               # True to copy original files to $bkp.m
TRIM_TRAILING_SPACES = True         # True to clear trailing spaces from all processd files


# Get path to repository root folder
ASTROPACK_PATH = os.getenv('ASTROPACK_PATH')

# Prepare path to autogen documentation
AUTOGEN_PATH = os.path.join(ASTROPACK_PATH, 'matlab/doc/autogen')
MLX_ELEMENTS_PATH = os.path.join(ASTROPACK_PATH, 'matlab/doc/mlx/mlx_elements')

# Markers
FUNC_BLOCK_BEGIN = '% #functions (autogen)'
FUNC_BLOCK_END = '% #/functions (autogen)'

# ===========================================================================

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
#
class MlxBookmark:
    def __init__(self, text, type, tag):
        self.text = text
        self.type = type
        self.tag = tag


class MlxWriter:

    def __init__(self, fname = ''): #, template_fname = ''):

        #
        self.doc_text = ''
        self.fname = fname
        self.temp_path = 'c:/_mlx/temp'
        self.template_fname = os.path.join(MLX_ELEMENTS_PATH, 'empty.mlx')
        self.align = 'left'
        self.with_toc = True
        self.toc_list = []

        # Load elements
        self.xml_start = self.load('start')
        self.xml_end = self.load('end')
        self.xml_title = self.load('title')
        self.xml_heading1 = self.load('heading1')
        self.xml_heading2 = self.load('heading2')
        self.xml_heading3 = self.load('heading3')
        self.xml_text = self.load('text')
        self.xml_code = self.load('code')
        self.xml_paragraph_start = self.load('paragraph_start')
        self.xml_paragraph_end = self.load('paragraph_end')
        self.xml_normal = self.load('normal')
        self.xml_bold = self.load('bold')
        self.xml_italic = self.load('italic')
        self.xml_numbered = self.load('numbered')
        self.xml_bullet = self.load('bullet')
        self.xml_toc = self.load('toc')

        #
        self.create()


    def __enter__(self):
        return self


    def __exit__(self, exc_type, exc_value, traceback):
        self.close()

    # -----------------------------------------------------------------------
    #
    def create(self):
        self.doc_text = ''
        self.wr(self.xml_start)


    # Close file, update XML or MLX file on disk
    def close(self):
        self.text('')
        self.wr(self.xml_end)
        if self.fname != '':
            fname = self.fname.lower()
            if fname.endswith('.xml'):
                with open(fname, 'wt') as f:
                    f.write(self.doc_text)
            elif fname.endswith('.mlx') or fname.endswith('.zip'):
                self.write_mlx(self.fname)


    # -----------------------------------------------------------------------
    # Title
    def title(self, text, body=''):
        self.wr(self.xml_title, text)
        if body != '':
            self.text(body)

        if self.with_toc:
            self.toc()


    # Heading 1
    def heading1(self, text, body=''):
        self.heading(self.xml_heading1, text, body, 'heading1')

    # Heading 2
    def heading2(self, text, body=''):
        self.heading(self.xml_heading2, text, body, 'heading2')

    # Heading 3
    def heading3(self, text, body=''):
        self.heading(self.xml_heading3, text, body, 'heading3')

    #
    def heading(self, template, text, body='', bookmark_type = ''):
        if self.with_toc:
            tag, bm_start, bm_end = self.get_bookmark()
            template = template.replace('$BmStart', bm_start)
            template = template.replace('$BmEnd', bm_end)
            bookmark = MlxBookmark(text, bookmark_type, tag)
            self.toc_list.append(bookmark)
        else:
            template = template.replace('$BmStart', '')
            template = template.replace('$BmEnd', '')

        self.wr(template, text)
        if body != '':
            self.text(body)


    # Single/mutli-line paragraph
    def text(self, text):
        lines = text.split('\n')
        for line in lines:
            self.wr(self.xml_text, line)

    # -----------------------------------------------------------------------
    #
    def start_par(self):
        self.wr(self.xml_paragraph_start)

    #
    def end_par(self):
        self.wr(self.xml_paragraph_end)

    #
    def normal(self, text):
        self.wr(self.xml_normal, text)

    #
    def bold(self, text):
        self.wr(self.xml_bold, text)

    #
    def italic(self, text):
        self.wr(self.xml_italic, text)

    # -----------------------------------------------------------------------

    #
    def code(self, text):
        self.wr(self.xml_code, text)

    #
    def bullet(self, text=''):
        self.wr(self.xml_bullet)
        if text != '':
            self.normal(text)
            self.end_par()

    #
    def numbered(self, text=''):
        self.wr(self.xml_numbered)
        if text != '':
            self.normal(text)
            self.end_par()

    # Add table-of-contents, should be added at top
    def toc(self):
        self.wr(self.xml_toc)

    # '<w:bookmarkStart w:name="MW_H_DFC5E6A7" w:id="H_DFC5E6A7"/>'
    def get_bookmark(self):
        tag = hex(randint(0, 65536 ** 2)).upper()[2:]
        start = '<w:bookmarkStart w:name="MW_H_{}" w:id="H_{}"/>'.format(tag, tag)
        end = '<w:bookmarkEnd w:id="H_{}"/>'.format(tag)
        return tag, start, end

    # -----------------------------------------------------------------------
    #
    def wr(self, template, text=''):
        # Replace alignment in <w:jc w:val="left"/>
        if self.align != 'left' and '<w:jc w:val=' in template:
            template = template.replace('"left"', '"' + self.align + '"')

        text = text.replace('&', '&amp;')
        s = template.replace('$Text', text)
        self.doc_text = self.doc_text + s


    # Load element from XML file, ignore comments starting with # or ;
    def load(self, fname):
        text = ''
        fname = os.path.join(MLX_ELEMENTS_PATH, fname + '.xml')
        if os.path.exists(fname):
            with open(fname) as f:
                lines = f.read().splitlines()
                for line in lines:
                    if line.startswith('#') or line.startswith(';'):
                        continue
                    text = text + line.strip()
        else:
            log('MlxWriter.load: File not found: ' + fname)

        return text

    # -----------------------------------------------------------------------
    def write_toc(self):

        # Save text
        save_doc_text = self.doc_text
        self.doc_text = ''

        # Generate TOC
        for item in self.toc_list:
            pass


        # Get the generated TOC
        toc_text = self.doc_text
        self.doc_text = save_doc_text.replace('$TOC', toc_text)

    # -----------------------------------------------------------------------
    # MLX files are ZIP files with structured format
    # Open Packaging Conventions - https://en.wikipedia.org/wiki/Open_Packaging_Conventions
    # Office Open XML - https://en.wikipedia.org/wiki/Office_Open_XML
    # Inside the ZIP file, the document is stored in matlab/document.xml file
    # See matlab.internal.liveeditor.openAndConvert, matlab.internal.liveeditor.openAndSave
    def write_mlx(self, mlx_fname):

        #
        if self.with_toc:
            self.write_toc()

        #
        template_fname = self.template_fname

        _remove = True
        if _remove and os.path.exists(mlx_fname):
            os.remove(mlx_fname)

        # Copy template if file does not exist yet
        if template_fname != '' and not os.path.exists(mlx_fname):
            log('copying mlx template: {} to {}'.format(template_fname, mlx_fname))
            shutil.copyfile(template_fname, mlx_fname)

        # Extract mlx as zip file
        fname = os.path.split(mlx_fname)[1]
        mlx_temp_folder = os.path.join(self.temp_path, fname)
        try:
            shutil.unpack_archive(mlx_fname, mlx_temp_folder, 'zip')
        except:
            log('error extracting mlx file: ' + mlx_fname)
            return

        # @Todo? Read existing xml file and process it????

        # Write document.xml
        doc_fname = os.path.join(mlx_temp_folder, 'matlab/document.xml')
        try:
            with open(doc_fname, 'wt') as f:
                f.write(self.doc_text)
        except:
            log('not updated')
            return

        # Create new zip file
        mlx_temp_fname = os.path.join(self.temp_path, fname + '_new')
        if os.path.exists(mlx_temp_fname):
            os.remove(mlx_temp_fname)

        try:
            shutil.make_archive(mlx_temp_fname, 'zip', mlx_temp_folder)
        except:
            return

        # Copy new zip file
        mlx_temp_filename = mlx_temp_fname + '.zip'
        try:
            log('copying mlx template: {} to {}'.format(mlx_temp_filename, mlx_fname))
            if os.path.exists(mlx_fname):
                os.remove(mlx_fname)
            shutil.copyfile(mlx_temp_filename, mlx_fname)
        except:
            log('error copying file: {} to {}'.format(mlx_temp_filename, mlx_fname))
            return

    # -----------------------------------------------------------------------
    # Unit-Test
    @staticmethod
    def unit_test():
        path = os.path.join(MLX_ELEMENTS_PATH, '../unit_test/')

        # Write bare XML file
        with MlxWriter(path + 'line1.xml') as m:
            m.text('This is my line in XML file')

        # Write MLX file
        with MlxWriter(path + 'line1.mlx') as m:
            m.text('This is my line in MLX file')

        # Test 1
        with MlxWriter(path + 'test1.mlx') as m:
            #m.toc()
            m.title('My Title', 'Text line 1\nText line 2\nText line 3\n')
            m.heading1('My Heading One', 'Text line 1\nText line 2\nText line 3\n')
            m.heading2('My Heading Two under one', 'Text line 1\nText line 2\nText line 3\n')
            m.heading3('My Heading Three under two', 'Text line 1\nText line 2\nText line 3\n')
            m.code('Sample code:\nLine 1\nLine 2\nLine3\Last line.')
            m.text('More text\nLine two')

            m.heading1('My Heading One', 'Text line 1\nText line 2\nText line 3\n')
            m.heading3('My Heading Three under one', 'Text line 1\nText line 2\nText line 3\n')
            m.code('More code:\nLine 1\nLine 2\nLine3\Last line.')

            m.heading3('Numbered list')
            m.numbered('numbered item 1')
            m.numbered('numbered item 2')
            m.numbered('numbered item 3')

            m.heading3('Bullet list')
            m.bullet('bullet item 1')
            m.bullet('bullet item 2')
            m.bullet('bullet item 3')

            m.start_par()
            for i in range(0,10):
                m.normal('This text is normal ')
                m.bold('then changes to Bold')
                m.normal(' back to normal ')
                m.italic('now in Italic')
                m.normal(' back to normal. ')
            m.end_par()

            m.text('THE END')


        # Test 2
        with MlxWriter(path + 'test2.mlx') as m:

            m.text('Bullet list of bold items')
            m.bullet()
            m.bold('Item One')
            m.normal(' is blabla blabla')
            m.end_par()

            m.bullet()
            m.bold('Item Two')
            m.normal(' is blabla blabla')
            m.end_par()

            m.text('THE END')


        # Test 3
        with MlxWriter(path + 'test3.mlx') as m:
            m.toc()
            m.title('My Title', 'Text line 1\nText line 2\nText line 3\n')
            m.heading1('My Heading One', 'Text line 1\nText line 2\nText line 3\n')
            m.heading2('My Heading Two under one', 'Text line 1\nText line 2\nText line 3\n')
            m.heading3('My Heading Three under two', 'Text line 1\nText line 2\nText line 3\n')
            m.code('Sample code:\nLine 1\nLine 2\nLine3\Last line.')
            m.text('More text\nLine two')

            m.text('THE END')


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
        self.is_constructor = False

# ===========================================================================
# Data for each class property (currently unused)
class PropertyData:

    def __init__(self):
        self.name = ''
        self.type = ''              # Datatype
        self.comment = ''
        self.long_comment = ''

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
    def write_file(self, fname, lines, line_sep = '\n'):
        with open(fname, 'wt') as f:
            for line in lines:
                f.write(line + line_sep)

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

        start_idx = self.index_starts_with(lines, FUNC_BLOCK_BEGIN)
        end_idx   = self.index_starts_with(lines, FUNC_BLOCK_END)

        # Look for comment line below the function line
        count = 0
        for line_num in range(idx+1, len(lines)):

            # Skip autogen #functions block
            if start_idx > -1 and end_idx > -1:
                if line_num >= start_idx and line_num <= end_idx:
                    continue

            line = lines[line_num].strip().replace('\t', ' ')
            if short:
                # Stop on non-comment line or empty line
                if not line.strip().startswith('%'):
                    break
            else:
                # Stop on non-comment line (assuming it is a code)
                if not line.strip().startswith('%') and line.strip() != '':
                    break

            # Clean comment marks and separators
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
    def remove_autogen_funclist(self, lines):
        start_idx = self.index_starts_with(lines, FUNC_BLOCK_BEGIN)
        end_idx   = self.index_starts_with(lines, FUNC_BLOCK_END)

        # Found both strings, cut out the block
        if start_idx > -1 and end_idx > -1:
            lines = lines[:start_idx] + lines[end_idx + 3:]
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
            if BACKUP_M_FILE:
                bkp_fname = fname + '$bkp.m'
                if os.path.exists(bkp_fname):
                    os.remove(bkp_fname)
                os.rename(fname, bkp_fname)

        log('write_m_file: ' + out_fname)
        with open(out_fname, 'wt') as f:
            for line in lines:

                # Trim trailing spaces only from non-empty lines
                if TRIM_TRAILING_SPACES:
                    if line.strip() != '':
                        line = line.rstrip()

                f.write(line + '\n')

        log('file updated: ' + out_fname)

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
        lines, start_idx = self.remove_autogen_funclist(lines)

        # Insert functions list at to of file
        lines.insert(start_idx+0, FUNC_BLOCK_BEGIN)  # (auto-generated list python script)
        for i, func in enumerate(func_list_lines):
            line = '% ' + func
            lines.insert(start_idx+i+1, line)

        lines.insert(start_idx + 1 + len(func_list_lines), FUNC_BLOCK_END)
        lines.insert(start_idx + 2 + len(func_list_lines), '%')
        lines.insert(start_idx + 3 + len(func_list_lines), '')

        self.write_m_file(fname, lines)

        log('update_class_m_file done: ')

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
    # @todo - Maybe it would be enough to export from MLX ?
    def write_html(self):
        log('write_html')

    # -----------------------------------------------------------------------
    # todo Unused
    def new_mlx_block(self, lines):
        start_idx = self.index_starts_with(lines, FUNC_BLOCK_BEGIN)
        end_idx   = self.index_starts_with(lines, FUNC_BLOCK_END)

        # Found both strings, cut out the block
        if start_idx > -1 and end_idx > -1:
            lines = lines[:start_idx] + lines[end_idx + 3:]
        else:
            start_idx = 0

            #
            for i, line in enumerate(lines):
                if not line.startswith('%'):
                    start_idx = i+1
                    break

        return lines, start_idx

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
    # Get property name from line
    # function Result = funcWithRet(Obj, FileName)
    # function funcWithoutRet()
    # Result = classFuncInOtherFile
    def get_property_name(self, code_line):
        prop_name = ''
        comment = code_line.split('%')
        if len(comment) > 1:
            comment = comment[1].strip()
        else:
            comment = ''

        code = code_line.split('%')[0].strip()
        tokens = code.replace('=', ' ').strip().split(' ')

        # Found function keyword
        if len(tokens) > 0:

            prop_name = tokens[0].strip()

            # Function with return value(s)
            #if '=' in tokens:
            #    func_name = code_line.split('=')[1].strip().split('(')[0].strip()

            # Function without return values
            #else:
            #    func_name = code_line.split('function')[1].strip().split('(')[0].strip()

        return prop_name, comment

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
        prop_type = ''
        in_properties = False

        for line_num, line in enumerate(lines):
            try:
                code_line = self.get_code_line(line)
                tokens = code_line.split(' ')

                # Ignore empty lines
                if len(tokens) == 0:
                    continue

                # properties
                if tokens[0] == 'properties':
                    in_properties = True
                    new_prop_type = ''
                    if 'Static' in tokens:
                        new_prop_type = 'Static'

                    # swtiched type
                    if new_prop_type != prop_type:
                        prop_type = new_prop_type
                        #outf.write('\n% properties ' + new_prop_type_type + '\n%\n')

                elif in_properties and tokens[0] == 'end':
                    in_properties = False

                # Get property name
                elif in_properties:
                    prop_name, comment = self.get_property_name(code_line)
                    if prop_name != '':
                        #log_line('found property', line_num, line)

                        if prop_name in cls.prop_dict:
                            log_line('Duplicate property definition:', line_num, line)
                            continue

                        prop = PropertyData()
                        prop.name = prop_name
                        prop.comment = comment #self.get_comment(lines, line_num)
                        #prop.long_comment = self.get_comment(lines, line_num, short=False)
                        cls.prop_dict[prop_name] = prop


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

                        if func_name == self.unpack_name(cls.name):
                            func.is_constructor = True

                        # Debug
                        if 'copyElement' in func_name:
                            log('debug')

                        func.comment = self.get_comment(lines, line_num)
                        func.long_comment = self.get_comment(lines, line_num, short=False)
                        cls.func_dict[func_name] = func
            except:
                log('exception parsing line: ' + line)

        # Update the class source file with list of functions and other collected data
        if UPDATE_M:
            self.update_class_m_file()

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


    # -----------------------------------------------------------------------
    # Check if should process folder
    def should_process_folder(self, path):
        path = path.replace('\\', '/')
        process = True

        # Skip files in doc/autogen folder
        if 'doc/autogen' in path.lower():
            process = False

        # Skip unused/obsolete/temp files
        skip = ['unused', 'obsolete', 'old', 'temp', 'bkp', 'backup', 'external', 'draft', 'testing']
        tokens = path.lower().split('/')
        for tok in tokens:
            tok = tok.replace('_', '')
            tok = tok.replace('+', '')
            if tok in skip or tok.startswith('draft'):
                process = False
                break

        return process

    # -----------------------------------------------------------------------
    # Check if should process file
    def should_process_file(self, fname):
        process = False
        if self.should_process_folder(os.path.split(fname)[0]):
            fnlower = fname.lower()
            if fnlower.endswith('.m') and not fnlower.endswith('$out.m') \
                and not fnlower.endswith('bkp.m') and not fnlower.startswith('draft_'):
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
    def generate_packages_txt_md(self):

        # Prepare folder names
        out_path_txt = os.path.join(AUTOGEN_PATH, 'package_functions')
        out_path_md = os.path.join(AUTOGEN_PATH, 'package_functions_md')

        # List of all packages
        package_list = list(self.package_dict.keys())
        package_list.sort()
        self.write_file(self.package_list_filename, package_list)

        for pkg_name in package_list:
            pkg = self.get_package(pkg_name)
            func_list = list(pkg.func_dict.keys())
            func_list.sort()
            if len(func_list) == 0:
                continue

            pkg_fname_txt = os.path.join(out_path_txt, pkg_name + '.txt')
            pkg_fname_md = os.path.join(out_path_md, pkg_name + '.md')

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

                # MD
                md_lines.append('### ' + self.unpack_name(pkg_name + '.' + func.name) + '\n')
                md_lines.append(func.comment + '\n\n')
                self.append_indent(md_lines, func.long_comment.split('\n'))

            # Write txt and md files to disk
            self.write_file(pkg_fname_txt, lines)
            self.write_file(pkg_fname_md, md_lines)


    def generate_packages_mlx(self):

        # Prepare folder names
        out_path_mlx = os.path.join(AUTOGEN_PATH, 'package_functions_mlx')

        # Packages
        package_list = list(self.package_dict.keys())
        package_list.sort()
        #self.write_file(self.package_list_filename, package_list)

        '''
        for pkg_name in package_list:
            pkg = self.get_package(pkg_name)
            func_list = list(pkg.func_dict.keys())
            func_list.sort()
            if len(func_list) == 0:
                continue

            pkg_fname_mlx = os.path.join(out_path_mlx, pkg_name + '.mlx')

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

                # MD
                md_lines.append('### ' + self.unpack_name(pkg_name + '.' + func.name) + '\n')
                md_lines.append(func.comment + '\n\n')
                self.append_indent(md_lines, func.long_comment.split('\n'))

        '''



    # Classes
    def generate_classes_txt_md(self):

        #
        out_path_txt = os.path.join(AUTOGEN_PATH, 'class_functions')
        out_path_md = os.path.join(AUTOGEN_PATH, 'class_functions_md')

        #
        class_list = list(self.class_dict.keys())
        class_list.sort()
        lines = class_list.copy()
        for i, line in enumerate(lines):
            lines[i] = self.unpack_name(line)
        self.write_file(self.class_list_filename, lines)

        #
        for cls_name in class_list:

            cls = self.get_class(cls_name)
            func_list = list(cls.func_dict.keys())
            func_list.sort()
            if len(func_list) == 0:
                continue

            cls_fname_txt = os.path.join(out_path_txt, cls_name + '.txt')
            cls_fname_md = os.path.join(out_path_md, cls_name + '.md')

            # Txt
            lines = []
            lines.append('Class: ' + self.unpack_name(cls_name))
            lines.append('')

            # MD
            md_lines = []
            md_lines.append('# Class: ' + self.unpack_name(cls_name))
            md_lines.append('')
            md_lines.append(cls.comment + '\n')
            self.append_indent(md_lines, cls.long_comment.split('\n'))
            md_lines.append('')

            # MD
            md_lines.append('### Functions List\n')
            md_func_list = []
            for func_name in func_list:
                func = cls.func_dict[func_name]
                line = func.name + ' - ' + func.comment
                md_func_list.append(line)
            self.append_indent(md_lines, md_func_list)
            md_lines.append('')

            # Iterate class functions
            for func_name in func_list:
                func = cls.func_dict[func_name]
                line = func.name + ' - ' + func.comment
                lines.append(line + '\n')

                # Section per function
                md_lines.append('### ' + func.name + '\n')
                md_lines.append(func.comment + '\n\n')
                self.append_indent(md_lines, func.long_comment.split('\n'))
                md_lines.append('\n')

            # Write txt and md files to disk
            self.write_file(cls_fname_txt, lines)
            self.write_file(cls_fname_md, md_lines)


    def generate_classes_mlx(self):

        # Update class list files
        out_path_mlx = os.path.join(AUTOGEN_PATH, 'class_functions_mlx')
        class_list = list(self.class_dict.keys())
        class_list.sort()
        lines = class_list.copy()
        for i, line in enumerate(lines):
            lines[i] = self.unpack_name(line)
        #self.write_file(self.class_list_filename, lines)

        for cls_name in class_list:

            cls = self.get_class(cls_name)
            func_list = list(cls.func_dict.keys())
            func_list.sort()
            prop_list = list(cls.prop_dict.keys())
            prop_list.sort()
            if len(func_list) == 0 and len(prop_list) == 0:
                continue

            cls_fname_mlx = os.path.join(out_path_mlx, self.unpack_name(cls_name) + '.mlx')

            # MLX
            mlx = MlxWriter(cls_fname_mlx)
            mlx.title(self.unpack_name(cls_name))

            #
            mlx.heading1('Description')
            mlx.start_par()
            mlx.bold(self.unpack_name(cls_name))
            mlx.normal(cls.comment)
            mlx.end_par()

            #
            mlx.text(cls.long_comment)
            mlx.text('For additional help see manuals.main')

            mlx.heading1('Properties')
            for prop_name in prop_list:
                prop = cls.prop_dict[prop_name]
                mlx.bullet()
                mlx.bold(prop.name)
                mlx.normal(' - ' + prop.comment)
                mlx.end_par()


            mlx.heading2('Additional & Hidden Properties')
            mlx.text('Properties')

            mlx.heading1('Constructor')
            for func_name in func_list:
                func = cls.func_dict[func_name]
                if not func.is_constructor:
                    continue
                mlx.start_par()
                mlx.bold(func.name)
                mlx.normal(' - ' + func.comment)
                mlx.end_par()

            mlx.text('')
            mlx.heading1('Methods')

            # Iterate class functions
            for func_name in func_list:
                func = cls.func_dict[func_name]
                if func.is_constructor:
                    continue
                mlx.bullet()
                mlx.bold(func.name)
                mlx.normal(' - ' + func.comment)
                mlx.end_par()

            # Section per function
            for func_name in func_list:
                func = cls.func_dict[func_name]
                mlx.text('')
                mlx.heading3(func.name)
                mlx.start_par()
                mlx.bold(func.name)
                mlx.normal(' - ' + func.comment)
                mlx.end_par()
                mlx.text(func.long_comment)
                mlx.text('Example')
                mlx.code('Example code here')


            mlx.heading1('Static Methods')
            mlx.text('Static Methods')

            mlx.heading1('See Also', 'Add related issues here')
            mlx.heading1('Notes', 'Add notes here')

            mlx.close()


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

        self.generate_packages_txt_md()
        self.generate_packages_mlx()
        self.generate_classes_txt_md()
        self.generate_classes_mlx()


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

    MlxWriter.unit_test()

    #proc.process('D:/Ultrasat/AstroPack.git/matlab/base')


    proc.process('D:/Ultrasat/AstroPack.git/matlab')

    #proc.process('D:\\Ultrasat\\AstroPack.git\\matlab\\util\\+tools\\+interp')
    #proc.process('D:/Ultrasat/AstroPack.git/matlab/base/@Base')
    #proc.process('D:/Ultrasat/AstroPack.git/matlab/util/+db')


if __name__ == '__main__':
    main()
