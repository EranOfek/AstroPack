# Compile MEX files
#
# Author: Chen Tishler (May 2023)
#

import os, glob, time, argparse, shutil, platform, subprocess
from datetime import datetime


class MexCompiler:
    """

    """

    def __init__(self):
        """

        """
        self.is_win = os.name == 'nt'
        if self.is_win:
            self.log_path = 'c:/temp/'
        else:
            self.log_path = '/tmp/'
        self.logfile = open(os.path.join(self.log_path, 'cmex.log'), 'a')

        #
        self.ext_list = ['.c', '.cpp']
        self.win_mex = 'mex {} COMPFLAGS="$COMPFLAGS /openmp"'
        self.win_mex_avx2 = 'mex {} COMPFLAGS="$COMPFLAGS /openmp /arch:AVX2"'
        self.win_mex_avx512 = 'mex {} COMPFLAGS="$COMPFLAGS /openmp /arch:AVX512"'
        self.linux_mex = "mex -v CXXFLAGS='$CXXFLAGS -fopenmp' LDFLAGS='$LDFLAGS -fopenmp' CXXOPTIMFLAGS='-O3 -DNDEBUG' {}"
        self.linux_mex_avx2 = "mex -v CXXFLAGS='$CXXFLAGS -fopenmp -mavx2' LDFLAGS='$LDFLAGS -fopenmp' CXXOPTIMFLAGS='-O3 -DNDEBUG' {}"
        self.linux_mex_avx512 = "mex -v CXXFLAGS='$CXXFLAGS -fopenmp -mavx2' LDFLAGS='$LDFLAGS -fopenmp' CXXOPTIMFLAGS='-O3 -DNDEBUG' {}"


    def log(self, msg, dt = False):
        # Log message to file
        if msg == '': dt = False
        if dt: msg = datetime.now().strftime('%d/%m/%y %H:%M:%S.%f')[:-3] + ' ' + msg
        print(msg)
        if self.logfile:
            self.logfile.write(msg)
            self.logfile.write("\n")
            self.logfile.flush()


    def run_mex(self, fn):
        #
        try:
            if self.is_win:
                cmd = self.win_mex.format(fn)
            else:
                cmd = self.linux_mex.format(fn)

            self.log(f'[{os.getcwd()}] {cmd}')
            result = subprocess.run(cmd, shell=True, capture_output=True, text=True)
            #args = cmd_line.split()
            #result = subprocess.run(args, capture_output=True, text=True)

            print(result.stdout)
            print(result.stderr)
        except Exception as ex:
            self.log(str(ex))


    def get_flags(self, line, marker):
        # // $dtype: int8, int16, int32, int64, single, double
        flags = []
        line = line.strip().lower()
        if line.startswith('//'):
            if marker in line:
                flags = line.split(marker)[1].strip().split(',')
                flags = [flag.strip().lower() for flag in flags]
                self.log(f'{marker}: {str(flags)}')

        return flags

    # =======================================================================

    def write_dtype_file(self, fn, dtype):

        dtype_fn = fn.replace('_include', f'_{dtype}')
        text = []

        # int
        if dtype.startswith('int'):
            ctypes = {'int8': 'char', 'int16': 'short', 'int32': 'int', 'int64': 'long long'}
            ctype = ctypes[dtype]
            text = f'''
typedef {ctype} __Type;
#define MEX_TYPE  mx{dtype.upper()}_CLASS
#define MEX_UTYPE mxU{dtype.upper()}_CLASS
#include "{fn}"
'''

        # single
        if dtype == 'single':
            text = f'''
typedef float __Type;
#define MEX_TYPE  mxSINGLE_CLASS
#define MEX_UTYPE mxSINGLE_CLASS
#include "{fn}"
'''

        # double
        if dtype == 'double':
            text = f'''
typedef double __Type;
#define MEX_TYPE  mxDOUBLE_CLASS
#define MEX_UTYPE mxDOUBLE_CLASS
#include "{fn}"                            
'''

        if text:
            text = '// $Auto generated file\n' + text
            with open(dtype_fn, 'wt') as f:
                f.write(text)
            return dtype_fn

        return []

    # =======================================================================

    def process_include_file(self, fname):
        #
        save_cwd = os.getcwd()
        folder, fn = os.path.split(fname)
        try:
            os.chdir(folder)
            with open(fname) as f:
                lines = f.read().splitlines()

            # Do NOT process auto-generated files
            if len(lines) > 0 and lines[0].startswith('// $Auto generated file'):
                return

            dtypes = []
            for line in lines:
                flags = self.get_flags(line, '$dtype:')
                if flags: dtypes = flags

            for dtype in dtypes:
                dtype_fn = self.write_dtype_file(fn, dtype)
                if dtype_fn:
                    self.run_mex(dtype_fn)

        except Exception as ex:
            self.log(str(ex))

        os.chdir(save_cwd)


    def process_file(self, fname):
        #

        if '_include' in fname:
            self.process_include_file(fname)

        save_cwd = os.getcwd()
        folder, fn = os.path.split(fname)
        try:
            os.chdir(folder)

            with open(fname) as f:
                lines = f.read().splitlines()

            # Do NOT process auto-generated files
            if len(lines) > 0 and lines[0].startswith('// $Auto generated file'):
                return

            self.run_mex(fn)
        except Exception as ex:
            self.log(str(ex))
        os.chdir(save_cwd)

    # =======================================================================

    def process_folder(self, fpath, subdirs = True):
        #
        if subdirs:
            flist = glob.glob(os.path.join(fpath, '**/*.*'), recursive=True)
        else:
            flist = glob.glob(os.path.join(fpath, '*.*'), recursive=False)

        for fname in flist:
            fnlower = fname.lower()
            folder, fn = os.path.split(fnlower)
            if 'draft' in folder or 'obsolete' in folder or 'temp' in folder or 'bkp' in folder or 'backup' in folder:
                continue

            if folder.endswith('+mex'):
                if fn.startswith('mex_') and fn.endswith('.cpp'):
                     if not '_draft' in fn:
                        if '_include' in fn:
                            self.process_include_file(fname)
                        else:
                            self.process_file(fname)


def main():

    print('Compile all MEX files in folder')
    print('NOTE: You may need to type "clear all" in MATLAB to release the current compiled binary file.')


    # Read command line options
    parser = argparse.ArgumentParser()

    # Arguments
    parser.add_argument('-d',       dest='dir',     default=None, help='Folder to process')
    parser.add_argument('-f',       dest='file',    default=None, help='File to process')
    parser.add_argument('-r',       dest='recurse', action='store_true', default=False, help='Process subfolders')
    args = parser.parse_args()

    mc = MexCompiler()
    folder = 'C:/Ultrasat/AstroPack.git/matlab/util/+tools/+operators/+mex'
    folder = r'C:\Ultrasat\AstroPack.git\matlab\util\+tools\+checksum\+mex'
    mc.process_folder(folder)
    return

    if args.dir:
        mc.process_folder(args.dir, args.recurse)
    else:
        mc.process_file(args.file)



if __name__ == '__main__':
    main()
