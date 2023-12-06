# Compile MEX files
#
# Author: Chen Tishler (May 2023)
#
# Usage:
#
#   -d directory    - Process folder with optionaliy subfolders (-r)
#   -f filename     - Process file
#   -r              - Recurse (process subfolders)
#
# Linux examples:
#   /dev/AstroPack.git/matlab$ python3 util/scripts/cmex.py -d . -r
#
# Windows examples:
#   C:\Ultrasat\AstroPack.git\matlab\util\+tools>python3 ..\scripts\cmex.py -d +array/mex_bitsetFlag_include.cpp
#   >python3 ..\scripts\cmex.py -f +array/+mex/mex_bitsetFlag_include.cpp
#

import os, glob, argparse, subprocess
from base import init_log, msg_log, Color


class MexCompiler:
    """

    """

    def __init__(self):
        """

        """
        self.is_win = os.name == 'nt'

        #
        self.mex_flags = ''  # '-v'
        self.ext_list = ['.c', '.cpp']
        self.win_mex = 'mex {} COMPFLAGS="$COMPFLAGS /openmp"'
        self.win_mex_avx2 = 'mex {} COMPFLAGS="$COMPFLAGS /openmp /arch:AVX2"'
        self.win_mex_avx512 = 'mex {} COMPFLAGS="$COMPFLAGS /openmp /arch:AVX512"'
        self.linux_mex = "mex " + self.mex_flags + " CXXFLAGS='$CXXFLAGS -fopenmp' LDFLAGS='$LDFLAGS -fopenmp' CXXOPTIMFLAGS='-O3 -DNDEBUG' {}"
        self.linux_mex_avx2 = "mex " + self.mex_flags + " CXXFLAGS='$CXXFLAGS -fopenmp -mavx2' LDFLAGS='$LDFLAGS -fopenmp' CXXOPTIMFLAGS='-O3 -DNDEBUG' {}"
        self.linux_mex_avx512 = "mex " + self.mex_flags + " CXXFLAGS='$CXXFLAGS -fopenmp -mavx2' LDFLAGS='$LDFLAGS -fopenmp' CXXOPTIMFLAGS='-O3 -DNDEBUG' {}"

        #
        self.mex_cmd_list = []
        self.mex_cmd_output = {}


    def log(self, msg, color=None):
        # Log message to file
        msg_log(msg, color=color)


    def run_mex(self, fn):
        #
        try:
            if self.is_win:
                cmd = self.win_mex.format(fn)
            else:
                cmd = self.linux_mex.format(fn)

            cwd_cmd = f'[{os.getcwd()}] {cmd}'
            self.mex_cmd_list.append(cwd_cmd)
            self.log(cwd_cmd, color=Color.blue)

            result = subprocess.run(cmd, shell=True, capture_output=True, text=True)
            self.mex_cmd_output[cwd_cmd] = {'out': result.stdout, 'err': result.stderr }

            if 'MEX completed successfully' in result.stdout:
                self.log(result.stdout, color=Color.green)
            else:
                self.log(result.stdout, color=Color.black)
            self.log(result.stderr, color=Color.red)
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
                #self.log(f'{marker}: {str(flags)}')

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
                self.log(f'skipping auto-generated file: {fname}', color=Color.black)
                return

            dtypes = []
            for line in lines:
                flags = self.get_flags(line, '$dtype:')
                if flags:
                    dtypes = flags

            self.log(f'\nprocessing INCLUDE file: {fname} - $dtype: {str(dtypes)}\n', color=Color.purple)

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
            return

        save_cwd = os.getcwd()
        folder, fn = os.path.split(fname)
        try:
            os.chdir(folder)

            with open(fname) as f:
                lines = f.read().splitlines()

            # Do NOT process auto-generated files
            if len(lines) > 0 and lines[0].startswith('// $Auto generated file'):
                self.log(f'skipping auto-generated file: {fname}', color=Color.black)
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

    def summary(self):
        self.log('\nSummary:\n')
        for cmd in self.mex_cmd_list:
            self.log(cmd)

        self.log('\nError summary:\n')
        for cmd in self.mex_cmd_output:
            out = self.mex_cmd_output[cmd]
            if out['err'] != '':
                self.log(cmd, Color.blue)

                if 'MEX completed successfully' in out['out']:
                    self.log(out['out'], color=Color.green)
                else:
                    self.log(out['out'], color=Color.black)

                self.log(out['err'], color=Color.red)


def main():

    logger = init_log()
    logger.use_dt = False
    logger.use_pid = False

    logger.msg_log('Compile all MEX files in folder')
    logger.msg_log('by Chen Tishler, 05/2023')
    logger.msg_log('\n*** NOTE: You may need to type "clear all" in MATLAB to release the current compiled binary file.\n')


    # Read command line options
    parser = argparse.ArgumentParser()

    # Arguments
    parser.add_argument('-a',       dest='all',     action='store_true', default=False, help='Compile all supported mex files under /matlab subfolder')
    parser.add_argument('-d',       dest='dir',     default=None, help='Folder to process')
    parser.add_argument('-f',       dest='file',    default=None, help='File to process')
    parser.add_argument('-r',       dest='recurse', action='store_true', default=False, help='Process subfolders')

    args = parser.parse_args()

    mc = MexCompiler()
    #folder = 'C:/Ultrasat/AstroPack.git/matlab/util/+tools/+operators/+mex'
    #folder = r'C:\Ultrasat\AstroPack.git\matlab\util\+tools\+checksum\+mex'
    #folder = r'C:\Ultrasat\AstroPack.git\matlab\util\+tools\+array'
    #mc.process_folder(folder)

    if args.all:
        cur_dir = os.path.dirname(os.path.abspath(__file__))
        dir = os.path.abspath(os.path.join(cur_dir, '../..'))
        mc.process_folder(os.path.abspath(dir), True)
    elif args.dir:
        mc.process_folder(os.path.abspath(args.dir), args.recurse)
    elif args.file:
        mc.process_file(os.path.abspath(args.file))
    else:
        #fn = '../../+tools/+array/+mex/mex_bitsetFlag_include.cpp'
        #mc.process_file(os.path.abspath(fn))
        return

    mc.summary()


if __name__ == '__main__':
    main()
