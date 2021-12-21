# Trim trailing spaces from all lines in processed files.
#
# Author: Chen Tishler (May 2021)

# See: https://gist.github.com/antivanov/59e00f6129725e9b4404

import os, glob, time, argparse, shutil
from datetime import datetime

FILE_EXT = ['.txt', '.m']


# Log message to file
LOG_PATH = 'c:/temp/'
logfile = open(os.path.join(LOG_PATH, 'trim_trailing_spaces.log'), 'a')
def log(msg, dt = False):
    global logfile
    if msg == '': dt = False
    if dt: msg = datetime.now().strftime('%d/%m/%y %H:%M:%S.%f')[:-3] + ' ' + msg
    print(msg)
    if logfile:
        logfile.write(msg)
        logfile.write("\n")
        logfile.flush()



def process_file(fname):
    with open(fname) as f:
        lines = f.read().splitlines()

    new_lines = []
    for line in lines:
        line = line.rstrip()
        #line = line.replace("/t", "    ")

    with open(fname, 'wt') as f:
        for line in new_lines:
            f.write('%s\n' % line)


def process_folder(fpath, ext_list, subdirs = True):
    if subdirs:
        flist = glob.glob(os.path.join(fpath, '**/*.*'), recursive=True)
    else:
        flist = glob.glob(os.path.join(fpath, '*.*'), recursive=False)

    for fname in flist:
        fnlower = fname.lower()
        for ext in ext_list:
            ext = ext.lower()
            if fnlower.endswith(ext):
                process_file(fname)


def main():

    # Read command line options
    parser = argparse.ArgumentParser()

    # Arguments
    #parser.add_argument('-d',           dest='dir',         default=None,                                   help='pcap folder')
    #parser.add_argument('-s',           dest='subdirs',     action='store_true',    default=True,   help='Process pcap files in subfolders')
    #args = parser.parse_args()

    folder = '.'
    process_folder(folder, FILE_EXT, True)


if __name__ == '__main__':
    main()
