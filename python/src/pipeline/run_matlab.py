# Run the MATLAB Pipeline script or compiled build


import os, glob, time, argparse, shutil, csv, json, yaml, uuid
from datetime import datetime

USE_MATLAB = True
USE_EXEC = False


# Log message to file
LOG_PATH = 'c:/temp/'
logfile_dt = datetime.now().strftime('%%y_%m_%d__%H_%M_%S_%f')[:-3]
logfile_name = os.path.join(LOG_PATH, 'run_pipeline_' + logfile_dt + '.log'
logfile = open(logfile_name, 'a')
def log(msg, dt = False):
    global logfile
    if msg == '': dt = False
    if dt: msg = datetime.now().strftime('%d/%m/%y %H:%M:%S.%f')[:-3] + ' ' + msg
    print(msg)
    if logfile:
        logfile.write(msg)
        logfile.write("\n")
        logfile.flush()



# Run command line with timing
def run(cmdline):
    log('run: ' + cmdline)
    try:
        start = time.time()
        os.system(cmdline)
        end = time.time()
        elapsed = end - start
        log(cmdline + ' - ' + ('%.3f' % elapsed) + " seconds")
        return elapsed
    except:
        log('run: FAILED: ' + cmdline)
        return 0


# Run matlab script or compiled app
# https://www.mathworks.com/matlabcentral/answers/97204-how-can-i-pass-input-parameters-when-running-matlab-in-batch-mode-in-windows
# https://www.mathworks.com/help/compiler/create-and-install-a-standalone-application-from-matlab-code.html
def run_matlab(func_name, func_args):
    log('run_image_file: ' + func_name)

    # matlab /r "x=2;myscript"
    # matlab /r "myfunc(2)"
    if USE_MATLAB:
        log('Running using matlab')
        cmd = 'matlab /r ' + func_name
        if func_args != '':
            cmd += '(' + func_args + ')'

    elif USE_EXEC:
        log('Running using exec')
        cmd = func_name
        if func_args != '':
            cmd += ' ' + ' '.join(func_args.split(','))
    else:
        log('run method not supportred')

    if cmd != '':
        res = run(cmd)
    else:
        res = False

    return res



def run_image_file(image_file_name):
    log('run_image_file: ' + image_file_name)
    return run_matlab('pipeline', image_file_name)



def main():

    # Read command line options
    parser = argparse.ArgumentParser()

    # Arguments
    parser.add_argument('-f',           dest='filename',         default=None,                                   help='image file name')
    #parser.add_argument('-d',           dest='dir',         default=None,                                   help='pcap folder')
    #parser.add_argument('-s',           dest='subdirs',     action='store_true',    default=True,   help='Process pcap files in subfolders')
    args = parser.parse_args()

    if args.filename:
        run_image_file(args.filename)


if __name__ == '__main__':
    main()
