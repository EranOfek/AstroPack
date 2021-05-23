#

# Imports
import os, glob, time, argparse
from datetime import datetime

#------------------------------- Parameters -------------------------------

# Set as we have in C++
_WIN32 = os.name == 'nt'

#--------------------------- Utility Functions ---------------------------

# Log message to file
logfile = None
def log(msg, dt = True):
    global logfile
    if logfile == None: return
    if msg == '': dt = False
    if dt: msg = datetime.now().strftime('%d/%m/%y %H:%M:%S.%f')[:-3] + ' ' + msg
    logfile.write(msg)
    logfile.write("\n")
    logfile.flush()
    print(msg)


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



def mkdirs(path):
    if not os.path.exists(path):
        #log('mkdirs: ' + path)
        os.makedirs(path)


# Run mflow, return full path to report.json
def run_mflow(filename, iter):
    global args

    # Prepare folders
    mflow_bin = os.path.join(base_bin, args.mflow_bin)
    mflow_outdir = os.path.join(base_outdir, str(iter) + '_mflow/')

    if args.pcapdir:
        dir = os.path.join(args.pcapdir, '')
        sub_fname = filename[len(dir):]
        sub_fname = os.path.splitext(sub_fname)[0]
        sub_dir = os.path.join(mflow_outdir, sub_fname)
        mflow_outdir = sub_dir

    log_dir = os.path.join(base_outdir, str(iter) + '_log/')
    mkdirs(mflow_outdir)
    mkdirs(log_dir)

    fname = os.path.split(filename)[1]
    fn = os.path.splitext(fname)[0]
    outdir = os.path.join(mflow_outdir, '')
    outdir_fn = os.path.join(mflow_outdir, fn)

    # Create and change to output folder, so result path in report.json would be without folder name
    mkdirs(outdir)
    os.chdir(outdir)

    # Prepare command line
    cmdline = mflow_bin + ' -r ' + filename + ' -tcpflow'
    if args.flowlog: cmdline += ' -flowlog'
    else: cmdline += ' -nolog'
    if args.scanner: cmdline += ' -tcpscanner'
    if args.no_ratelimit: cmdline += ' -no_ratelimit'
    if args.hash: cmdline += ' -hash'

    # Prepare stdout redirection and run
    stdout_fname = os.path.join(log_dir, fn + '_mflow.txt')
    cmdline += ' > ' + stdout_fname
    seconds = run(cmdline)

    # Check output
    json_filename = os.path.join(outdir, 'report.json')
    if os.path.exists(json_filename):
        pass
        #log('json: ' + json_filename)
    else:
        log('json not found: ' + json_filename)

    sorted_json = json_filename
    return sorted_json, seconds


#------------------------------- Windows -------------------------------

#
def prepare_windows():


    # Verify Matlab installation

    # Verify Python installation

    # Lazarus (free Pascal)

    # Postgres

    # Utils


    return True


#------------------------------- Linux -------------------------------

#
def prepare_linux():

    # Verify Matlab installation

    # Verify Python installation

    # Lazarus (free Pascal)

    # Postgres

    # Utils

    return True



#------------------------------- Main -------------------------------

# parser.add_option("-f", "--file", action="store", type="string", dest="filename")
def main():
    global logfile, base_bin, base_outdir, args

    # Read command line options
    parser = argparse.ArgumentParser()

    # Arguments
    parser.add_argument('-b',           dest='bin',             default='/home/chen/',                          help='Binaries base folder')
    parser.add_argument('-d',           dest='pcapdir',         default=None,                                   help='pcap folder')
    parser.add_argument('-iter',        dest='iters',           default=3,                                      help='pcap folder')

    # Flags
    parser.add_argument('-s',           dest='subdirs',         action='store_true',    default=True,   help='Process pcap files in subfolders')
    parser.add_argument('-1',           dest='hash',            action='store_true',    default=False,  help='Calculate files CRC32')
    base_bin = args.bin
    base_outdir = args.outdir

    # Create log file based on current date/time
    mkdirs(base_outdir)
    dtstr = datetime.now().strftime('%Y_%m_%d__%H_%M_%S')
    logfile = open(os.path.join(base_outdir, 'process_files_' + dtstr + '.log'), 'a')
    log('started')

    # Prepare input parameters
    files = []
    if args.pcapfile:
        files.append(args.pcapfile)

    # Scan folder, optionally with sub-folders, add all pcap fies to files[]
    dirs = []
    if args.pcapdir:
        dirs.append(args.pcapdir)

    suffix = '.pcap'
    win_suffix = '_$win.pcap'
    for path in dirs:
        log('Scanning folder: %s' % path)
        if args.subdirs:
            flist = glob.glob(os.path.join(path, '**/*.pcap'), recursive=True)
        else:
            flist = glob.glob(os.path.join(path, '*.pcap'), recursive=False)

        for filename in flist:
            path, fname = os.path.split(filename)
            use_file = fname[-len(suffix):] == suffix
            win_file = fname[-len(win_suffix):] == win_suffix
            if use_file and not win_file:
                files.append(filename)

    log('Found total %d data files' % len(files))
    if len(files) == 0:
        log('No pcap files found in specfied folders')
        return

    # Process all data files
    for pcap_fn in files:
        try:
            process_file(pcap_fn)
        except:
            log('Exception while processing file: ' + pcap_fn)


    # Add all artifacts to 7z archive
    # Requires: sudo apt-get install p7zip-full
    if args.artifacts:
        os.chdir(base_outdir)
        fn7z = dtstr + '.7z'
        cmdline = '7z a -r ' + fn7z + ' *.json *.log *.txt log/'
        run(cmdline)


if __name__ == '__main__':
    main()
