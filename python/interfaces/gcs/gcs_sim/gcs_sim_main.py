# GCS Simulator - Command & Control Messages

# Simulate GSC image generation, by coping images to the target folder.

import os, glob, time, json, argparse, random, shutil, yaml
from datetime import datetime

config_filename = '../../../matlab/Pipeline/PipelineManager/pipeline.yml'
if not os.path.exists(config_filename):
    print('Config file not exist: ', config_filename)

with open(config_filename) as f:
    config = yaml.load(f, Loader=yaml.FullLoader)
    print(config)

conf = config['gcsSimulator']
GCS_IMAGE_PATH = conf['ImagePath']
SOURCE_IMAGE_PATH = conf['ImageSourcePath']
IMAGE_INTERVAL_SEC = 60
LOG_PATH = conf['LogPath']


#--------------------------- Utility Functions ---------------------------

# Log lines are cached in this list, to allow 'undo' of logging if we
# decide later that it should be ommited.
log_lines = None

# Log message to file
logfile = None
def log(msg, dt = False):
    global logfile, log_lines
    if msg == '': dt = False
    if dt: msg = datetime.now().strftime('%d/%m/%y %H:%M:%S.%f')[:-3] + ' ' + msg
    print(msg)
    if log_lines != None:
        log_lines.append(msg)
    elif logfile:
        logfile.write(msg)
        logfile.write("\n")
        logfile.flush()


def clear_log_lines():
    global log_lines
    log_lines = None


def write_log_lines():
    global log_lines
    lines = log_lines
    log_lines = None
    if lines:
        for line in lines:
            log(line)


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


# Creates folders
def mkdirs(path):
    if not os.path.exists(path):
        #log('mkdirs: ' + path)
        os.makedirs(path)


# Replace text between two tag lines, keeping the tags
def replace_between_tags(lines, start_tag, stop_tag, text):
    start_index = -1
    stop_index = -1

    for i, line in enumerate(lines):
        if line.strip() == start_tag:
            start_index = i
        elif line.strip() == stop_tag:
            stop_index = i

    new_lines = lines
    if start_index > -1 and stop_index > start_index:
        new_lines = lines[:start_index+1]
        new_lines.extend(text.split('\n'))
        new_lines.extend(lines[stop_index:])

    return new_lines


def replace_file_between_tags(target_fname, start_tag, stop_tag, fname, remove_file):

    # Read the generated text
    with open(fname) as f:
        text = f.read()

    if remove_file:
        os.remove(fname)

    # Load .h file
    with open(target_fname) as f:
        lines = f.read().splitlines()

    lines = replace_between_tags(lines, start_tag, stop_tag, text)

    # Replace file contents
    with open(target_fname, 'wt') as f:
        for line in lines:
            f.write('%s\n' % line)


def delay(seconds):
    try:
        while seconds > 0:
            print('Delay: ' + str(seconds), end='\r')
            time.sleep(1)
            seconds -= 1

        return True
    except KeyboardInterrupt:
        print('interrupted!')
        return False

#============================================================================

#============================================================================
#
# You can try wrapping that code in a try/except block, because keyboard interrupts are just exceptions.
# Then you can exit the loop with CTRL-C.

def sim_gcs_image():

    subdirs = False

    if subdirs:
        flist = glob.glob(os.path.join(SOURCE_IMAGE_PATH, '**/*.fits'), recursive=True)
    else:
        flist = glob.glob(os.path.join(SOURCE_IMAGE_PATH, '*.fits'), recursive=False)

    try:
        while True:
            source_filename = random.choice(flist)

            dtstr = datetime.now().strftime('%Y_%m_%d__%H_%M_%S')

            dest_filaname = os.path.join(GCS_IMAGE_PATH, dtstr) + '.fits'

            log('Copying file: ' + source_filename + ' -> ' + dest_filaname)
            shutil.copyfile(source_filename, dest_filaname)

            if not delay(IMAGE_INTERVAL_SEC):
                break

    except KeyboardInterrupt:
        print('interrupted!')

#------------------------------- Main -------------------------------

# parser.add_option("-f", "--file", action="store", type="string", dest="filename")
def main():
    global logfile, args

    # Read command line options
    parser = argparse.ArgumentParser()

    # Arguments
    #parser.add_argument('-d',           dest='dir',         default=None,                                   help='pcap folder')
    #parser.add_argument('-s',           dest='subdirs',     action='store_true',    default=True,   help='Process pcap files in subfolders')
    #args = parser.parse_args()

    # Create log file based on current date/time
    dir = './' #args.dir
    dtstr = datetime.now().strftime('%Y_%m_%d__%H_%M_%S')
    #outdir = '.' #os.path.join(dir, '_check_' + dtstr)
    #mkdirs(outdir)
    logfile = open(os.path.join(LOG_PATH, 'sim_gcs.log'), 'a')

    log('started: ' + dtstr + '  ')
    log('')

    sim_gcs_image()

    log('\n\nDone\n')


if __name__ == '__main__':
    main()

