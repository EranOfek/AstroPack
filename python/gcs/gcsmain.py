from gcsbase import Component

# GCS Interface - Main

class GcsMain(Component):

    # Constructor
    def __init__(self):
        self.interface_name = ''

    # Destructor
    def __del__(self):
        # Deleted
        pass


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



