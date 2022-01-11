import os, glob, shutil
from datetime import datetime
from gcsbase import Component


# ===========================================================================

# ===========================================================================

# GCS Communication Manager

class GcsComm(Component):

    # Constructor
    def __init__(self):
        self.interface_name = ''

    # Destructor
    def __del__(self):
        # Deleted
        pass

    # Send message to GCS
    def send_msg(self, msg):
        pass

    # Send Target-Of-Opertunity
    def send_target_oo(self, task):
        pass

    # Send task for validatation
    def send_task_validation(self, task):
        pass

    # Send task
    def send_task(self, task):
        pass

    # Handle incoming message from GCS
    def handle_msg(self, msg):
        pass

    def handle_telemetry(self):
        pass


# ===========================================================================

# ===========================================================================

# Parent class for file based processing
#
# Polll input folder for new files:
#   Call derived processFileImpl() function
#   Delete or move the processed file to archive folder
#   Clean archive folder after specified numberof days

class FileProcessor(Component):

    # Constructor
    def __init__(self):
        super().__init__()
        self.rcv_path = ''             # Input files folder
        self.processed_path = ''       # Optional archived input files folder
        self.send_path = ''            # Optional output folder  (response/result of input files)
        self.input_file_mask = '*.*'   #
        self.KeepProcessedFiles = True   # true to keep the processed files in ProcessedPath, otherwise deleted after processing
        self.KeepOutputFiles = True      #
        self.process_files_max_age = 7      # Number of days to keep processed files in Processed Path
        self.output_files_max_age = 7       # Number of days to keep output files in Output path
        self.process_file_callback = None


    # Initialize with default settings
    def init(self, rcv_path, send_path):
        self.rcv_path = rcv_path
        self.send_path = send_path

        if self.rcv_path != '' and not os.path.isdir(self.rcv_path):
            os.mkdir(self.rcv_path)

        if self.send_path != '' and not os.path.isdir(self.send_path):
            os.mkdir(self.send_path)


    def get_send_filename(self):
        fn = datetime.now().strftime('%y_%m_%d_%H_%M_%S_%f') + '.ini'
        return os.path.join(self.send_path, fn)


    # Poll input folder with specified delay, perform single step in DelayMS == -1
    def poll_rcv(self):

        flist = glob.glob(os.path.join(self.rcv_path, self.input_file_mask), recursive=False)
        flist.sort()

        for fname in flist:
            try:
                if self.process_file_callback:
                    self.process_file_callback(fname)
            except:
                self.log('error')

            # Move file to processed folder
            path, fn = os.path.split(fname)
            processed_fname = os.path.join(self.processed_path, fn)
            shutil.move(fname, processed_fname)
            return fname


        # Delete files before specified date
        def delete_old_files(self, path, mask, delete_before):
            flist = glob.glob(os.path.join(path, mask), recursive=False)
            for fname in flist:
                t = os.path.getmtime(fname)
                ft = datetime.fromtimestamp(t)
                if ft < delete_before:
                    self.log('remove: ' + fname)
                    #os.remove(fname)


# ===========================================================================

# ===========================================================================

# GCS Message
class ImageProcessor(Component):

    # Constructor
    def __init__(self):
        self.msg_id = ''
        self.rcv_time = 0
        self.send_time = 0
        self.process_time = 0

