# Simple shared memory between two python processes

import sys, os, time, datetime, mmap, array, argparse, numpy
from sys import platform

if platform == "win32":
    FILENAME = 'c:/temp/smem_camera1'
else:
    FILENAME = '/tmp/smem_camera_0'


# Log message to file
if platform == "win32":
    LOG_PATH = 'c:/temp/'
else:
    LOG_PATH = '/tmp/'


logfile = open(os.path.join(LOG_PATH, 'shared_mem.log'), 'a')
def log(msg, dt = False):
    global logfile
    if msg == '': dt = False
    if dt: msg = datetime.datetime.now().strftime('%d/%m/%y %H:%M:%S.%f')[:-3] + ' ' + msg
    print(msg)
    if logfile:
        logfile.write(msg)
        logfile.write("\n")
        logfile.flush()


'''
// Shared memory header, at offset 0
struct MemHeader {
    uint32_t    signature;      // Unique signature
    uint32_t    memsize;        // Size of entire shared memory block
    uint32_t    flags;          // Flags
    uint32_t    item_size;      // Image buffer size
    uint32_t    item_alloc;     // Number of buffers currently in queue
    uint32_t    head;           // Head index
    uint32_t    tail;           // Tail index
    uint32_t    count;          // Number of items in buffer
};

struct BufHeader {
    uint32_t    put_counter;    // Running counter by put_buf()
    uint32_t    buf_len;        // Data length
    uint32_t    width;          // Image width
    uint32_t    height;         // Image height
    uint32_t    flags;          // Flags, BUFH_...
};

'''

#============================================================================
# Mutex locker
# See https://www.python.org/dev/peps/pep-0343/
# See https://stackoverflow.com/questions/3774328/implementing-use-of-with-object-as-f-in-custom-class-in-python
class Locker:

    def __init__(self):
        pass

    def __del__(self):
        self.unlock()

    def __enter__(self):
        self.lock()
        return self

    def __exit__(self, type, value, traceback):
        self.unlock()

    def lock(self):
        pass

    def unlock(self):
        pass


# https://stackoverflow.com/questions/6931342/system-wide-mutex-in-python-on-linux
'''
class Locker:
    def __enter__(self):
        self.fp = open("./lockfile.lck")
        fcntl.flock(self.fp.fileno(), fcntl.LOCK_EX)

    def __exit__(self, _type, value, tb):
        fcntl.flock(self.fp.fileno(), fcntl.LOCK_UN)
        self.fp.close()
'''
#============================================================================


# Shared memory class
class SharedMemory:

    # Constructor
    def __init__(self):
        print('SharedMemory created')
        self.f = None
        self.fid = None
        self.filename = None
        self.filesize = 0
        self.memsize = 100000
        self.mmap = None
        self.memsignature = 0x11031973
        self.memheader_size = 8 * 4     # sizeof(struct MemHeader)
        self.bufheader_size = 5 * 4     # sizeof(struct BufHeader)
        self.BUFH_UYVY = 0x00000001
        self.BUFH_MAT_RGB = 0x00000002

    # Destructor
    def __del__(self):
        print('SharedMemory deleted')
        #self.close()


    # Open memory mapped file
    def open(self, fname, _create = False):
        self.filename = fname
        print('SharedMemory.open: ' + self.filename)

        # Open/create file and set size
        if _create:
            if not os.path.exists(self.filename):
                self.f = open(self.filename, 'wb')
                self.f.seek(self.memsize-1)
                self.f.write(b'\0')
                self.f.close()

        # Open file
        self.f = open(self.filename, 'rb+')
        self.fid = self.f.fileno()
        self.filesize = os.path.getsize(self.filename)
        if self.filesize > 0:
            self.memsize = self.filesize

        print('file size: ', os.path.getsize(self.filename), 'mmap size: ', self.memsize)

        # Open mmap
        self.mmap = mmap.mmap(self.fid, 0, access=mmap.ACCESS_WRITE)
        self.mmap.seek(0)


    # Close file
    def close(self):
        print('SharedMemory.close: ' + self.filename)

        # Close mmap
        if self.mmap:
            self.mmap.close()
            self.mmap = None

        # Close file
        if self.fid > -1:
            self.f.close()
            self.fid = -1

        print('SharedMemory.close: done: ' + self.filename)


    # Write buffer to shared-memory
    def write(self, pos, buf):
        self.mmap.seek(pos)
        self.mmap.write(buf)


    # Read buffer from shared-memory
    def read(self, pos, nbytes):
        self.mmap.seek(pos)
        buf = self.mmap.read(nbytes)
        return buf


    # Return buf, put_counter, w, h
    def get_buf(self):
        #with m = Locker(...)
        buf, put_counter, width, height, flags = self.do_get_buf()
        return buf, put_counter, width, height, flags


    # Get next buffer from shared-memory, return bytes object or None if queue is empty
    # Return buf, put_counter, width, height, flags
    def do_get_buf(self):

        # Read header to array of unsigned integers
        header = array.array('I')
        header_bytes = self.read(0, self.memheader_size)
        if len(header_bytes) > 0:
            header.frombytes(header_bytes)

            # Check signature to validate that we have a correct header
            signature = header[0]
            if signature == self.memsignature:

                # Check 'count' field for number of items in queue
                count = header[7]
                if count > 0:

                    # Get tail and copy buffer from memory
                    memsize = header[1]
                    flags = header[2]
                    item_size = header[3]
                    item_alloc = header[4]
                    tail = header[6]
                    hdr_offset = self.memheader_size + (tail * (item_size + self.bufheader_size))

                    # Get buffer length
                    hdr_buf = self.read(hdr_offset, self.bufheader_size)
                    hdr = array.array('I')
                    hdr.frombytes(hdr_buf)

                    # Get data from header (BufHeader)
                    put_counter = hdr[0]
                    buflen = hdr[1]
                    width = hdr[2]
                    height = hdr[3]
                    flags = hdr[4]

                    # Get data buffer
                    buf = self.read(hdr_offset + self.bufheader_size, buflen)

                    # Update header: count, tail
                    count = count - 1
                    header[7] = count
                    tail = tail + 1
                    if tail == item_alloc:
                        tail = 0
                    header[6] = tail

                    # Update header, tail and count are at [5], [6]
                    header_update = array.array('I')
                    header_update.append(tail)
                    header_update.append(count)
                    update_bytes = header_update.tobytes()
                    tail_offset_in_header = 6 * 4
                    self.write(tail_offset_in_header, update_bytes)

                    return buf, put_counter, width, height, flags

        return None, 0, 0, 0, 0


# Simple test (without header) - write current time string to shared memory
def server_side():
    log('cpp side (server)')
    sm = SharedMemory()
    sm.open(FILENAME)
    while True:
        msg = datetime.datetime.now().strftime('%d/%m/%y %H:%M:%S.%f')  # [:-3]
        print(msg)
        buf = bytes(msg, 'ascii')
        sm.write(0, buf)
        time.sleep(0.1)


# Simple test - read string from shared memory
def client_side():
    log('python side (client)')
    sm = SharedMemory()
    sm.open(FILENAME)
    while True:
        buf = sm.read(0, 100)
        if len(buf) > 0:
            s = buf.decode('ascii')
            s = s.replace('\x00', '')
            if len(s) > 0:
                log('rcv: ' + s)
        time.sleep(0.01)


# Read next buffer from shared-memory queue
def client_side_buf():
    log('python side (client)')
    sm = SharedMemory()
    sm.open(FILENAME)
    last_put_counter = 0
    while True:

        buf, put_counter, w, h = sm.get_buf()
        if buf:
            print('get_buf: put_counter: {}, len: {}, w: {}, h: {}, buf[0]: {}'.format(put_counter, len(buf), w, h, buf[0]))
            if put_counter != last_put_counter+1:
                print('GAP IN put_counter: {}, last: {}'.format(put_counter, last_put_counter))

            last_put_counter = put_counter
            frame, grabbed = sm.buf_to_frame(buf, w, h)

        time.sleep(0.001)


def main():
    log('main started')

    # Read command line options
    parser = argparse.ArgumentParser()
    parser.add_argument('-s', dest='server_side',        action='store_true',   default=False, help='cpp side')
    parser.add_argument('-c', dest='client_side',        action='store_true',   default=False, help='python side')
    parser.add_argument('-t', dest='simple_client_side', action='store_true', default=False, help='simple python side')
    args = parser.parse_args()

    if args.server_side:
        server_side()
    elif args.simple_client_side:
        client_side()
    #elif args.client_side:
    else:
        client_side_buf()		


if __name__ == '__main__':
    main()

