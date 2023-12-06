#ifndef SHAREDMEM_H
#define SHAREDMEM_H
//
// Created by Chen Tishler on 05/07/2021.
//

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <string.h>

#ifdef WIN32    // Windows includes
#include <Windows.h>
#include <process.h>
#define sleep(x) Sleep(x)

#else        // Linux includes
#include <pthread.h>
#include <sys/mman.h>
#endif

// Unsigned 32-bit int is used for all fields
typedef unsigned int uint32_t;

#define MEMSIGNATURE 0x11031973     // Must match memsignature in shared_mem.py

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

// Flags for BufHeader::flags
// Use flags to specify additional data such as image format (@Ido)
// If you add more data to this struct, modify in shared_mem.py:
// memheader_size and bufheader_size, and note the fields order in do_get_buf()
#define BUFH_UYVY           0x00000001      // Buffer is raw image with format V4L2_PIX_FMT_UYVY
#define BUFH_MAT_RGB        0x00000002      // Buffer is RGB opencv Mat


struct BufHeader {
    uint32_t    put_counter;    // Running counter by put_buf()
    uint32_t    buf_len;        // Data length
    uint32_t    width;          // Image width
    uint32_t    height;         // Image height
    uint32_t    flags;          // Flags, BUFH_...
};


// Cross-process thread-safe mutex
// Must be compatible with the Locker class of the Python side
// Currently unused
class Mutex {
public:
    // Constructor with specified name
    Mutex(const char* name);

    // Destructor
    ~Mutex();

    // Lock
    bool lock();

    // Unlock
    bool unlock();

    char     name[256];     // Mutex name
#ifdef WIN32
    HANDLE   hMutex;        // Mutex handle for synchronization
    DWORD    dwLastError;
#else
#endif

};


// Automatic lock/unlock for Mutex object
class Locker {
public:
    // Constructor - Lock
    Locker(Mutex* _mutex) : mutex(_mutex)
        { if (mutex) mutex->lock(); }

    // Destructor - Unlock
    ~Locker()
        { if (mutex) mutex->unlock(); }

private:
    // Pointer to mutex object
    Mutex* mutex;
};


// Shared-Memory
// Must be compatible with the Shared-Memory class of the Python side
class SharedMemory {
public:
    // Constructor
    SharedMemory();

    // Destructor
    ~SharedMemory();

    // Initialize
    bool init(const char* _name, uint32_t _item_size, uint32_t _item_alloc);

    // Open file and shared-memory object
    bool open();

    // Close
    bool close();

    // Put buffer in queue
    bool put_buf(void* buf, uint32_t len, uint32_t w, uint32_t h, uint32_t flags);

    // Get next buffer from queue, return buffer size or zero if queue is empty
    uint32_t get_buf(void* buf, BufHeader& bufhdr);

    // Return true if shared-memory is open
    bool is_open()
        { return (memptr != NULL); }

    // Get current put_counter
    uint32_t get_put_counter()
        { return put_counter; }

    // Unit-test
    static bool unitTest(int argc, char **argv);

private:

    // Write block to shared memory
    bool write(uint32_t pos, void* buf, uint32_t size);

    // Read block from shared memory
    bool read(uint32_t pos, void* buf, uint32_t size);

private:
    // Data
    char        path[256];          // Folder path
    char        memname[256];       // Memory name
    char        filename[256];      // Full file name (including path)
    uint32_t    memsize;            // Allocated memory size
    char*       memptr;             // Pointer to beginning of shared-memory
    MemHeader*  header;             // Pointer to header at the beginning of shared-memory
    char*       data;               // Pointer to beginning of data (right after the header)
    Mutex*      mutex;              // Mutex object - Currently unused
    uint32_t    put_counter;        // put_buf() counter

#ifdef WIN32
    HANDLE hFile;                   // File handle, returned by CreateFile()
    HANDLE hMap;                    // Mapped memory handle
#else
	int fd;							// File descriptor, returned by open()
#endif
};

#endif
