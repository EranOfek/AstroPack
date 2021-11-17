//
// Created by chen on 05/07/2021.
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

#else        // POSIX includes
#include <pthread.h>
#include <sys/mman.h>
#include <unistd.h>
#include <sys/stat.h>
#define sleep(x) usleep(1000*x)
#endif
#include "sharedmem.h"


Mutex::Mutex(const char* _name)
{
    strcpy(name, _name);
#ifdef WIN32
    hMutex = CreateMutexA(NULL, false, name);
#else
#endif
}


Mutex::~Mutex()
{
#ifdef WIN32
    CloseHandle(hMutex);
    hMutex = 0;
#else
#endif
}


bool Mutex::lock()
{
#ifdef WIN32
    DWORD WaitResult = WaitForSingleObject(hMutex, 10000);  // Changed from 5000, 08/02/08.  //50 !!! (11/06/07) //dwLockTimeout);  // INFINITE);
#else
#endif

    return true;
}


bool Mutex::unlock()
{
#ifdef WIN32
    if (ReleaseMutex(hMutex)) {

    }
#else
#endif

    return true;
}


SharedMemory::SharedMemory()
{
    memset(memname, 0, sizeof(memname));
    memset(filename, 0, sizeof(filename));
    memsize = 0;
    memptr = NULL;
    data = NULL;
    header = NULL;
    mutex = NULL;
    put_counter = 0;

#ifdef WIN32
    hFile = NULL;
    hMap = NULL;
    strcpy(path, "c:\\temp\\");
#else
    fd = -1;
    strcpy(path, "/tmp/");
#endif
}


SharedMemory::~SharedMemory()
{
#ifdef WIN32
    if (hFile) {
        close();
    }

    if (mutex) {
        delete mutex;
    }
#else
	if (fd > -1) {
		close();
	}
#endif
}


bool SharedMemory::init(const char* _name, uint32_t _item_size, uint32_t _item_alloc) {

    // Calculate total size requried for shared memory
    memsize = sizeof(MemHeader) + ((_item_size + sizeof(BufHeader)) * _item_alloc);

    // Prepare shared-memory name
    strcpy(memname, "smem_");
    strcat(memname, _name);

    // Prepare file name
    strcpy(filename, path);
    strcat(filename, memname);

    if (!open()) {
        return false;
    }

    header = (MemHeader*)memptr;
    data = memptr + sizeof(MemHeader);

    // Create mutex
    mutex = new Mutex(memname);

    // Initialize header
    if (header->signature == 0) {
        header->signature = MEMSIGNATURE;
        header->memsize = memsize;
        header->flags = 0;
        header->item_size = _item_size;
        header->item_alloc = _item_alloc;
        header->head = 0;
        header->tail = 0;
        header->count = 0;
    }

    return true;
}


bool SharedMemory::open()
{
    // Delete existing file
    remove(filename);

#ifdef WIN32
    // Create a memory-mapped file (MMF)
    hFile = CreateFile((LPCTSTR)filename,
        GENERIC_WRITE | GENERIC_READ,
        FILE_SHARE_READ | FILE_SHARE_WRITE, NULL,
        OPEN_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL);

    if(hFile == INVALID_HANDLE_VALUE) {
        // Failed to create file
        return false;
    }

    DWORD fileSize = GetFileSize(hFile, NULL);

    // Create and fill file (Windows)
    if (fileSize == 0) {
        printf("Filling file: %d\n", memsize);
        char *buf = new char[memsize];
        memset(buf, 0, memsize);
        DWORD bytesWritten = 0;
        if (!WriteFile(hFile, buf, memsize, &bytesWritten, NULL)) {
        }
        delete[] buf;
    }

    hMap = CreateFileMapping(hFile, NULL, PAGE_READWRITE, 0, memsize, "MMAPShmem");
    memptr = (char *)MapViewOfFile (hMap, FILE_MAP_READ | FILE_MAP_WRITE, 0, 0, 0);
#else
    fd = ::open(filename, O_RDWR | O_CREAT, 0666);

    // Create and fill file (Linux)
    if (fd > -1) {
        struct stat statbuf;
        fstat(fd, &statbuf);
        if (statbuf.st_size == 0) {
            printf("Filling file: %d\n", memsize);
            char* buf = new char[memsize];
            memset(buf, 0, memsize);
            ::write(fd, buf, memsize);
            delete[] buf;
        }

        // Get pointer to memory map
        memptr = (char *) mmap(NULL, memsize,
                               PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);

    }
    else {

    }
#endif

    if (memptr != NULL) {
        printf("SharedMemory created: %s\n", filename);
    }
    else {
        printf("SharedMemory::open FAILED!: %s\n", filename);
    }
    return (memptr != NULL);

}


bool SharedMemory::close()
{
#ifdef WIN32
    // Windows code
    //WaitForSingleObject(handle, INFINITE);

    // Clean up
    UnmapViewOfFile(memptr);
    CloseHandle(hMap);
    CloseHandle(hFile);
#else
     // Clean up
    munmap(memptr, memsize);
    ::close(fd);
#endif
    return true;
}


bool SharedMemory::write(uint32_t pos, void* buf, uint32_t size)
{
    if (pos+size < memsize) {
        memcpy(data+pos, buf, size);
        return true;
    }
    return false;
}


bool SharedMemory::read(uint32_t pos, void* buf, uint32_t size)
{
    if (pos+size < memsize) {
        memcpy(buf, data+pos, size);
        return true;
    }
    return false;
}


bool SharedMemory::put_buf(void* buf, uint32_t len, uint32_t w, uint32_t h, uint32_t flags)
{
    Locker lock(mutex);

    // Increment counter
    put_counter++;

    // Check if queue is not full
    if (header->count < header->item_alloc) {

        // Limit buffer len to allocated size
        if (len == 0)
            len = header->item_size;
        else if (len > header->item_size) {
            printf("truncating length from: %d to %d\n", len, header->item_size);
            len = header->item_size;
        }

        // Get pointer to dest buffer in shared memory
        char* dst = data + (header->head * (header->item_size + sizeof(BufHeader)));

        // Put counter, buffer length, image dimensions
        BufHeader* hdr = (BufHeader*)dst;
        hdr->put_counter = put_counter;
        hdr->buf_len = len;
        hdr->width = w;
        hdr->height = h;
        hdr->flags = flags;

        // Copy data
        //printf("put_buf: memcpy: dst: %p, src: %p, size: %d\n", dst + sizeof(BufHeader), buf, len);
        memcpy(dst + sizeof(BufHeader), buf, len);

        // Update counters
        header->count++;
        header->head++;
        if (header->head == header->item_alloc)
            header->head = 0;

        return true;
    }
    return false;
}


uint32_t SharedMemory::get_buf(void* buf, BufHeader& bufhdr)
{
    Locker lock(mutex);

    if (header->count > 0) {

        char* src = data + header->tail * (header->item_size + sizeof(BufHeader));

        // Get counter, buffer length, image dimensions
        BufHeader* hdr = (BufHeader*)src;
        bufhdr = *hdr;
        uint32_t len = hdr->buf_len;
        if (len > 0 && (len <= header->item_size)) {
            memcpy(buf, src + sizeof(BufHeader), len);
            header->count--;
            header->tail++;
            if (header->tail == header->item_alloc)
                header->tail = 0;

            return len;
        }
    }

    return 0;
}


// Unit-test
// Run two instances of the process: when there is command line argument, it runs as server,
// putting buffers with incrementing value.
// When it runs without command line arguments, it runs as client, polling the buffers and
// displaying the value.
bool SharedMemory::unitTest(int argc, char **argv)
{
    printf("started\n");

    bool is_server = false;
    if (argc > 1)
        is_server = true;

    // Create shared memory object
    SharedMemory mem;
    uint32_t buf_size = 100000;
    char* buf = new char[buf_size];
    mem.init("camera1", buf_size, 3);

    // Server side: put new buffer once a second with incrementing value
    if (is_server) {
        printf("Running as server\n");
        unsigned char val = 1;
        while (true) {
            memset(buf, val, buf_size);
            uint32_t len = buf_size - (val % 100);
            if (mem.put_buf(buf, len, 320, 240, 0x12345678)) {
                printf("put val: %d\n", val);
                val++;
            }
            else {
                //printf("put_buf FAILED: %d\n", len);
            }
            sleep(1);
        }
    }

    // Client side: poll waiting buffers
    else {
        printf("Running as client\n");
        uint32_t count, w, h;
        while (true) {
            BufHeader bufhdr;
            uint32_t len = mem.get_buf(buf, bufhdr);
            if (len > 0) {
                unsigned char val = buf[0];
                printf("get: w: %d, h: %d, flags: %08X, val: %d\n", bufhdr.width, bufhdr.height, bufhdr.flags, val);
            }
            else {
                printf(".");
            }
            sleep(10);
        }
    }

    return true;
}


// Main (rename it to main_ut when compiling the final project)
int main_ut(int argc, char **argv)
{
    SharedMemory::unitTest(argc, argv);
    return 0;
}
