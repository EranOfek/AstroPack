#ifndef mf_fileH
#define mf_fileH

// Windows/Linux compatible 64-bit integer
#ifndef _WIN32
	// Required on Linux to support 64-bit file offset, see features.h
	// See https://www.gnu.org/software/libc/manual/html_node/Feature-Test-Macros.html
	#define _FILE_OFFSET_BITS 64
#endif

#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <string.h>
#include <sys/types.h>
#include <string>
#include <vector>

#ifdef _WIN32
    #include <io.h>
	#include <Windows.h>
#else
    #include <unistd.h>
	#include <errno.h>
#endif

#include "mf_def.h"
#include "mf_component.h"
#include "mf_thread.h"
#include "mf_string.h"


// Cache data to be written
struct FileBuf {
	uint8_t* buf;		// Pointer to allocated memory
    size_t size;		// Size

    // Constructor - allocate and copy
    FileBuf(void* _buf, size_t _size)
    {
        size = _size;
        buf = new uint8_t[size];

		if (_buf)
			memcpy(buf, _buf, size);
    }

    // Destructor - free buffer
    ~FileBuf()
    {
        if (buf) {
            delete[] buf;
        }
    }

};

//=======================================================================

// FileBase - Base class for cross-platrform file access
class FileUtil : public Component {
public:

	// Convert '\' to '/'
	static std::string pathToLinux(const std::string& _path);

	// Convert '/' to '\'
	static std::string pathToWin(const std::string& _path);

	// Convert to Linux or Windows
	static std::string pathToNative(const std::string& _path)
	{
	#ifdef _WIN32
		return pathToWin(_path);
	#else
		return pathToLinux(_path);
	#endif
	}

	// Join path and filename
	static std::string join(const std::string& _path, const std::string& fname);

	// Remove trailing path delimiter
	static std::string excludeTrailingPathDelimiter(const std::string& _path);

	// Get path without name
	static std::string getPath(const std::string& filename);

	// Get file name without path
	static std::string getName(const std::string& filename);

	// Get file extension
	static std::string getExt(const std::string& filename);

	// Change file extension
	static std::string changeExt(const std::string& filename, const std::string& ext);

	// Get file name without extension
	static std::string getBaseName(const std::string& filename)
	{
		return changeExt(getName(filename), "");
	}

	// Create directory
	static bool createDir(const std::string& path);

	// Create directories tree (Recursive function)
	static bool createPath(const std::string& path);

	// Get disk free space, return -1 on error
	static int64_t diskFreeSpace(const std::string& path);

	// Check if file exists
	static bool exists(const std::string& path);

	// Check if directory exists
	static bool dirExists(const std::string& path);

	// Get file size
	static int64_t getFileSize(const std::string& fname);

	// Get list of files in folder, returned names are relative to path
	static std::vector<std::string> listFiles(const std::string& path, bool recursive = false);

	// Check the maximum open file supported by operating system
	// On Linux you can run 'ulimit -n' to check this limit
	static int checkMaxOpenFiles(const std::string& path, int maxCount = 65536, size_t writeSize = 0, bool remove = true);

	// Get temporary path
	static std::string getTempPath();

	// Get current path
	static std::string getCurPath();

	// Read entire file as binary string
	static std::string readFileBinary(const std::string& fname);

	// Read entire file as text string
	static std::string readFileText(const std::string& fname);
};


// FileBase - Base class for cross-platrform file access
class FileBase : public Component {
public:

	// Default constructor
	FileBase() : filename(""), useWriteQueue(false), errorCode(0) {}

	// Copy constructor
	FileBase(const FileBase& _f) : filename(_f.filename), useWriteQueue(_f.useWriteQueue), errorCode(0) {}

	// Destructor - release
	virtual ~FileBase() {}

	// Required by std containers
	FileBase& operator= (const FileBase& _f)
	{
		filename = _f.filename;
		useWriteQueue = _f.useWriteQueue;
		return *this;
	}

	// Open file, optionally delete existing file
	// mode, from fopen() man: C string containing a file access mode. It can be:
	//		"r"	  read:			 Open file for input operations. The file must exist.
	//		"w"	  write:		 Create an empty file for output operations. If a file with the same name already exists, its contents are discardedand the file is treated as a new empty file.
	//		"a"	  append:		 Open file for output at the end of a file. Output operations always write data at the end of the file, expanding it.Repositioning operations(fseek, fsetpos, rewind) are ignored.The file is created if it does not exist.
	//		"r+"  read/update:	 Open a file for update (both for input and output). The file must exist.
	//		"w+"  write/update:  Create an empty file and open it for update (both for inputand output). If a file with the same name already exists its contents are discardedand the file is treated as a new empty file.
	//		"a+"  append/update: Open a file for update (both for input and output) with all output operations writing data at the end of the file.Repositioning operations(fseek, fsetpos, rewind) affects the next input operations, but output operations move the position back to the end of file.The file is created if it does not exist.
	virtual bool open(const std::string& fname, const std::string& mode) = 0;

	// Return true if file is open
	virtual bool isOpen() = 0;

	// Close file, optionally delete existing file
	virtual bool close() = 0;

 	// Write data to file
	virtual int write(const void* buf, size_t size, int64_t pos = -1) = 0;

	// Write string to file
	virtual int writeStr(const std::string& str)
	{
		return write(str.c_str(), str.length());
	}

	// Write string to file
	virtual void writeStrQueue(const std::string& str)
	{
		FileBuf* buf = new FileBuf((void*)str.c_str(), str.length());
		writeQueue.push_back(buf);
	}

	// Read data from file
	virtual int read(void* buf, size_t size, int64_t pos = -1) = 0;

	// Set file position
	virtual bool setPos(int64_t pos, int mode) = 0;

	// Get file position by matId, optionally add the mat to map
	virtual int64_t getPos() = 0;

	// Get file size
	virtual int64_t getSize() = 0;

	// Flush file to disk
	virtual bool flush() = 0;

	// Set errorCode for last operation
#ifdef _WIN32
	virtual void setError(DWORD _errorCode, const char* _msg = "") { errorCode = (int)_errorCode; }
#else
	virtual void setError(int _errorCode, const char* _msg = "") { errorCode = _errorCode; }
#endif

	// Set file name
	virtual void setFileName(const std::string& fname) 
	{
		filename = fname;
	}

    // Open file, append data, and close
    virtual bool openAppendClose(const std::string& fname, const std::string& text);

    // Insert in the middle or beginning of the file
    virtual bool shiftFile(int64_t pos, int64_t insertLen);

	// Flush write queue, return number of bytes written
	virtual int64_t flushWriteQueue();

	// Get last error code
	virtual int getError() { return errorCode; }

    // Data
	std::string filename;				// File name
    std::vector<FileBuf*> writeQueue;	// Optional write queue
	bool useWriteQueue;					// True to use write queue
	int errorCode;						//
};

//===========================================================================
// FileRaw - Unbuffered file access using file descriptors (use for Linux)
class FileRaw : public FileBase {
public:

	// Default constructor
	FileRaw() : FileBase(), fd(-1) {}

	// Copy constructor
	FileRaw(const FileRaw& _f) : FileBase(_f), fd(_f.fd) {}

	// Destructor - release
	virtual ~FileRaw() { close();  }

	// Required by std containers
	FileRaw& operator= (const FileRaw& _f)
	{
		FileBase::operator=(_f);
		fd = _f.fd;
		return *this;
	}

	// Open file, optionally delete existing file
	virtual bool open(const std::string& fname, const std::string& mode);
	
	// Return true if file is open
	virtual bool isOpen() { return (fd > -1); }

	// Close file, optionally delete existing file
	virtual bool close();

	// Write data to file
	virtual int write(const void* buf, size_t size, int64_t pos = -1)
	{
		if (pos > -1)
			setPos(pos, SEEK_SET);

		int ret = ::write(fd, buf, (unsigned int)size);
		setError(ret > -1 ? 0 : errno, "write");
		return ret;
	}

	// Read data from file
	virtual int read(void* buf, size_t size, int64_t pos = -1)
	{
		if (pos > -1)
			setPos(pos, SEEK_SET);

		int ret = ::read(fd, buf, (unsigned int)size);
		setError(ret > -1 ? 0 : errno, "read");
		return ret;
	}

	// Set file position
	virtual bool setPos(int64_t pos, int mode)
	{
#ifdef _WIN32
		int64_t ret = _lseeki64(fd, pos, mode);
		setError(ret > -1 ? 0 : errno, "_lseeki64");
#else
		int64_t ret = lseek64(fd, pos, mode);
		setError(ret > -1 ? 0 : errno, "lseek64");
#endif
		return (ret == pos);
	}

	// Get file position by matId, optionally add the mat to map
	virtual int64_t getPos()
	{
#ifdef _WIN32
		int64_t ret = _lseeki64(fd, 0, SEEK_CUR);
		setError(ret > -1 ? 0 : errno, "_lseeki64");
#else
		int64_t ret = lseek64(fd, 0, SEEK_CUR);
		setError(ret > -1 ? 0 : errno, "lseek64");
#endif
		return ret;
	}

	// Get file size
	virtual int64_t getSize()
	{
		int64_t pos = getPos();
		setPos(0, SEEK_END);
		int64_t size = getPos();
		setPos(pos, SEEK_SET);
		return size;
	}

	// Flush file to disk
	virtual bool flush()
	{
#ifdef _WIN32
		// fsync is not implemented on Windows - check how to do it
		return true;
#else
		int ret = fsync(fd);
		setError(ret == 0 ? 0 : errno, "fsync");
		return (ret == 0);
#endif
	}

	// Set errorCode for last operation
	virtual void setError(int _errorCode, const char* _msg = "");

	//
	static int openFilesCount;

private:
	int fd;
};

//===========================================================================
// FileFILE - Buffered file access using FILE
// @Todo: Currently errorCode is not set by any function.
// ** Currently unused in mflow **
class FileFILE : public FileBase {
public:

	// Default constructor
	FileFILE() : FileBase(), fp(0) {}

	// Copy constructor
	FileFILE(const FileFILE& _f) : FileBase(_f), fp(_f.fp) {}

	// Destructor - release
	virtual ~FileFILE() { close(); }

	// Required by std containers
	FileFILE& operator= (const FileFILE& _f)
	{
		FileBase::operator=(_f);
		fp = _f.fp;
		return *this;
	}

	// Open file, optionally delete existing file
	virtual bool open(const std::string& fname, const std::string& mode);

	// Return true if file is open
	virtual bool isOpen() { return (fp != NULL); }

	// Close file, optionally delete existing file
	virtual bool close();

	// Write data to file
	virtual int write(const void* buf, size_t size, int64_t pos = -1);

	// Read data from file
	virtual int read(void* buf, size_t size, int64_t pos = -1);

	// Set file position
	virtual bool setPos(int64_t pos, int mode);

	// Get file position by matId, optionally add the mat to map
	virtual int64_t getPos();

	// Get file size
	virtual int64_t getSize();

	// Flush file to disk
	virtual bool flush();

private:
	FILE* fp;		// File
};

//===========================================================================
#ifdef _WIN32
// FileWin - Windows implementation using Windows API
// Required to avoid open files limit caused by stdlib wrapper around file access
class FileWin : public FileBase {
public:

	// Default constructor
	FileWin() : FileBase(), handle(INVALID_HANDLE_VALUE) {}

	// Copy constructor
	FileWin(const FileWin& _f) : FileBase(_f), handle(_f.handle) {}

	// Destructor
	virtual ~FileWin() { close(); }

	// Required by std containers
	FileWin& operator= (const FileWin& _f)
	{
		FileBase::operator=(_f);
		handle = _f.handle;
		return *this;
	}

	// Open file, optionally delete existing file
	virtual bool open(const std::string& fname, const std::string& mode);

	// Return true if file is open
	virtual bool isOpen() { return (handle != INVALID_HANDLE_VALUE); }

	// Close file, optionally delete existing file
	virtual bool close();

	// Write data to file
	virtual int write(const void* buf, size_t size, int64_t pos = -1);

	// Read data from file
	virtual int read(void* buf, size_t size, int64_t pos = -1);

	// Set file position
	virtual bool setPos(int64_t pos, int mode);

	// Get file position by matId, optionally add the mat to map
	virtual int64_t getPos();

	// Get file size
	virtual int64_t getSize();

	// Flush file to disk
	virtual bool flush();

    // Return true if we have valid file handle
    bool haveHandle() { return (handle != INVALID_HANDLE_VALUE); }

	// Set errorCode for last operation, return true if there was an error
	virtual void setError(DWORD _errorCode, const char* _msg = "");

private:
	HANDLE handle;       // Windows file handle
};

// O/S native implemenation
typedef FileWin FileNative;
//typedef FileRaw FileNative;

#else  // Windows

// Linux
typedef FileRaw FileLinux;
typedef FileRaw FileNative;
#endif

#endif
