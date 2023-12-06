#include "mf_file.h"
#include "mf_perfcounter.h"
#include "mf_string.h"
#include "mf_logfile.h"

#ifndef _WIN32
    #include <dirent.h>
    #include <sys/statvfs.h>
    #include <sys/stat.h>
#endif

#include <fstream>
#include <istream>
#include <iostream>
#include <sstream> 

#include "mf_config.h"

// Static
int FileRaw::openFilesCount = 0;
//===========================================================================

std::string FileUtil::pathToLinux(const std::string& _path)
{
    std::string path = _path;
    for (int i = 0; i < path.size(); i++) {
        if (path[i] == '\\')
            path[i] = '/';
    }
    return path;
}


std::string FileUtil::pathToWin(const std::string& _path)
{
    std::string path = _path;
    for (int i = 0; i < path.size(); i++) {
        if (path[i] == '/')
            path[i] = '\\';
        else if ((i > 1) && (path[i] == ':'))
            path[i] = '.';
    }
    return path;
}


std::string FileUtil::join(const std::string& _path, const std::string& fname)
{
    std::string result;
    std::string path = _path;

    if (path.length() > 0) {

        #ifdef _WIN32
        // Replace '/' with '\'
        for (int i=0;  i < (int)path.length();  i++) {
            if (path[i] == '/') {
                path[i] = '\\';
            }
        }
        // Add trailing '\'
        if ((path.length() > 0) && (path[path.length()-1] != '\\')) {
            path += '\\';
        }
        #else
        // Replace '\' with '/'
        for (int i=0;  i < (int)path.length();  i++) {
            if (path[i] == '\\') {
                path[i] = '/';
            }
        }

        // Add trailing '/'
        if ((path.length() > 0) && (path[path.length()-1] != '/')) {
            path += '/';
        }
        #endif
    }

    result = path + fname;
    return result;
}


std::string FileUtil::excludeTrailingPathDelimiter(const std::string& _path)
{
    std::string path = _path;
    
    if (path.length() > 0) {
        size_t idx = path.size() - 1;

#ifdef _WIN32
        if (path[idx] == '\\') 
            path.erase(idx);
#else
        if (path[idx] == '/') 
            path.erase(idx);
#endif
    }
    return path;
}


std::string FileUtil::getPath(const std::string& filename)
{
    std::string path;
    std::string::size_type idx = filename.rfind('/');
    if (idx == std::string::npos)
        idx = filename.rfind('\\');

    if (idx != std::string::npos)
        path = filename.substr(0, idx);

    return path;
}


std::string FileUtil::getName(const std::string& filename)
{
    std::string name;
    std::string::size_type idx = filename.rfind('/');
    if (idx == std::string::npos)
        idx = filename.rfind('\\');

    if (idx != std::string::npos)
        name = filename.substr(idx + 1);
	else
		name = filename;

    return name;
}

std::string FileUtil::getExt(const std::string& filename)
{
    std::string ext;
    std::string::size_type idx = filename.rfind('.');
    if (idx != std::string::npos)
        ext = filename.substr(idx);

    return ext;
}


std::string FileUtil::changeExt(const std::string& filename, const std::string& ext)
{
    std::string newName = filename;
    std::string::size_type idx = filename.rfind('.');
    if (idx != std::string::npos) {
        newName.erase(idx);
        newName += ext;
    }

    return newName;
}


int64_t FileUtil::diskFreeSpace(const std::string& path)
{
    int64_t result = -1;

#ifdef _WIN32
    result = 1024*1024*1024;
#else
    struct statvfs fs;
    if (statvfs(path.c_str(), &fs) == 0) {
        result = (int64_t)fs.f_bsize * (int64_t)fs.f_bfree;
    }
#endif
    return result;
}


bool FileUtil::exists(const std::string& path)
{
    bool result = false;

    if (path.empty())
        return false;

#ifdef _WIN32
    uint32_t attr = GetFileAttributesA(path.c_str());
    return (attr != 0xFFFFFFFF) && ((attr & FILE_ATTRIBUTE_DIRECTORY) == 0);
#else
    struct stat sb;
    return ((stat(path.c_str(), &sb) == 0) && !S_ISDIR(sb.st_mode));
#endif
    return result;
}


bool FileUtil::dirExists(const std::string& path)
{
    if (path.empty())
        return false;

#ifdef _WIN32
    uint32_t attr = GetFileAttributesA(path.c_str());
    return (attr != 0xFFFFFFFF) && ((attr & FILE_ATTRIBUTE_DIRECTORY) != 0);
#else
    struct stat sb;
    return (stat(path.c_str(), &sb) == 0) && S_ISDIR(sb.st_mode);
#endif
}


int64_t FileUtil::getFileSize(const std::string& fname)
{
    int64_t fsize = -1;
#ifdef _WIN32
    WIN32_FILE_ATTRIBUTE_DATA fad;
    if (GetFileAttributesExA(fname.c_str(), GetFileExInfoStandard, &fad)) {
        LARGE_INTEGER size;
        size.HighPart = fad.nFileSizeHigh;
        size.LowPart = fad.nFileSizeLow;
        fsize = size.QuadPart;
    }
#else
    struct stat sb;
    int rc = stat(fname.c_str(), &sb);
    fsize = (rc == 0) ? sb.st_size : -1;
#endif
    return fsize;
}


bool FileUtil::createDir(const std::string& path)
{
    if (path.empty())
        return false;

    bool result = false;
#ifdef _WIN32
    result = CreateDirectoryA(path.c_str(), NULL);
#else
    result = (mkdir(path.c_str(), S_IRWXU | S_IRWXG | S_IROTH | S_IXOTH) == 0);
#endif
    return result;
}

// Note: Recusive function
bool FileUtil::createPath(const std::string& _path)
{
    if (_path.empty())
        return true;

    std::string dir = excludeTrailingPathDelimiter(_path);

    // Avoid 'xyz:\' problem on Windows
#ifdef _WIN32
    if (dir.size() < 3)
        return true;
#endif        

    if (dirExists(dir) || (getPath(dir) == dir))
        return true;

    // Get parent folder (as we removed the trailing delimiter above)
    std::string path = getPath(dir);

    // Create parent folder
    bool result = createPath(path) && createDir(dir);

    return result;
}


std::vector<std::string> FileUtil::listFiles(const std::string& path, bool recursive)
{
    std::vector<std::string> list;

#ifdef WIN32
    std::string dir = path;
    if (dir[dir.size() - 1] == '\\' || dir[dir.size() - 1] == '/')
        dir = dir.substr(0, dir.size() - 1);

    WIN32_FIND_DATAA fdata;
    HANDLE hFind = FindFirstFileA((std::string(dir).append("\\*")).c_str(), &fdata);
    if (hFind != INVALID_HANDLE_VALUE) {
        do {
            if ((fdata.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) != 0) {
                if (recursive && (strcmp(fdata.cFileName, ".") != 0) && (strcmp(fdata.cFileName, "..") != 0)) {
                    std::string subPath = FileUtil::join(path, fdata.cFileName);
                    std::vector<std::string> subList = FileUtil::listFiles(subPath, true);
                    for (auto& sub : subList) {
                        std::string fn = FileUtil::join(fdata.cFileName, sub);
                        list.push_back(fn);
                    }
                }
            }
            else {
                list.push_back(fdata.cFileName);
            }
        } while (FindNextFileA(hFind, &fdata) != 0);
        FindClose(hFind);
    }
#else
    DIR* dir;
    struct dirent* ent;
    if ((dir = opendir(path.c_str())) != NULL) {
        while ((ent = readdir(dir)) != NULL) {
            if (ent->d_type == DT_DIR) {
                if (recursive && (strcmp(ent->d_name, ".") != 0) && (strcmp(ent->d_name, "..") != 0)) {
                    std::string subPath = FileUtil::join(path, ent->d_name);
                    std::vector<std::string> subList = FileUtil::listFiles(subPath, true);
                    for (auto& sub : subList) {
                        std::string fn = FileUtil::join(ent->d_name, sub);
                        list.push_back(fn);
                    }
                }
            }
            else {
                list.push_back(ent->d_name);
            }
        }
        closedir(dir);
    }
#endif
    return list;
}


int FileUtil::checkMaxOpenFiles(const std::string& path, int maxCount, size_t writeSize, bool remove)
{
    // Allocate array of pointers
    FileBase** f = new FileBase * [maxCount];
    for (int i = 0; i < maxCount; i++)
        f[i] = NULL;

    uchar* buf = NULL;
    if (writeSize > 0) {
        buf = new uchar[writeSize];
        memset(buf, 0, writeSize);
    }

    std::string mode("w");
    int count = 0;
    for (int i = 0; i < maxCount; i++) {
        std::string fn = std::to_string(i) + ".tmp";
        //std::string fn = "015.192.136.170.00080-172.016.133.163.03621_" + std::to_string(i);
        std::string fname = FileUtil::join(path, fn);

#ifdef _WIN32
        f[i] = new FileWin;
#else
        f[i] = new FileLinux;
#endif

        bool result = f[i]->open(fname, mode);
        if (result) {
            count++;

            if (writeSize > 0) {
                f[i]->write(buf, writeSize);
            }
        }
        else {
            break;
        }
    }

    // Close and remove all files
    for (int i = 0; i < count; i++) {
        f[i]->close();
        if (remove) {
            unlink(f[i]->filename.c_str());
        }
        delete f[i];
    }
    delete[] f;

    if (buf)
        delete[] buf;

    return count;
}


std::string FileUtil::getTempPath()
{
    std::string path;

#ifdef _WIN32
    path = "C:\\Temp";
#else
    path = "/tmp";
#endif

    return path;
}


std::string FileUtil::getCurPath()
{
    std::string path;
    char buf[1024];

#ifdef _WIN32
    if (GetCurrentDirectoryA(sizeof(buf), buf) > 0)
        path = std::string(buf);
#else
    if (getcwd(buf, sizeof(buf)) != NULL)
        path = std::string(buf);
#endif

    return path;
}


std::string FileUtil::readFileBinary(const std::string& fname)
{
    std::ifstream f;
    f.open(fname, std::ios::binary);
    std::stringstream buffer;
    buffer << f.rdbuf();
    return buffer.str();
}


std::string FileUtil::readFileText(const std::string& fname)
{
    std::ifstream f;
    f.open(fname);
    std::stringstream buffer;
    buffer << f.rdbuf();
    return buffer.str();
}

//===========================================================================

bool FileBase::openAppendClose(const std::string& fname, const std::string& text)
{
    CritSectLocker locker(pCritSect);

    if (filename.size() == 0)
        filename = fname;

    if (useWriteQueue || thread) {        
        FileBuf* buf = new FileBuf((void*)text.c_str(), text.length());
        writeQueue.push_back(buf);
        return true;
    }

    bool result = open(fname, "w+");
    if (result) {
        setPos(0, SEEK_END);
        write(text.c_str(), text.length());
        close();
    }
    return result;
}


// Based on tcpflow - tcpip.cpp, function shift_file()
bool FileBase::shiftFile(int64_t pos, int64_t insertLen)
{
    PerfLog perf("shiftFile: " + filename + Asprintf("%lld", insertLen));

    size_t bufferSize = 1024*1024;
    FileBuf buffer(NULL, bufferSize);

    // Move data after offset up by inslen bytes
    size_t bytesToMove = getSize();
    size_t readEndOffset = bytesToMove;

    while (bytesToMove != 0) {
        size_t bytesThisTime = bytesToMove < bufferSize ? bytesToMove : bufferSize;

        size_t readOffset = readEndOffset - bytesThisTime;
        size_t writeOffset = readOffset + insertLen;

        if (read(buffer.buf, bytesThisTime, readOffset) != bytesThisTime)
            return false;

        if (write(buffer.buf, bytesThisTime, writeOffset) != bytesThisTime)
            return false;

        bytesToMove -= bytesThisTime;
    }

    return true;
}


int64_t FileBase::flushWriteQueue()
{
    CritSectLocker locker(pCritSect);
    int64_t written = 0;

    if (writeQueue.size() > 0) {

        // Open or create
        bool wasOpen = isOpen();
        if (!isOpen()) {
            open(filename, "w+");
        }

        // Write all data from queue
        if (isOpen()) {
            setPos(0, SEEK_END);
            for (auto iter : writeQueue) {
                written += write(iter->buf, iter->size);
                delete iter;
            }
            writeQueue.clear();

            // Close file if it was not open before
            if (!wasOpen)
                close();
        }

        // Failed to open file
        else {
            written = -1;
            writeQueue.clear();
        }
    }

    return written;
}

//===========================================================================
//                                 FileRaw
//===========================================================================

bool FileRaw::open(const std::string& fname, const std::string& mode)
{
    // Close if alrady open
    if (fd > -1)
        close();

    // Set file name
    filename = fname;

    // Read / write+create
    // O_EXCL - If O_CREATand O_EXCL are set, open() shall fail if the file exists.
    int flags = 0;

         if (mode == "r")  { flags = O_RDONLY; }
    else if (mode == "w")  { flags = O_RDWR | O_CREAT | O_TRUNC; }
    else if (mode == "a")  { flags = O_RDWR | O_CREAT; }
    else if (mode == "r+") { flags = O_RDWR | O_CREAT; }
    else if (mode == "w+") { flags = O_RDWR | O_CREAT; }
    else if (mode == "a+") { flags = O_RDWR | O_CREAT; }

#ifdef _WIN32
    // On Windows only (Linux is always binary)
    flags |= O_BINARY;

    // Access-Mode: No special meaning on Windows
    int acmode = _S_IREAD | _S_IWRITE;
#else
    // Access-Mode: Read/write permissions to all
    int acmode = S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH;
#endif

    fd = ::open(filename.c_str(), flags, acmode);
    setError(fd > -1 ? 0 : errno, "open");
    if (fd > -1) {
        openFilesCount++;

        // Seek to end of file
        if (mode.find("a") != std::string::npos)
            setPos(0, SEEK_END);
    }

    // Debug log
    if (systemConfig && systemConfig->flowLog)
        log(Asprintf("FileRaw::open: filename=%s, fd=%d, openFilesCount=%d", filename.c_str(), fd, openFilesCount));

    return (fd > -1);
}



// Close file, optionally delete existing file
bool FileRaw::close()
{
    if (fd > -1) {
        int ret = ::close(fd);
        setError(ret > -1 ? 0 : errno, "close");
        if (ret == 0) {
            fd = -1;
            openFilesCount--;
        }
    }

    // Debug log
    if (systemConfig && systemConfig->flowLog)
        log(Asprintf("FileRaw::close: filename=%s, fd=%d, openFilesCount=%d", filename.c_str(), fd, openFilesCount));

    return (fd == -1);
}



void FileRaw::setError(int _errorCode, const char* _msg)
{
    errorCode = _errorCode;
    if (_errorCode != 0) {
        mfLog(Asprintf("FileRaw::setError: %s - filename=%s, fd=%d, error=%d, openFilesCount=%d", _msg, filename.c_str(), fd, _errorCode, openFilesCount));
    }
}

//===========================================================================
//                                 FileFILE
//===========================================================================

// Open file, optionally delete existing file
bool FileFILE::open(const std::string& fname, const std::string& mode)
{
    if (fp)
        close();

    if (filename.size() == 0)
        filename = fname;

    fp = fopen(filename.c_str(), mode.c_str());

    return fp != NULL;
}


bool FileFILE::close()
{
    if (fp) {
        fclose(fp);
        fp = NULL;
    }
    return true;
}


int FileFILE::write(const void* buf, size_t size, int64_t pos)
{
    if (pos > -1)
        setPos(pos, SEEK_SET);

    return (int)fwrite(buf, 1, size, fp);
}


int FileFILE::read(void* buf, size_t size, int64_t pos)
{
    if (pos > -1)
        setPos(pos, SEEK_SET);

    return (int)fread(buf, 1, size, fp);
}


bool FileFILE::setPos(int64_t pos, int mode)
{
#ifdef _WIN32
    return _fseeki64(fp, pos, mode) == 0;
#else
    return fseeko64(fp, pos, mode) == 0;
#endif
}


int64_t FileFILE::getPos()
{
#ifdef _WIN32
    return _ftelli64(fp);
#else
    return ftello64(fp);
#endif
}


int64_t FileFILE::getSize()
{
    int64_t pos = getPos();
    setPos(0, SEEK_END);
    int64_t size = getPos();
    setPos(pos, SEEK_SET);
    return size;
}


bool FileFILE::flush()
{
    return fflush(fp) == 0;
}

//===========================================================================
//                                 FileWin
//===========================================================================
#ifdef _WIN32

// Open file, optionally delete existing file
bool FileWin::open(const std::string& fname, const std::string& mode)
{
    if (haveHandle())
        close();

    // Set new file name
    if (filename.empty())
        filename = fname;

    // Prepare flags
    DWORD accessFlags = (mode == "r") ? GENERIC_READ : GENERIC_WRITE | GENERIC_READ;
    DWORD createFlags = 0;

         if (mode == "r")   { createFlags = OPEN_EXISTING; }
    else if (mode == "w")   { createFlags = CREATE_ALWAYS; }
    else if (mode == "a")   { createFlags = OPEN_ALWAYS;   }
    else if (mode == "r+")  { createFlags = OPEN_EXISTING; }
    else if (mode == "w+")  { createFlags = OPEN_ALWAYS;   }
    else if (mode == "a+")  { createFlags = OPEN_ALWAYS;   }
    
    // Open/create
    handle = CreateFileA(filename.c_str(), accessFlags, FILE_SHARE_READ | FILE_SHARE_WRITE, NULL, createFlags, FILE_ATTRIBUTE_NORMAL, NULL);
    setError((handle != INVALID_HANDLE_VALUE) ? 0 : GetLastError(), "CreateFileA");

    // Seek to end of file
    if ((handle != INVALID_HANDLE_VALUE) && (mode.find("a") != std::string::npos))
        setPos(0, SEEK_END);

    return haveHandle();
}


bool FileWin::close()
{
    if (haveHandle()) {
        bool ret = CloseHandle(handle);
        setError(ret ? 0 : GetLastError(), "CloseHandle");
        handle = INVALID_HANDLE_VALUE;
    }
    return !haveHandle();
}


int FileWin::write(const void* buf, size_t size, int64_t pos)
{
    if (!haveHandle())
        return -1;

    if (pos > -1)
        setPos(pos, SEEK_SET);

    DWORD bytesWritten;
    if (WriteFile(handle, buf, (DWORD)size, &bytesWritten, NULL)) {
        setError(0);
    }
    else {
        setError(GetLastError(), "WriteFile");
        bytesWritten = 0;
    }

    return (int)bytesWritten;
}


int FileWin::read(void* buf, size_t size, int64_t pos)
{
    if (!haveHandle())
        return -1;

    if (pos > -1)
        setPos(pos, SEEK_SET);

    DWORD bytesRead;
    if (ReadFile(handle, buf, (DWORD)size, &bytesRead, NULL)) {
        setError(0);
    }
    else {
        setError(GetLastError(), "ReadFile");
        bytesRead = 0;
    }

    return (int)bytesRead;
}


bool FileWin::setPos(int64_t pos, int mode)
{
    bool result = false;
    if (haveHandle()) {
        LONG offsetLow  = pos & 0xFFFFFFFF;
        LONG offsetHigh = pos >> 32;
        DWORD ret;

        if (mode == SEEK_SET)
            ret = SetFilePointer(handle, offsetLow, &offsetHigh, FILE_BEGIN);
        else if (mode == SEEK_CUR)
            ret = SetFilePointer(handle, offsetLow, &offsetHigh, FILE_CURRENT);
        else if (mode == SEEK_END)
            ret = SetFilePointer(handle, offsetLow, &offsetHigh, FILE_END);

        setError((ret != INVALID_SET_FILE_POINTER) ? 0 : GetLastError(), "SetFilePointer");

        int64_t offset = (int64_t)ret | ((int64_t)offsetHigh << 32);
        result = (offset == pos);
    }

    return result;
}


int64_t FileWin::getPos()
{
    int64_t pos = -1;
    if (haveHandle()) {
        LONG offsetLow = 0, offsetHigh = 0;
        offsetLow = SetFilePointer(handle, offsetLow, &offsetHigh, FILE_CURRENT);
        pos = (int64_t)offsetLow | ((int64_t)offsetHigh << 32);
    }
    return pos;
}


int64_t FileWin::getSize()
{
    int64_t size = -1;
    if (haveHandle()) {
        DWORD low, high;
        low = ::GetFileSize(handle, &high);
        DWORD err = GetLastError();
        if (low == 0xFFFFFFFF && (err != NO_ERROR)) {
            setError(err, "GetFileSize");
            size = -2;
        }
        else {
            size = (((int64_t)high) << 32) | (int64_t)low;
        }
   }
   return size;
}


bool FileWin::flush()
{
    bool ret = false;
    if (haveHandle()) {
        ret = FlushFileBuffers(handle);
        setError(ret ? 0 : GetLastError(), "FlushFileBuffers");
    }
    return ret;
}


void FileWin::setError(DWORD _errorCode, const char* _msg)
{ 
    errorCode = (int)_errorCode;
    if (_errorCode != 0) {
        mfLog(Asprintf("FileWin::setError: %s - filename=%s, handle=%08X, error=%08X", _msg, filename.c_str(), handle, _errorCode));
    }
}

#endif
//===========================================================================
