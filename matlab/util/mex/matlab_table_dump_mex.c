/*
 * matlab_table_dump_mex.c -- MEX helper for the matlab_table_dump MATLAB writer (C, classic matrix API).
 *
 * Builds in MATLAB (mex -R2018a) and in GNU Octave (mkoctfile --mex); see
 * matlab_table_dump.build_mex. All layout decisions (offsets, JSON metadata) are made in
 * M code (matlab_table_dump.write) so the M engine and this engine produce identical
 * bytes; this file only does the I/O that M cannot do well:
 *
 *   matlab_table_dump_mex('version')                              -> text
 *   n = matlab_table_dump_mex('write', path, bufs, offsets, meta, metaOffset, fsync)
 *         bufs:    cell of real numeric/logical arrays, written raw (no conversion,
 *                  no copy: straight from MATLAB's memory to the file)
 *         offsets: double vector, 64-byte aligned, increasing (one per buffer)
 *         meta:    uint8 vector, the JSON metadata
 *         fsync:   logical; flush to stable storage before returning
 *         returns the file size in bytes
 *   c = matlab_table_dump_mex('crc32', array)                     -> CRC-32 (zlib) of the array's bytes, as a double
 *   matlab_table_dump_mex('rename', src, dst)                     -> atomic rename; error if dst exists
 *   matlab_table_dump_mex('fsyncdir', dir)                        -> make a rename durable (no-op on Windows)
 *
 * Paths are UTF-8 (wide-character APIs on Windows).
 */
#include "mex.h"

#include <errno.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef _WIN32
#  define WIN32_LEAN_AND_MEAN
#  include <windows.h>
#  include <io.h>
#else
#  include <fcntl.h>
#  include <sys/stat.h>
#  include <unistd.h>
#endif

#define MATLAB_TABLE_DUMP_MEX_VERSION "matlab_table_dump_mex 1.1.0 (format 1)"
#define ALIGN 64

static char* get_text(const mxArray* a, const char* what) {
    char* s;
    if (!mxIsChar(a)) mexErrMsgIdAndTxt("matlab_table_dump:mex", "%s must be a character vector", what);
#ifdef MATLAB_TABLE_DUMP_OCTAVE
    s = mxArrayToString(a); /* Octave strings are UTF-8 */
#else
    s = mxArrayToUTF8String(a);
#endif
    if (!s) mexErrMsgIdAndTxt("matlab_table_dump:mex", "cannot read %s", what);
    return s;
}

#ifdef _WIN32
static wchar_t* widen(const char* utf8) {
    int n = MultiByteToWideChar(CP_UTF8, 0, utf8, -1, NULL, 0);
    wchar_t* w = (wchar_t*)mxMalloc(sizeof(wchar_t) * (size_t)n);
    MultiByteToWideChar(CP_UTF8, 0, utf8, -1, w, n);
    return w;
}
#endif

static FILE* open_write(const char* path) {
#ifdef _WIN32
    wchar_t* w = widen(path);
    FILE* f = _wfopen(w, L"wb");
    mxFree(w);
    return f;
#else
    return fopen(path, "wb");
#endif
}

static int remove_file(const char* path) {
#ifdef _WIN32
    wchar_t* w = widen(path);
    int r = _wremove(w);
    mxFree(w);
    return r;
#else
    return remove(path);
#endif
}

static int put(FILE* f, const void* p, uint64_t n) {
    return n == 0 || fwrite(p, 1, (size_t)n, f) == (size_t)n;
}

static void store_u64(uint8_t* p, uint64_t v) {
    int i;
    for (i = 0; i < 8; ++i) p[i] = (uint8_t)(v >> (8 * i));
}

static double scalar(const mxArray* a, const char* what) {
    if (!mxIsDouble(a) || mxGetNumberOfElements(a) != 1 || mxIsComplex(a))
        mexErrMsgIdAndTxt("matlab_table_dump:mex", "%s must be a real double scalar", what);
    return mxGetScalar(a);
}

static void cmd_write(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[]) {
    char* path;
    const mxArray *bufs, *offs, *meta;
    mwSize nb, i;
    const double* off;
    uint64_t meta_offset, pos, meta_n, file_n;
    int do_sync, ok = 1;
    FILE* f;
    static const uint8_t zeros[ALIGN] = {0};
    uint8_t header[64], trailer[32];

    if (nrhs != 7) mexErrMsgIdAndTxt("matlab_table_dump:mex", "usage: matlab_table_dump_mex('write', path, bufs, offsets, meta, metaOffset, fsync)");
    bufs = prhs[2];
    offs = prhs[3];
    meta = prhs[4];
    if (!mxIsCell(bufs)) mexErrMsgIdAndTxt("matlab_table_dump:mex", "bufs must be a cell array");
    nb = mxGetNumberOfElements(bufs);
    if (!mxIsDouble(offs) || mxIsComplex(offs) || (mwSize)mxGetNumberOfElements(offs) != nb)
        mexErrMsgIdAndTxt("matlab_table_dump:mex", "offsets must be a double vector with one entry per buffer");
    if (!mxIsUint8(meta)) mexErrMsgIdAndTxt("matlab_table_dump:mex", "meta must be uint8");
    meta_offset = (uint64_t)scalar(prhs[5], "metaOffset");
    do_sync = mxIsLogicalScalarTrue(prhs[6]);
    off = (const double*)mxGetData(offs);

    /* Check every buffer before creating the file. */
    pos = 64;
    for (i = 0; i < nb; ++i) {
        const mxArray* b = mxGetCell(bufs, i);
        uint64_t o = (uint64_t)off[i];
        if (!b || !(mxIsNumeric(b) || mxIsLogical(b)) || mxIsComplex(b) || mxIsSparse(b))
            mexErrMsgIdAndTxt("matlab_table_dump:mex", "buffer %d must be a real, full numeric or logical array", (int)i + 1);
        if ((double)o != off[i] || o % ALIGN != 0 || o < pos)
            mexErrMsgIdAndTxt("matlab_table_dump:mex", "offset of buffer %d is not 64-byte aligned and increasing", (int)i + 1);
        pos = o + (uint64_t)mxGetNumberOfElements(b) * (uint64_t)mxGetElementSize(b);
    }
    if (meta_offset != pos) mexErrMsgIdAndTxt("matlab_table_dump:mex", "metaOffset does not match the buffer layout");
    meta_n = (uint64_t)mxGetNumberOfElements(meta);
    file_n = meta_offset + meta_n + 32;

    path = get_text(prhs[1], "path");
    f = open_write(path);
    if (!f) {
        mxFree(path);
        mexErrMsgIdAndTxt("matlab_table_dump:io", "cannot create file (%s)", strerror(errno));
    }
    /* Unbuffered: large buffers go to the kernel in one write, without a stdio copy. */
    setvbuf(f, NULL, _IONBF, 0);

    memset(header, 0, sizeof header);
    memcpy(header, "MTABDUMP", 8);
    header[8] = 1; /* version 1, little-endian uint32; flags stay 0 */
    ok &= put(f, header, 64);
    pos = 64;
    for (i = 0; ok && i < nb; ++i) {
        const mxArray* b = mxGetCell(bufs, i);
        uint64_t o = (uint64_t)off[i];
        uint64_t n = (uint64_t)mxGetNumberOfElements(b) * (uint64_t)mxGetElementSize(b);
        if (o > pos) ok &= put(f, zeros, o - pos);
        ok &= put(f, mxGetData(b), n);
        pos = o + n;
    }
    if (ok) ok &= put(f, mxGetData(meta), meta_n);
    store_u64(trailer, meta_offset);
    store_u64(trailer + 8, meta_n);
    store_u64(trailer + 16, file_n);
    memcpy(trailer + 24, "MTABEND1", 8);
    if (ok) ok &= put(f, trailer, 32);
    if (ok && do_sync) {
        fflush(f);
#ifdef _WIN32
        ok &= FlushFileBuffers((HANDLE)_get_osfhandle(_fileno(f))) != 0;
#else
        ok &= fsync(fileno(f)) == 0;
#endif
    }
    if (fclose(f) != 0) ok = 0;
    if (!ok) {
        int e = errno;
        remove_file(path);
        mxFree(path);
        mexErrMsgIdAndTxt("matlab_table_dump:io", "write failed (%s); partial file removed", strerror(e));
    }
    mxFree(path);
    if (nlhs > 0) plhs[0] = mxCreateDoubleScalar((double)file_n);
}

/* CRC-32 (zlib polynomial, slicing-by-8) of an array's bytes; same value as Python zlib.crc32. */
static void cmd_crc32(mxArray* plhs[], int nrhs, const mxArray* prhs[]) {
    static uint32_t t[8][256];
    static int ready = 0;
    const mxArray* a;
    const uint8_t* p;
    uint64_t n;
    uint32_t crc = 0xFFFFFFFFu;
    if (nrhs != 2) mexErrMsgIdAndTxt("matlab_table_dump:mex", "usage: matlab_table_dump_mex('crc32', array)");
    a = prhs[1];
    if (!(mxIsNumeric(a) || mxIsLogical(a)) || mxIsComplex(a) || mxIsSparse(a))
        mexErrMsgIdAndTxt("matlab_table_dump:mex", "crc32 needs a real, full numeric or logical array");
    if (!ready) {
        uint32_t i;
        int s, k;
        for (i = 0; i < 256; ++i) {
            uint32_t c = i;
            for (k = 0; k < 8; ++k) c = (c >> 1) ^ (0xEDB88320u & (0u - (c & 1u)));
            t[0][i] = c;
        }
        for (i = 0; i < 256; ++i)
            for (s = 1; s < 8; ++s) t[s][i] = (t[s - 1][i] >> 8) ^ t[0][t[s - 1][i] & 0xFF];
        ready = 1;
    }
    p = (const uint8_t*)mxGetData(a);
    n = (uint64_t)mxGetNumberOfElements(a) * (uint64_t)mxGetElementSize(a);
    for (; n >= 8; n -= 8, p += 8) {
        uint32_t x = ((uint32_t)p[0] | (uint32_t)p[1] << 8 | (uint32_t)p[2] << 16 | (uint32_t)p[3] << 24) ^ crc;
        uint32_t y = (uint32_t)p[4] | (uint32_t)p[5] << 8 | (uint32_t)p[6] << 16 | (uint32_t)p[7] << 24;
        crc = t[7][x & 0xFF] ^ t[6][(x >> 8) & 0xFF] ^ t[5][(x >> 16) & 0xFF] ^ t[4][x >> 24] ^
              t[3][y & 0xFF] ^ t[2][(y >> 8) & 0xFF] ^ t[1][(y >> 16) & 0xFF] ^ t[0][y >> 24];
    }
    for (; n; --n, ++p) crc = (crc >> 8) ^ t[0][(crc ^ *p) & 0xFF];
    plhs[0] = mxCreateDoubleScalar((double)(crc ^ 0xFFFFFFFFu));
}

static void cmd_rename(int nrhs, const mxArray* prhs[]) {
    char *src, *dst;
    int ok;
    if (nrhs != 3) mexErrMsgIdAndTxt("matlab_table_dump:mex", "usage: matlab_table_dump_mex('rename', src, dst)");
    src = get_text(prhs[1], "src");
    dst = get_text(prhs[2], "dst");
#ifdef _WIN32
    {
        wchar_t* ws = widen(src);
        wchar_t* wd = widen(dst);
        ok = MoveFileExW(ws, wd, MOVEFILE_WRITE_THROUGH) != 0; /* fails if dst exists */
        mxFree(ws);
        mxFree(wd);
    }
#else
    {
        struct stat st;
        if (stat(dst, &st) == 0) {
            errno = EEXIST;
            ok = 0;
        } else {
            ok = rename(src, dst) == 0;
        }
    }
#endif
    if (!ok) {
        int e = errno;
        mxFree(src);
        mxFree(dst);
        mexErrMsgIdAndTxt("matlab_table_dump:io", "rename failed (%s)", strerror(e));
    }
    mxFree(src);
    mxFree(dst);
}

static void cmd_fsyncdir(int nrhs, const mxArray* prhs[]) {
    char* dir;
    if (nrhs != 2) mexErrMsgIdAndTxt("matlab_table_dump:mex", "usage: matlab_table_dump_mex('fsyncdir', dir)");
    dir = get_text(prhs[1], "dir");
#ifndef _WIN32
    {
        int fd = open(dir, O_RDONLY);
        if (fd >= 0) {
            fsync(fd);
            close(fd);
        }
    }
#endif
    mxFree(dir);
}

void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[]) {
    char* cmd;
    if (nrhs < 1) mexErrMsgIdAndTxt("matlab_table_dump:mex", "usage: matlab_table_dump_mex(command, ...)");
    cmd = get_text(prhs[0], "command");
    if (strcmp(cmd, "version") == 0) {
        mxFree(cmd);
        plhs[0] = mxCreateString(MATLAB_TABLE_DUMP_MEX_VERSION);
    } else if (strcmp(cmd, "write") == 0) {
        mxFree(cmd);
        cmd_write(nlhs, plhs, nrhs, prhs);
    } else if (strcmp(cmd, "crc32") == 0) {
        mxFree(cmd);
        cmd_crc32(plhs, nrhs, prhs);
    } else if (strcmp(cmd, "rename") == 0) {
        mxFree(cmd);
        cmd_rename(nrhs, prhs);
    } else if (strcmp(cmd, "fsyncdir") == 0) {
        mxFree(cmd);
        cmd_fsyncdir(nrhs, prhs);
    } else {
        mxFree(cmd);
        mexErrMsgIdAndTxt("matlab_table_dump:mex", "unknown command");
    }
}
