// read_catalog_batch.cpp -- load MANY LAST proc_Cat FITS binary tables in ONE mex call, multi-threaded.
//
// Build (no cfitsio needed):
//   mex -R2018a CXXFLAGS='$CXXFLAGS -O3 -std=c++17 -pthread' LDFLAGS='$LDFLAGS -pthread' read_catalog_batch.cpp
//
// Usage:
//   [C, ColNames, ColUnits, Status, SchemaId] = read_catalog_batch(Files, NThreads, Prefetch)
//
//   Files    - cellstr (N x 1 or 1 x N) of FITS paths
//   NThreads - worker threads (default 16)
//   Prefetch - 1 (default): posix_fadvise(WILLNEED) on all files up front so the kernel/RAID
//              sees one deep, sortable I/O queue; 0: none
//   C        - N x 1 cell, each a [Nrows x Ncols] double matrix (column-major, i.e. exactly what
//              table2array(struct2table(io.fits.mex.read_catalog(...))) gives for all-double tables)
//   ColNames - G x 1 cell; ColNames{g} is a 1 x Ncols cellstr (TTYPEn) for schema group g
//   ColUnits - same shape, TUNITn ('' when absent)
//   Status   - N x 1 double. 0 = ok; otherwise re-read that file with io.fits.mex.read_catalog:
//              1 cannot open/read, 2 no BINTABLE, 3 table not all '1D' doubles; C{i} is [] then
//   SchemaId - N x 1 double, group g (1-based, order of first appearance) of file i; 0 if Status ~= 0.
//              Files in one group share column names/units (LAST files can carry 43 or 40 columns...)
//
// Only the first BINTABLE is read (header + data bytes); trailing HDUs are never touched.
// Author: benchmark prototype (2026-10)

#include "mex.h"

#include <fcntl.h>
#include <unistd.h>
#include <sys/stat.h>

#include <algorithm>
#include <atomic>
#include <cstdint>
#include <cstring>
#include <string>
#include <thread>
#include <vector>

namespace {

constexpr size_t BLOCK = 2880;

struct FileJob {
    std::string path;
    int status = 0;
    std::vector<char> buf;        // header + table bytes (offset 0 = file start)
    size_t dataOff = 0;
    long nrows = 0, ncols = 0;
    uint64_t schemaHash = 0;
    std::vector<std::string> names, units;   // filled only if requested (first file) -- see below
    mxArray* out = nullptr;
};

bool parseIntCard(const char* hdr, size_t len, const char* key, long long& val) {
    // key padded to 8 chars followed by '=' at 80-byte aligned positions
    char pat[9];
    std::snprintf(pat, sizeof pat, "%-8s", key);
    for (size_t i = 0; i + 80 <= len; i += 80) {
        if (std::memcmp(hdr + i, pat, 8) == 0 && hdr[i + 8] == '=') {
            char tmp[71];
            std::memcpy(tmp, hdr + i + 10, 70);
            tmp[70] = 0;
            char* e = nullptr;
            val = std::strtoll(tmp, &e, 10);
            return e != tmp;
        }
    }
    return false;
}

// Returns offset just past the header's END card block, 0 if END not within [off, len)
size_t headerEnd(const char* b, size_t len, size_t off) {
    while (off + BLOCK <= len) {
        for (size_t i = 0; i < BLOCK; i += 80)
            if (std::memcmp(b + off + i, "END     ", 8) == 0) return off + BLOCK;
        off += BLOCK;
    }
    return 0;
}

bool preadFull(int fd, char* dst, size_t n, off_t off) {
    size_t done = 0;
    while (done < n) {
        ssize_t r = pread(fd, dst + done, n - done, off + (off_t)done);
        if (r <= 0) return false;
        done += (size_t)r;
    }
    return true;
}

uint64_t fnv(uint64_t h, const char* p, size_t n) {
    for (size_t i = 0; i < n; ++i) { h ^= (unsigned char)p[i]; h *= 1099511628211ULL; }
    return h;
}

// Phase A: read header + table bytes of one file into job.buf
void loadFile(FileJob& j) {
    int fd = open(j.path.c_str(), O_RDONLY);
    if (fd < 0) { j.status = 1; return; }
    struct stat st;
    if (fstat(fd, &st) != 0) { close(fd); j.status = 1; return; }
    size_t fsize = (size_t)st.st_size;

    size_t have = std::min<size_t>(fsize, 16 * BLOCK);          // 46 kB covers primary + a typical table header
    j.buf.resize(have);
    if (!preadFull(fd, j.buf.data(), have, 0)) { close(fd); j.status = 1; return; }

    size_t off = 0;
    for (;;) {                                                     // walk HDUs until the first BINTABLE
        size_t end = headerEnd(j.buf.data(), have, off);
        while (end == 0) {                                         // header longer than what we hold
            if (have >= fsize) { close(fd); j.status = 2; return; }
            size_t more = std::min<size_t>(fsize - have, 16 * BLOCK);
            j.buf.resize(have + more);
            if (!preadFull(fd, j.buf.data() + have, more, (off_t)have)) { close(fd); j.status = 1; return; }
            have += more;
            end = headerEnd(j.buf.data(), have, off);
        }
        const char* hdr = j.buf.data() + off;
        size_t hlen = end - off;
        if (std::memcmp(hdr, "XTENSION= 'BINTABLE'", 20) == 0) {
            long long n1, n2, tf;
            if (!parseIntCard(hdr, hlen, "NAXIS1", n1) || !parseIntCard(hdr, hlen, "NAXIS2", n2) ||
                !parseIntCard(hdr, hlen, "TFIELDS", tf)) { close(fd); j.status = 2; return; }
            // all columns must be 1D (double); count TFORMn cards equal to '1D      '
            long long nD = 0;
            uint64_t h = 1469598103934665603ULL;
            for (size_t i = 0; i + 80 <= hlen; i += 80) {
                const char* c = hdr + i;
                if (std::memcmp(c, "TFORM", 5) == 0 && c[8] == '=' && std::memcmp(c + 10, "'1D      '", 10) == 0) ++nD;
                if (std::memcmp(c, "TTYPE", 5) == 0 || std::memcmp(c, "TUNIT", 5) == 0) h = fnv(h, c, 80);
            }
            if (n1 != 8 * tf || nD != tf) { close(fd); j.status = 3; return; }
            j.nrows = (long)n2; j.ncols = (long)tf; j.schemaHash = h;
            j.dataOff = end;
            size_t need = end + (size_t)n1 * (size_t)n2;
            if (need > fsize) { close(fd); j.status = 1; return; }
            if (need > have) {
                j.buf.resize(need);
                if (!preadFull(fd, j.buf.data() + have, need - have, (off_t)have)) { close(fd); j.status = 1; return; }
            }
            close(fd);
            return;
        }
        // skip this HDU's data (padded to 2880) and continue
        long long nax = 0, bp = 0, n = 1;
        parseIntCard(hdr, hlen, "NAXIS", nax);
        parseIntCard(hdr, hlen, "BITPIX", bp);
        for (long long k = 1; k <= nax; ++k) {
            char key[9]; std::snprintf(key, sizeof key, "NAXIS%lld", k);
            long long v = 0; parseIntCard(hdr, hlen, key, v); n *= v;
        }
        size_t dbytes = nax == 0 ? 0 : (size_t)(n * (bp < 0 ? -bp : bp) / 8);
        dbytes = (dbytes + BLOCK - 1) / BLOCK * BLOCK;
        off = end + dbytes;
        if (off >= fsize) { close(fd); j.status = 2; return; }
        if (off + 16 * BLOCK > have) {                              // make sure next header is in memory
            size_t want = std::min<size_t>(fsize, off + 16 * BLOCK);
            j.buf.resize(want);
            if (!preadFull(fd, j.buf.data() + have, want - have, (off_t)have)) { close(fd); j.status = 1; return; }
            have = want;
        }
    }
}

// Extract TTYPEn / TUNITn text for column k (1-based) from the header preceding dataOff
std::string cardString(const char* hdr, size_t hlen, const char* prefix, int k) {
    char key[9]; std::snprintf(key, sizeof key, "%s%-*d", prefix, (int)(8 - std::strlen(prefix)), k);
    for (size_t i = 0; i + 80 <= hlen; i += 80) {
        if (std::memcmp(hdr + i, key, 8) == 0 && hdr[i + 8] == '=') {
            const char* v = hdr + i + 10;                           // value field: 'text    ' / comment
            while (v < hdr + i + 80 && *v == ' ') ++v;
            if (*v != '\'') return std::string();
            ++v;
            std::string s;
            while (v < hdr + i + 80) {
                if (*v == '\'') { if (v + 1 < hdr + i + 80 && v[1] == '\'') { s += '\''; v += 2; continue; } break; }
                s += *v++;
            }
            while (!s.empty() && s.back() == ' ') s.pop_back();
            return s;
        }
    }
    return std::string();
}

// Phase C: big-endian row-major -> native column-major
void convert(const FileJob& j) {
    const long nr = j.nrows, nc = j.ncols;
    double* dst = mxGetDoubles(j.out);
    const uint64_t* src = reinterpret_cast<const uint64_t*>(j.buf.data() + j.dataOff);   // dataOff is 2880-aligned
    constexpr long TB = 64;                                       // row tile for cache friendliness
    for (long r0 = 0; r0 < nr; r0 += TB) {
        long r1 = std::min(nr, r0 + TB);
        for (long c = 0; c < nc; ++c) {
            uint64_t* d = reinterpret_cast<uint64_t*>(dst + (size_t)c * nr);
            for (long r = r0; r < r1; ++r) d[r] = __builtin_bswap64(src[(size_t)r * nc + c]);
        }
    }
}

template <class F>
void parallelFor(size_t n, int nthreads, F fn) {
    std::atomic<size_t> next{0};
    auto work = [&]() { for (size_t i; (i = next.fetch_add(1)) < n;) fn(i); };
    nthreads = (int)std::max<size_t>(1, std::min<size_t>((size_t)nthreads, n));
    std::vector<std::thread> th;
    for (int t = 1; t < nthreads; ++t) th.emplace_back(work);
    work();
    for (auto& t : th) t.join();
}

}  // namespace

void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[]) {
    if (nrhs < 1 || !mxIsCell(prhs[0])) mexErrMsgIdAndTxt("read_catalog_batch:args", "first argument must be a cellstr of file names");
    int nthreads = nrhs >= 2 && !mxIsEmpty(prhs[1]) ? (int)mxGetScalar(prhs[1]) : 16;
    int prefetch = nrhs >= 3 && !mxIsEmpty(prhs[2]) ? (int)mxGetScalar(prhs[2]) : 1;
    const size_t N = mxGetNumberOfElements(prhs[0]);

    std::vector<FileJob> jobs(N);
    for (size_t i = 0; i < N; ++i) {
        const mxArray* c = mxGetCell(prhs[0], i);
        if (!mxIsChar(c)) mexErrMsgIdAndTxt("read_catalog_batch:args", "element %zu is not a char vector", i + 1);
        char* s = mxArrayToString(c);
        jobs[i].path = s;
        mxFree(s);
    }

    if (prefetch)   // one deep readahead queue; open() itself is also a cold-metadata seek, so parallelise it
        parallelFor(N, nthreads, [&](size_t i) {
            int fd = open(jobs[i].path.c_str(), O_RDONLY);
            if (fd >= 0) { posix_fadvise(fd, 0, 0, POSIX_FADV_WILLNEED); close(fd); }
        });

    parallelFor(N, nthreads, [&](size_t i) { loadFile(jobs[i]); });

    // group files by schema (TTYPE/TUNIT cards hash + column count); names/units come from each group's first file
    std::vector<uint64_t> groupHash;
    std::vector<std::vector<std::string>> gNames, gUnits;
    std::vector<double> schemaId(N, 0.0);
    for (size_t i = 0; i < N; ++i) {
        FileJob& f = jobs[i];
        if (f.status != 0) continue;
        uint64_t key = f.schemaHash ^ ((uint64_t)f.ncols * 0x9E3779B97F4A7C15ULL);
        size_t g = 0;
        while (g < groupHash.size() && groupHash[g] != key) ++g;
        if (g == groupHash.size()) {
            groupHash.push_back(key);
            gNames.emplace_back(); gUnits.emplace_back();
            for (long k = 1; k <= f.ncols; ++k) {
                gNames[g].push_back(cardString(f.buf.data(), f.dataOff, "TTYPE", (int)k));
                gUnits[g].push_back(cardString(f.buf.data(), f.dataOff, "TUNIT", (int)k));
            }
        }
        schemaId[i] = (double)(g + 1);
    }

    // allocate outputs on the main thread (mx* API is not thread safe)
    plhs[0] = mxCreateCellMatrix(N, 1);
    for (size_t i = 0; i < N; ++i) {
        if (jobs[i].status == 0) {
            jobs[i].out = mxCreateUninitNumericMatrix((size_t)jobs[i].nrows, (size_t)jobs[i].ncols, mxDOUBLE_CLASS, mxREAL);
            mxSetCell(plhs[0], i, jobs[i].out);
        } else {
            mxSetCell(plhs[0], i, mxCreateDoubleMatrix(0, 0, mxREAL));
        }
    }

    parallelFor(N, nthreads, [&](size_t i) { if (jobs[i].status == 0) { convert(jobs[i]); std::vector<char>().swap(jobs[i].buf); } });

    auto cellOfRows = [](const std::vector<std::vector<std::string>>& v) {
        mxArray* out = mxCreateCellMatrix(v.size(), 1);
        for (size_t g = 0; g < v.size(); ++g) {
            mxArray* row = mxCreateCellMatrix(1, v[g].size());
            for (size_t k = 0; k < v[g].size(); ++k) mxSetCell(row, k, mxCreateString(v[g][k].c_str()));
            mxSetCell(out, g, row);
        }
        return out;
    };
    if (nlhs > 1) plhs[1] = cellOfRows(gNames);
    if (nlhs > 2) plhs[2] = cellOfRows(gUnits);
    if (nlhs > 3) {
        plhs[3] = mxCreateDoubleMatrix(N, 1, mxREAL);
        double* st = mxGetDoubles(plhs[3]);
        for (size_t i = 0; i < N; ++i) st[i] = jobs[i].status;
    }
    if (nlhs > 4) {
        plhs[4] = mxCreateDoubleMatrix(N, 1, mxREAL);
        std::memcpy(mxGetDoubles(plhs[4]), schemaId.data(), N * sizeof(double));
    }
}
