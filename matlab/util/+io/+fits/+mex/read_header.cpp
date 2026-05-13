// Read only the FITS header from a given HDU.
// Usage: header = io.fits.mex.read_header(filename, hdu)
//   filename : path to .fits or .fz file
//   hdu      : 1-based HDU index; 0 means HDU 1 (primary)
//   header   : Nx3 cell array {keyword, value, comment}
//
// compiled with
// mex CXX=g++-9 read_header.cpp /usr/lib/x86_64-linux-gnu/libcfitsio.a /home/sasha/ExternalLib/bzip2-1.0.8/libbz2.a -lz -lcurl -lm

#include "mex.h"
#include "fitsio.h"
#include <vector>
#include <cstring>
#include <string>
#include <unordered_set>
#include <unordered_map>

static void checkStatus(int status)
{
    if (status) {
        fits_report_error(stderr, status);
        mexErrMsgIdAndTxt("fits:cfitsio", "CFITSIO error");
    }
}

static bool isFITSFZ(const char* filename)
{
    const char* ext = strrchr(filename, '.');
    return ext && strcmp(ext, ".fz") == 0;
}

static bool unquoteFitsString(const char* buf,
                               std::string& out,
                               bool& had_continuation)
{
    const char* v = buf;
    while (*v == ' ') v++;
    if (*v != '\'') return false;
    v++;

    std::string s;
    while (*v) {
        if (*v == '\'') {
            if (*(v+1) == '\'') { s += '\''; v += 2; }
            else                { v++; break; }
        } else {
            s += *v++;
        }
    }

    if (!s.empty() && s.back() == '&') {
        s.pop_back();
        had_continuation = true;
    } else {
        had_continuation = false;
        while (!s.empty() && s.back() == ' ') s.pop_back();
    }

    out = s;
    return true;
}

static mxArray* readHeaderCell(fitsfile* fptr)
{
    int status = 0, nkeys = 0;

    fits_get_hdrspace(fptr, &nkeys, NULL, &status);
    checkStatus(status);

    std::vector<std::string> v_key, v_comment;
    std::vector<mxArray*>    v_val;
    std::vector<std::string> v_strval;
    std::vector<bool>        v_is_str;

    char key[FLEN_KEYWORD], value_str[FLEN_VALUE], comment[FLEN_COMMENT];

    for (int i = 1; i <= nkeys; i++) {
        fits_read_keyn(fptr, i, key, value_str, comment, &status);
        checkStatus(status);

        if (strcmp(key, "CONTINUE") == 0 && !v_key.empty() && v_is_str.back()) {
            std::string fragment;
            bool more = false;
            if (unquoteFitsString(comment, fragment, more)) {
                v_strval.back() += fragment;
                mxDestroyArray(v_val.back());
                v_val.back() = mxCreateString(v_strval.back().c_str());
            }
        }
        else {
            if (strcmp(key, "COMMENT") == 0 || strcmp(key, "HISTORY") == 0) {
                v_key.push_back(key);
                v_val.push_back(mxCreateString(comment));
                v_comment.push_back("");
                v_is_str.push_back(false);
                v_strval.push_back("");
                continue;
            }

            mxArray* val = nullptr;
            int status2  = 0;

            bool force_string = (strcmp(key, "OBJECT") == 0);

            double dval;
            if (!force_string &&
                fits_read_key(fptr, TDOUBLE, key, &dval, NULL, &status2) == 0) {
                val = mxCreateDoubleScalar(dval);
                v_is_str.push_back(false);
                v_strval.push_back("");
            }
            else {
                status2 = 0;
                int lval;
                if (!force_string &&
                    fits_read_key(fptr, TLOGICAL, key, &lval, NULL, &status2) == 0) {
                    val = mxCreateLogicalScalar(lval);
                    v_is_str.push_back(false);
                    v_strval.push_back("");
                }
                else {
                    status2 = 0;
                    char sval[FLEN_VALUE];
                    if (fits_read_key(fptr, TSTRING, key, sval, NULL, &status2) == 0) {
                        std::string s(sval);
                        bool had_cont = (!s.empty() && s.back() == '&');
                        if (had_cont) s.pop_back();
                        val = mxCreateString(s.c_str());
                        v_is_str.push_back(true);
                        v_strval.push_back(s);
                    }
                    else {
                        status2 = 0;
                        val = mxCreateString("");
                        v_is_str.push_back(false);
                        v_strval.push_back("");
                    }
                }
            }

            v_key.push_back(key);
            v_comment.push_back(comment);
            v_val.push_back(val);
        }
    }

    int nrows = (int)v_key.size();
    mxArray* cell = mxCreateCellMatrix(nrows, 3);

    for (int r = 0; r < nrows; r++) {
        mxSetCell(cell, r,           mxCreateString(v_key[r].c_str()));
        mxSetCell(cell, r + nrows,   v_val[r]);
        mxSetCell(cell, r + 2*nrows, mxCreateString(v_comment[r].c_str()));
    }

    return cell;
}

static mxArray* cleanCompressedHeader(mxArray* hdr)
{
    mwSize n = mxGetM(hdr);

    std::unordered_set<std::string> drop = {
        "XTENSION","PCOUNT","GCOUNT","TFIELDS","TTYPE1","TFORM1",
        "ZIMAGE","ZTILE1","ZTILE2","ZCMPTYPE","ZNAME1","BLOCKSIZE",
        "ZVAL1","ZNAME2","BYTEPIX","ZVAL2","EXTNAME",
        "BITPIX","NAXIS","NAXIS1","NAXIS2"
    };

    std::unordered_map<std::string,std::string> rename = {
        {"ZSIMPLE","SIMPLE"},
        {"ZBITPIX","BITPIX"},
        {"ZNAXIS","NAXIS"},
        {"ZNAXIS1","NAXIS1"},
        {"ZNAXIS2","NAXIS2"},
        {"ZEXTEND","EXTEND"}
    };

    std::vector<int> keep;
    std::vector<std::string> newKeys;
    keep.reserve(n);
    newKeys.reserve(n);

    for (mwSize i = 0; i < n; i++) {
        mxArray* keyCell = mxGetCell(hdr, i);
        if (!keyCell) continue;

        char* keyC = mxArrayToString(keyCell);
        std::string key(keyC ? keyC : "");
        mxFree(keyC);

        if (drop.count(key)) continue;
        if (rename.count(key)) key = rename[key];

        keep.push_back(i);
        newKeys.push_back(key);
    }

    mwSize m = keep.size();
    mxArray* out = mxCreateCellMatrix(m, 3);

    for (mwSize j = 0; j < m; j++) {
        mwSize i = keep[j];

        char* keyC = mxArrayToString(mxGetCell(hdr, i));
        std::string key(keyC ? keyC : "");
        mxFree(keyC);

        if (rename.count(key)) key = rename[key];

        mxSetCell(out, j + 0*m, mxCreateString(key.c_str()));
        mxSetCell(out, j + 1*m, mxDuplicateArray(mxGetCell(hdr, i + 1*n)));
        mxSetCell(out, j + 2*m, mxDuplicateArray(mxGetCell(hdr, i + 2*n)));
    }

    return out;
}

void mexFunction(int nlhs, mxArray* plhs[],
                 int nrhs, const mxArray* prhs[])
{
    if (nrhs < 2)
        mexErrMsgTxt("Usage: header = io.fits.mex.read_header(filename, hdu)");

    char* filename = mxArrayToString(prhs[0]);
    bool compressed = isFITSFZ(filename);

    int hdu = (int)mxGetScalar(prhs[1]);
    if (hdu <= 0) hdu = 1;

    fitsfile* fptr;
    int status = 0, hdutype;

    fits_open_file(&fptr, filename, READONLY, &status);
    checkStatus(status);
    mxFree(filename);

    fits_movabs_hdu(fptr, hdu, &hdutype, &status);
    checkStatus(status);

    mxArray* hdr = readHeaderCell(fptr);

    fits_close_file(fptr, &status);
    checkStatus(status);

    plhs[0] = compressed ? cleanCompressedHeader(hdr) : hdr;
    if (compressed) mxDestroyArray(hdr);
}
