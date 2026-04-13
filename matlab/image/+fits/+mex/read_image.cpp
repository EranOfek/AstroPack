// compiled with 
// mex CXX=g++-9 read_image.cpp /usr/lib/x86_64-linux-gnu/libcfitsio.a /home/sasha/ExternalLib/bzip2-1.0.8/libbz2.a -lz -lcurl -lm
// after sudo apt install libcfitsio-dev  
#include "mex.h"
#include "fitsio.h"
#include <vector>
#include <cstring>
#include <string>
#include <cstdlib>

void checkStatus(int status)
{
    if (status) {
        fits_report_error(stderr, status);
        mexErrMsgIdAndTxt("fits:cfitsio", "CFITSIO error");
    }
}

mxClassID bitpix_to_mxclass(int bitpix)
{
    switch(bitpix) {
        case BYTE_IMG:     return mxUINT8_CLASS;
        case SHORT_IMG:    return mxINT16_CLASS;
        case LONG_IMG:     return mxINT32_CLASS;
        case LONGLONG_IMG: return mxINT64_CLASS;
        case FLOAT_IMG:    return mxSINGLE_CLASS;
        case DOUBLE_IMG:   return mxDOUBLE_CLASS;
        default: mexErrMsgTxt("Unsupported BITPIX"); return mxDOUBLE_CLASS;
    }
}

int bitpix_to_datatype(int bitpix)
{
    switch(bitpix) {
        case BYTE_IMG:     return TBYTE;
        case SHORT_IMG:    return TSHORT;
        case LONG_IMG:     return TINT;
        case LONGLONG_IMG: return TLONGLONG;
        case FLOAT_IMG:    return TFLOAT;
        case DOUBLE_IMG:   return TDOUBLE;
        default: mexErrMsgTxt("Unsupported BITPIX"); return TDOUBLE;
    }
}

int find_image_hdu(fitsfile* fptr)
{
    int status = 0, nhdus, hdutype;

    fits_get_num_hdus(fptr, &nhdus, &status);
    checkStatus(status);

    for (int h = 1; h <= nhdus; h++) {
        fits_movabs_hdu(fptr, h, &hdutype, &status);
        checkStatus(status);

        int naxis = 0;
        fits_get_img_dim(fptr, &naxis, &status);

        if (status == 0 && naxis > 0)
            return h;

        status = 0; // reset and continue
    }

    return -1;
}

// ---------------------------------------------------------------------------
// Unquote a FITS string value from a raw char buffer.
// Handles '' -> ' escaping and strips the trailing '&' continuation marker.
// Sets had_continuation=true when '&' was present (more fragments follow).
// Returns false if the buffer does not start with a quote (not a string).
// ---------------------------------------------------------------------------

static bool unquoteFitsString(const char* buf,
                               std::string& out,
                               bool& had_continuation)
{
    const char* v = buf;
    while (*v == ' ') v++;
    if (*v != '\'') return false;
    v++;  // skip opening quote

    std::string s;
    while (*v) {
        if (*v == '\'') {
            if (*(v+1) == '\'') { s += '\''; v += 2; }  // '' -> '
            else                { v++; break; }           // closing quote
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

// ---------------------------------------------------------------------------
// Read header as Nx3 cell array {keyword, value, comment}.
//
// Implements the FITS Long String Convention (OGIP 1.0):
//   - The initial keyword stores a string value ending with '&' in value_str.
//   - Each CONTINUE card carries its fragment in the *comment* field as seen
//     by fits_read_keyn (cfitsio places the quoted continuation string there),
//     and value_str is empty.
//   - CONTINUE rows are merged into the preceding row: no new row is emitted.
// ---------------------------------------------------------------------------

mxArray* readHeaderCell(fitsfile* fptr)
{
    int status = 0, nkeys = 0;

    fits_get_hdrspace(fptr, &nkeys, NULL, &status);
    checkStatus(status);

    // Collect into vectors first — CONTINUE lines reduce the final row count.
    std::vector<std::string> v_key, v_comment;
    std::vector<mxArray*>    v_val;
    std::vector<std::string> v_strval;   // accumulated string content per row
    std::vector<bool>        v_is_str;

    char key[FLEN_KEYWORD], value_str[FLEN_VALUE], comment[FLEN_COMMENT];

    for (int i = 1; i <= nkeys; i++) {
        fits_read_keyn(fptr, i, key, value_str, comment, &status);
        checkStatus(status);

        if (strcmp(key, "CONTINUE") == 0 && !v_key.empty() && v_is_str.back()) {
            // Fragment is a quoted string sitting in the comment field
            std::string fragment;
            bool more = false;
            if (unquoteFitsString(comment, fragment, more)) {
                v_strval.back() += fragment;
                mxDestroyArray(v_val.back());
                v_val.back() = mxCreateString(v_strval.back().c_str());
            }
        }
        else {
            // --- parse value_str into a typed mxArray ---
            mxArray* val = nullptr;
            int status2  = 0;

            // Try double
            double dval;
            if (fits_read_key(fptr, TDOUBLE, key, &dval, NULL, &status2) == 0) {
                val = mxCreateDoubleScalar(dval);
                v_is_str.push_back(false);
                v_strval.push_back("");
            }
            else {
                status2 = 0;
                // Try logical
                int lval;
                if (fits_read_key(fptr, TLOGICAL, key, &lval, NULL, &status2) == 0) {
                    val = mxCreateLogicalScalar(lval);
                    v_is_str.push_back(false);
                    v_strval.push_back("");
                }
                else {
                    status2 = 0;
                    // Try string — also check for '&' continuation marker
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
                        val = mxCreateString("");  // fallback
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

#include <unordered_set>
#include <unordered_map>

mxArray* cleanCompressedHeader(mxArray* hdr)
{
    mwSize n = mxGetM(hdr);

    // ---- keys to delete ----
    std::unordered_set<std::string> drop = {
        "XTENSION","PCOUNT","GCOUNT","TFIELDS","TTYPE1","TFORM1",
        "ZIMAGE","ZTILE1","ZTILE2","ZCMPTYPE","ZNAME1","BLOCKSIZE",
        "ZVAL1","ZNAME2","BYTEPIX","ZVAL2","EXTNAME",
        "BITPIX","NAXIS","NAXIS1","NAXIS2"
    };

    // ---- rename map ----
    std::unordered_map<std::string,std::string> rename = {
        {"ZSIMPLE","SIMPLE"},
        {"ZBITPIX","BITPIX"},
        {"ZNAXIS","NAXIS"},
        {"ZNAXIS1","NAXIS1"},
        {"ZNAXIS2","NAXIS2"},
        {"ZEXTEND","EXTEND"}
    };

    std::vector<int> keep;
    keep.reserve(n);

    std::vector<std::string> newKeys;
    newKeys.reserve(n);

    // ---- pass 1: decide what to keep ----
    for (mwSize i = 0; i < n; i++)
    {
        mxArray* keyCell = mxGetCell(hdr, i);
        if (!keyCell) continue;

        char* keyC = mxArrayToString(keyCell);
        std::string key(keyC ? keyC : "");
        mxFree(keyC);

        if (drop.count(key))
            continue;

        // rename if needed
        if (rename.count(key))
            key = rename[key];

        keep.push_back(i);
        newKeys.push_back(key);
    }

    // ---- build output ----
    mwSize m = keep.size();
    mxArray* out = mxCreateCellMatrix(m, 3);

    for (mwSize j = 0; j < m; j++)
    {
        mwSize i = keep[j];

        // KEY
        mxArray* oldKey = mxGetCell(hdr, i);
        char* keyC = mxArrayToString(oldKey);
        std::string key(keyC ? keyC : "");
        mxFree(keyC);

        if (rename.count(key))
            key = rename[key];

        mxSetCell(out, j + 0*m, mxCreateString(key.c_str()));
        // VALUE
        mxSetCell(out, j + 1*m, mxDuplicateArray(mxGetCell(hdr, i + 1*n)));
        // COMMENT
        mxSetCell(out, j + 2*m, mxDuplicateArray(mxGetCell(hdr, i + 2*n)));
    }

    return out;
}

/////////

void mexFunction(int nlhs, mxArray* plhs[],
                 int nrhs, const mxArray* prhs[])
{
    if (nrhs < 2)
        mexErrMsgTxt("Usage: img = fits_read_image(filename, hdu, [CCDSEC])");

    char* filename = mxArrayToString(prhs[0]);
    int hdu_req = (int)mxGetScalar(prhs[1]);

    fitsfile* fptr;
    int status = 0, hdutype;

    fits_open_file(&fptr, filename, READONLY, &status);
    checkStatus(status);

    int hdu = hdu_req;

    if (hdu == 0) {
        hdu = find_image_hdu(fptr);
        if (hdu < 0)
            mexErrMsgTxt("No image HDU found");
    }

    fits_movabs_hdu(fptr, hdu, &hdutype, &status);
    checkStatus(status);
    
    // Get image parameters (robust for .fz)
    int bitpix, naxis;
    long naxes[10];

    fits_get_img_type(fptr, &bitpix, &status); checkStatus(status);
    fits_get_img_dim(fptr, &naxis, &status);   checkStatus(status);
    fits_get_img_size(fptr, 10, naxes, &status); checkStatus(status);

    if (naxis == 0)
        mexErrMsgTxt("The requested HDU has no image");
        
    int anynul = 0;
    
    // treat CCDSEC
    
    bool use_subset = false;
    
    if (nrhs >= 3) {
        if (!mxIsEmpty(prhs[2])) {
            if (mxGetNumberOfElements(prhs[2]) != 4)
                mexErrMsgTxt("CCDSEC must be [] or [X1 X2 Y1 Y2]");
            use_subset = true;
        }
    }
    
    std::vector<long> fpixel(naxis, 1);
    std::vector<long> lpixel(naxis);
    std::vector<long> inc(naxis, 1);
    
    for (int i = 0; i < naxis; i++)
        lpixel[i] = naxes[i];
    
    if (use_subset) {
        if (naxis < 2)
            mexErrMsgTxt("CCDSEC [X1 X2 Y1 Y2] requires at least 2D image");
        
        double* s = mxGetPr(prhs[2]);
        
        long X1 = (long)s[0];
        long X2 = (long)s[1];
        long Y1 = (long)s[2];
        long Y2 = (long)s[3];
        
        // Bounds check
        if (X1 < 1 || X2 > naxes[0] || X1 > X2)
            mexErrMsgTxt("Invalid X range");
        
        if (Y1 < 1 || Y2 > naxes[1] || Y1 > Y2)
            mexErrMsgTxt("Invalid Y range");
        
        fpixel[0] = X1;
        lpixel[0] = X2;
        
        fpixel[1] = Y1;
        lpixel[1] = Y2;
        
        // higher dimensions remain full range
    }
    
    std::vector<mwSize> dims(naxis);
    long nelements = 1;
    
    for (int i = 0; i < naxis; i++) {
        dims[i] = (mwSize)(lpixel[i] - fpixel[i] + 1);
        nelements *= dims[i];
    }
    
    // -------- Attempt native read --------
    mxArray* out = nullptr;
    void* data = nullptr;

    bool native_ok = true;

    // this is a separate case of uint16 coded as int16 and BZERO=32768.0
    if (bitpix == SHORT_IMG) {
        double bzero = 0.0;
        fits_read_key(fptr, TDOUBLE, "BZERO", &bzero, NULL, &status);
        if (status) status = 0;
        
        if (bzero == 32768.0) {
            // unsigned 16-bit path
            mxArray* out = mxCreateNumericArray(naxis, dims.data(), mxUINT16_CLASS, mxREAL);
            unsigned short* data = (unsigned short*)mxGetData(out);
                        
            fits_read_subset(fptr, TUSHORT,
                    fpixel.data(),
                    lpixel.data(),
                    inc.data(),
                    NULL,
                    data,
                    &anynul,
                    &status);
            checkStatus(status);
            
            // Output 2: header (optional)
            if (nlhs >= 2) {
                mxArray* rawheader = readHeaderCell(fptr);
                plhs[1] = cleanCompressedHeader(rawheader);
//                 plhs[1] = readHeaderCell(fptr);
            }            
            // Output 3: HDU number used (optional)
            if (nlhs >= 3) {
                plhs[2] = mxCreateDoubleScalar((double)hdu);
            }
            
            fits_close_file(fptr, &status);
            checkStatus(status);
            
            plhs[0] = out;
            return;
        }
    }

    try {
        mxClassID classid = bitpix_to_mxclass(bitpix);
        int datatype = bitpix_to_datatype(bitpix);

        out = mxCreateNumericArray(naxis, dims.data(), classid, mxREAL);
        data = mxGetData(out);

        int status_try = 0;

        fits_read_subset(fptr, datatype,
                fpixel.data(),
                lpixel.data(),
                inc.data(),
                NULL,
                data,
                &anynul,
                &status_try);
        checkStatus(status_try);

        if (status_try != 0) {
            native_ok = false;
            status = 0; // reset
        }

    } catch (...) {
        native_ok = false;
        status = 0;
    }

    // -------- Fallback to double --------
    if (!native_ok) {
        mexPrintf("Native FITS format not found, falling back to double\n");

        out = mxCreateNumericArray(naxis, dims.data(), mxDOUBLE_CLASS, mxREAL);
        double* data = mxGetPr(out);

        fits_read_subset(fptr, TDOUBLE,
                fpixel.data(),
                lpixel.data(),
                inc.data(),
                NULL,
                data,
                &anynul,
                &status);

        checkStatus(status);
    }
    
    // Output 2: header (optional)
    if (nlhs >= 2) {
        plhs[1] = readHeaderCell(fptr);
    }    
    // Output 3: HDU number used (optional)
    if (nlhs >= 3) {
        plhs[2] = mxCreateDoubleScalar((double)hdu);
    }

    fits_close_file(fptr, &status);
    checkStatus(status);

    plhs[0] = out;
}