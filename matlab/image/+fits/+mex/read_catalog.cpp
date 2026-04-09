// read_catalog_mex.cpp
// Compile with: mex read_catalog_mex.cpp -lcfitsio
// after: sudo apt install libcfitsio-dev
//
// Usage:
//   [S, Header, hdu] = fits.mex.read_catalog(filename, hdu)
//   [S, Header, hdu] = fits.mex.read_catalog(filename, hdu, rows)
//   [S, Header, hdu] = fits.mex.read_catalog(filename, hdu, rows, cols, hdu_header)
//
// Arguments:
//   filename  - path to FITS file
//   hdu       - HDU number (1-based); pass 0 to auto-detect first table HDU
//   rows      - optional [R1 R2] row range, 1-based inclusive; [] = all rows
//   cols      - optional cell array of column names to read; {} = all columns
//   hdu_header- if we wish to read the header from a different HDU 
//
// Outputs:
//   S         - struct with one field per column (all double arrays)
//   Header    - Nx3 cell array {keyword, value, comment}
//   hdu       - HDU number actually used to read the catalog
//
// All numeric columns are read as double. String columns are skipped with a
// warning. Vector columns (repeat > 1) are flattened with a warning.
//
// Author: A.M. Krassilchtchikov, D. Kovaleva (2026 Apr)

#include "mex.h"
#include "fitsio.h"
#include <vector>
#include <string>
#include <cstring>
#include <cctype>
#include <cstdlib>
#include <limits>

// ---------------------------------------------------------------------------
// Error helper
// ---------------------------------------------------------------------------

static void checkStatus(int status)
{
    if (status) {
        fits_report_error(stderr, status);
        mexErrMsgIdAndTxt("fits:cfitsio", "CFITSIO error");
    }
}

// ---------------------------------------------------------------------------
// RAII guard for mxArrayToString
// ---------------------------------------------------------------------------

struct MxStringGuard {
    char* p;
    ~MxStringGuard() { if (p) mxFree(p); }
};

// ---------------------------------------------------------------------------
// Find first binary or ASCII table HDU (1-based)
// ---------------------------------------------------------------------------

static int find_table_hdu(fitsfile* fptr)
{
    int status = 0, nhdus, hdutype;
    fits_get_num_hdus(fptr, &nhdus, &status);
    checkStatus(status);

    for (int h = 1; h <= nhdus; h++) {
        fits_movabs_hdu(fptr, h, &hdutype, &status);
        checkStatus(status);
        if (hdutype == BINARY_TBL || hdutype == ASCII_TBL)
            return h;
    }
    return -1;
}

// ---------------------------------------------------------------------------
// Case-insensitive string comparison
// ---------------------------------------------------------------------------

static bool iequal(const std::string& a, const std::string& b)
{
    if (a.size() != b.size()) return false;
    for (size_t i = 0; i < a.size(); ++i)
        if (std::tolower((unsigned char)a[i]) != std::tolower((unsigned char)b[i]))
            return false;
    return true;
}

// ---------------------------------------------------------------------------
// Unquote a FITS string value from a raw char buffer.
// Handles '' -> ' escaping and strips the trailing '&' continuation marker.
// Returns the unquoted content and sets *had_continuation=true when '&' was
// present (meaning more fragments follow on CONTINUE lines).
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

    // Strip trailing '&' (continuation marker) or trailing spaces
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
// Parse a raw FITS value_str into a typed mxArray.
// If the value is a string, *is_string=true and *out_str holds the unquoted
// content (without trailing '&').
// ---------------------------------------------------------------------------

static mxArray* parseValue(const char* value_str,
                            bool* is_string, std::string* out_str,
                            bool* had_continuation)
{
    *is_string        = false;
    *had_continuation = false;

    const char* v = value_str;
    while (*v == ' ') v++;

    if (*v == '\0') {
        return mxCreateString("");
    }
    else if (*v == 'T' && (*(v+1) == '\0' || *(v+1) == ' ' || *(v+1) == '/')) {
        return mxCreateLogicalScalar(true);
    }
    else if (*v == 'F' && (*(v+1) == '\0' || *(v+1) == ' ' || *(v+1) == '/')) {
        return mxCreateLogicalScalar(false);
    }
    else if (*v == '\'') {
        std::string s;
        unquoteFitsString(v, s, *had_continuation);
        *is_string = true;
        *out_str   = s;
        return mxCreateString(s.c_str());
    }
    else {
        char* endptr;
        double dval = strtod(v, &endptr);
        if (endptr != v && (*endptr == '\0' || *endptr == ' ' || *endptr == '/'))
            return mxCreateDoubleScalar(dval);
        else
            return mxCreateString(value_str);
    }
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

static mxArray* readHeaderCell(fitsfile* fptr)
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
            // The continuation fragment is a quoted string in the comment field.
            // e.g. comment == " 'age_1.fits'"  or  " 'more&'"
            std::string fragment;
            bool more = false;
            if (unquoteFitsString(comment, fragment, more)) {
                v_strval.back() += fragment;
                mxDestroyArray(v_val.back());
                v_val.back() = mxCreateString(v_strval.back().c_str());
            }
            // If 'more' is true there will be another CONTINUE — loop handles it.
        }
        else {
            bool is_str = false, had_cont = false;
            std::string strval;
            mxArray* val = parseValue(value_str, &is_str, &strval, &had_cont);

            v_key.push_back(key);
            v_comment.push_back(comment);
            v_val.push_back(val);
            v_strval.push_back(is_str ? strval : "");
            v_is_str.push_back(is_str);
        }
    }

    int nrows = (int)v_key.size();
    mxArray* cell = mxCreateCellMatrix(nrows, 3);

    for (int r = 0; r < nrows; r++) {
        mxSetCell(cell, r,            mxCreateString(v_key[r].c_str()));
        mxSetCell(cell, r + nrows,    v_val[r]);
        mxSetCell(cell, r + 2*nrows,  mxCreateString(v_comment[r].c_str()));
    }

    return cell;
}

// ---------------------------------------------------------------------------
// mexFunction
// ---------------------------------------------------------------------------

void mexFunction(int nlhs, mxArray* plhs[],
                 int nrhs, const mxArray* prhs[])
{
    // ------------------------------------------------------------------
    // 1. Parse inputs
    // ------------------------------------------------------------------
    if (nrhs < 2)
        mexErrMsgTxt(
            "Usage: [S, Header, hdu] = fits.mex.read_catalog(filename, hdu [, rows [, cols [, hdu_header]]])");

    char* filename = mxArrayToString(prhs[0]);
    MxStringGuard fnGuard{filename};

    int hdu_req = (int)mxGetScalar(prhs[1]);

    // Optional row range [R1 R2]
    bool use_rowrange = false;
    long row1 = 1, row2 = 0;

    if (nrhs >= 3 && !mxIsEmpty(prhs[2])) {
        if (mxGetNumberOfElements(prhs[2]) != 2)
            mexErrMsgTxt("rows must be [] or [R1 R2] (1-based, inclusive)");
        const double* rv = mxGetPr(prhs[2]);
        row1 = (long)rv[0];
        row2 = (long)rv[1];
        use_rowrange = true;
    }

    // Optional column name filter — cell array of strings
    std::vector<std::string> wanted_cols;
    if (nrhs >= 4 && !mxIsEmpty(prhs[3])) {
        if (!mxIsCell(prhs[3]))
            mexErrMsgTxt("cols must be a cell array of column name strings");
        mwSize ncw = mxGetNumberOfElements(prhs[3]);
        for (mwSize k = 0; k < ncw; k++) {
            mxArray* el = mxGetCell(prhs[3], k);
            if (!mxIsChar(el))
                mexErrMsgTxt("Each element of cols must be a string");
            char* s = mxArrayToString(el);
            wanted_cols.emplace_back(s);
            mxFree(s);
        }
    }

    // Optional HDU number from which to read the header (-1 = same as table)
    int hdu_header_req = -1;
    if (nrhs >= 5 && !mxIsEmpty(prhs[4]))
        hdu_header_req = (int)mxGetScalar(prhs[4]);

    // ------------------------------------------------------------------
    // 2. Open file & move to the right HDU
    // ------------------------------------------------------------------
    fitsfile* fptr;
    int status = 0, hdutype;

    fits_open_file(&fptr, filename, READONLY, &status);
    checkStatus(status);

    int hdu = hdu_req;
    if (hdu == 0) {
        hdu = find_table_hdu(fptr);
        if (hdu < 0)
            mexErrMsgTxt("No table HDU found in this file");
    }

    fits_movabs_hdu(fptr, hdu, &hdutype, &status);
    checkStatus(status);

    if (hdutype != BINARY_TBL && hdutype != ASCII_TBL)
        mexErrMsgTxt("Requested HDU is not a table (BINARY_TBL or ASCII_TBL)");

    // ------------------------------------------------------------------
    // 3. Table geometry
    // ------------------------------------------------------------------
    int  ncols_total;
    long nrows_total;

    fits_get_num_cols(fptr, &ncols_total, &status); checkStatus(status);
    fits_get_num_rows(fptr, &nrows_total, &status); checkStatus(status);

    if (!use_rowrange) {
        row1 = 1;
        row2 = nrows_total;
    } else {
        if (row1 < 1)            row1 = 1;
        if (row2 > nrows_total)  row2 = nrows_total;
        if (row1 > row2)
            mexErrMsgTxt("Invalid row range: R1 > R2 (or both out of bounds)");
    }
    long nrows = row2 - row1 + 1;

    // ------------------------------------------------------------------
    // 4. Enumerate columns; filter; detect string/vector columns
    // ------------------------------------------------------------------
    struct ColInfo {
        int         colnum;   // 1-based FITS column number
        std::string name;
        long        repeat;   // element count per row
    };

    std::vector<ColInfo> cols;
    cols.reserve(ncols_total);

    for (int c = 1; c <= ncols_total; c++) {
        char ttype[FLEN_VALUE] = {0};
        int  typecode = 0;
        long repeat = 1, width = 0;

        fits_get_bcolparms(fptr, c,
                           ttype, NULL,
                           NULL, &repeat, NULL, NULL, NULL, NULL,
                           &status);
        if (status) { status = 0; continue; }

        fits_get_coltype(fptr, c, &typecode, &repeat, &width, &status);
        if (status) { status = 0; continue; }

        std::string name(ttype);

        // Apply column filter (case-insensitive)
        if (!wanted_cols.empty()) {
            bool found = false;
            for (const auto& w : wanted_cols)
                if (iequal(w, name)) { found = true; break; }
            if (!found) continue;
        }

        // Skip string columns
        int tc = abs(typecode);
        if (tc == TSTRING) {
            mexPrintf("Warning: skipping string column '%s'\n", ttype);
            continue;
        }

        // Warn about vector columns (repeat > 1)
        if (repeat > 1) {
            mexPrintf("Warning: column '%s' has repeat=%ld, flattening to %ld rows\n",
                      ttype, repeat, nrows * repeat);
        }

        ColInfo ci;
        ci.colnum = c;
        ci.name   = name;
        ci.repeat = repeat;
        cols.push_back(ci);
    }

    int ncols = (int)cols.size();
    if (ncols == 0)
        mexErrMsgTxt("No columns matched (or table has only string columns)");

    // ------------------------------------------------------------------
    // 5. Build output struct
    // ------------------------------------------------------------------
    std::vector<const char*> fieldnames(ncols);
    for (int i = 0; i < ncols; i++)
        fieldnames[i] = cols[i].name.c_str();

    mxArray* S = mxCreateStructMatrix(1, 1, ncols, fieldnames.data());

    // ------------------------------------------------------------------
    // 6. Read each column as double
    // ------------------------------------------------------------------   
    double nulval = std::numeric_limits<double>::quiet_NaN();

    for (int i = 0; i < ncols; i++) {
        const ColInfo& ci = cols[i];
        int anynul = 0;

        long nelems = nrows * ci.repeat;
        mwSize dims[2] = { (mwSize)nrows, (mwSize)ci.repeat };

        mxArray* arr = mxCreateNumericArray(2, dims, mxDOUBLE_CLASS, mxREAL);
        double* data = (double*)mxGetData(arr);

        fits_read_col(fptr, TDOUBLE, ci.colnum,
                      row1, 1, nelems,
                      &nulval, data, &anynul, &status);
        checkStatus(status);

        mxSetFieldByNumber(S, 0, i, arr);
    }

    // ------------------------------------------------------------------
    // 7. Read header — explicitly move to the correct HDU first because
    //    fits_read_col may leave fptr on a different HDU internally.
    // ------------------------------------------------------------------
    {
        int hdr_hdu = (hdu_header_req == -1) ? hdu        // default: table HDU
                    : (hdu_header_req ==  0) ? 1           // 0 = primary HDU
                    : hdu_header_req;
        fits_movabs_hdu(fptr, hdr_hdu, NULL, &status);
        checkStatus(status);
    }

    mxArray* Header = readHeaderCell(fptr);

    // ------------------------------------------------------------------
    // 8. Close file & assign outputs
    // ------------------------------------------------------------------
    fits_close_file(fptr, &status);
    checkStatus(status);

    plhs[0] = S;
    if (nlhs >= 2) plhs[1] = Header;
    if (nlhs >= 3) plhs[2] = mxCreateDoubleScalar((double)hdu);
}
