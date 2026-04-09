// Compile with: mex read_catalog.cpp -lcfitsio
// after: sudo apt install libcfitsio-dev
//
// Usage:
//   [cat, colnames, units, hdu] = read_catalog(filename, hdu)
//   [cat, colnames, units, hdu] = read_catalog(filename, hdu, rows)
//   [cat, colnames, units, hdu] = read_catalog(filename, hdu, rows, cols)
//
// Arguments:
//   filename  - path to FITS file
//   hdu       - HDU number (1-based); pass 0 to auto-detect first table HDU
//   rows      - optional [R1 R2] row range, 1-based inclusive; [] = all rows
//   cols      - optional cell array of column names to read; {} = all columns
//
// Outputs:
//   cat       - struct with one field per column (typed arrays)
//   colnames  - Nx1 cell array of column names
//   units     - Nx1 cell array of column units
//   hdu       - HDU number actually used

#include "mex.h"
#include "fitsio.h"
#include <vector>
#include <string>
#include <cstring>
#include <algorithm>
#include <cctype>

// --------------------------------------------------------------------------
// Error helpers
// --------------------------------------------------------------------------

static void checkStatus(int status)
{
    if (status) {
        fits_report_error(stderr, status);
        mexErrMsgIdAndTxt("fits:cfitsio", "CFITSIO error");
    }
}

// --------------------------------------------------------------------------
// Find first binary or ASCII table HDU (1-based)
// --------------------------------------------------------------------------

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

// --------------------------------------------------------------------------
// Convert FITS typecode to a MATLAB class and a cfitsio read datatype.
// Returns false if the column should be read as a string.
// --------------------------------------------------------------------------

struct ColType {
    mxClassID   mxclass;   // mxUNKNOWN_CLASS = string column
    int         cftype;    // cfitsio TSTRING / TINT etc.
    bool        is_string;
    bool        is_logical;
};

static ColType resolve_coltype(int typecode)
{
    // typecode is the raw value from fits_get_coltype; strip the VARIABLE flag
    int tc = abs(typecode);

    ColType ct;
    ct.is_string  = false;
    ct.is_logical = false;

    switch (tc) {
        case TBIT:
        case TLOGICAL:
            ct.mxclass  = mxLOGICAL_CLASS;
            ct.cftype   = TLOGICAL;
            ct.is_logical = true;
            break;
        case TBYTE:
            ct.mxclass = mxUINT8_CLASS;
            ct.cftype  = TBYTE;
            break;
        case TSBYTE:
            ct.mxclass = mxINT8_CLASS;
            ct.cftype  = TSBYTE;
            break;
        case TSHORT:
            ct.mxclass = mxINT16_CLASS;
            ct.cftype  = TSHORT;
            break;
        case TUSHORT:
            ct.mxclass = mxUINT16_CLASS;
            ct.cftype  = TUSHORT;
            break;
        case TINT:
        case TLONG:
            ct.mxclass = mxINT32_CLASS;
            ct.cftype  = TINT;
            break;
        case TUINT:
        case TULONG:
            ct.mxclass = mxUINT32_CLASS;
            ct.cftype  = TUINT;
            break;
        case TLONGLONG:
            ct.mxclass = mxINT64_CLASS;
            ct.cftype  = TLONGLONG;
            break;
        case TULONGLONG:
            ct.mxclass = mxUINT64_CLASS;
            ct.cftype  = TULONGLONG;
            break;
        case TFLOAT:
            ct.mxclass = mxSINGLE_CLASS;
            ct.cftype  = TFLOAT;
            break;
        case TDOUBLE:
        case TCOMPLEX:    // fall through – read real part as double
        case TDBLCOMPLEX:
            ct.mxclass = mxDOUBLE_CLASS;
            ct.cftype  = TDOUBLE;
            break;
        case TSTRING:
        default:
            ct.mxclass  = mxUNKNOWN_CLASS;  // sentinel for string columns
            ct.cftype   = TSTRING;
            ct.is_string = true;
            break;
    }
    return ct;
}

// --------------------------------------------------------------------------
// Case-insensitive string comparison helper
// --------------------------------------------------------------------------

static bool iequal(const std::string& a, const std::string& b)
{
    if (a.size() != b.size()) return false;
    for (size_t i = 0; i < a.size(); ++i)
        if (std::tolower((unsigned char)a[i]) != std::tolower((unsigned char)b[i]))
            return false;
    return true;
}

// --------------------------------------------------------------------------
// mexFunction
// --------------------------------------------------------------------------

void mexFunction(int nlhs, mxArray* plhs[],
                 int nrhs, const mxArray* prhs[])
{
    // ------------------------------------------------------------------
    // 1.  Parse inputs
    // ------------------------------------------------------------------
    if (nrhs < 2)
        mexErrMsgTxt(
            "Usage: [cat,colnames,units,hdu] = read_catalog(filename, hdu [, rows [, cols]])");

    char* filename = mxArrayToString(prhs[0]);
    int   hdu_req  = (int)mxGetScalar(prhs[1]);

    // Optional row range [R1 R2]
    bool  use_rowrange = false;
    long  row1 = 1, row2 = 0;   // row2 filled after we know nrows

    if (nrhs >= 3 && !mxIsEmpty(prhs[2])) {
        if (mxGetNumberOfElements(prhs[2]) != 2)
            mexErrMsgTxt("rows must be [] or [R1 R2] (1-based, inclusive)");
        const double* rv = mxGetPr(prhs[2]);
        row1 = (long)rv[0];
        row2 = (long)rv[1];
        use_rowrange = true;
    }

    // Optional column name filter – cell array of strings
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

    // ------------------------------------------------------------------
    // 2.  Open file & move to the right HDU
    // ------------------------------------------------------------------
    fitsfile* fptr;
    int status = 0, hdutype;

    fits_open_file(&fptr, filename, READONLY, &status);
    checkStatus(status);
    mxFree(filename);

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
    // 3.  Table geometry
    // ------------------------------------------------------------------
    int  ncols_total;
    long nrows_total;

    fits_get_num_cols(fptr, &ncols_total, &status); checkStatus(status);
    fits_get_num_rows(fptr, &nrows_total, &status); checkStatus(status);

    // Clamp row range
    if (!use_rowrange) {
        row1 = 1;
        row2 = nrows_total;
    } else {
        if (row1 < 1)          row1 = 1;
        if (row2 > nrows_total) row2 = nrows_total;
        if (row1 > row2)
            mexErrMsgTxt("Invalid row range: R1 > R2 (or both out of bounds)");
    }
    long nrows = row2 - row1 + 1;

    // ------------------------------------------------------------------
    // 4.  Enumerate columns; apply column filter
    // ------------------------------------------------------------------
    struct ColInfo {
        int         colnum;      // 1-based FITS column number
        std::string name;
        std::string unit;
        ColType     type;
        long        repeat;      // repeat count (vector columns)
    };

    std::vector<ColInfo> cols;
    cols.reserve(ncols_total);

    for (int c = 1; c <= ncols_total; c++) {
        char ttype[FLEN_VALUE] = {0};
        char tunit[FLEN_VALUE] = {0};
        int  typecode = 0;
        long repeat = 1, width = 0;

        fits_get_bcolparms(fptr, c,
                           ttype, tunit,
                           NULL, &repeat, NULL, NULL, NULL, NULL,
                           &status);
        if (status) { status = 0; continue; }   // skip broken columns

        fits_get_coltype(fptr, c, &typecode, &repeat, &width, &status);
        if (status) { status = 0; continue; }

        std::string name(ttype);
        std::string unit(tunit);

        // Apply column filter (case-insensitive)
        if (!wanted_cols.empty()) {
            bool found = false;
            for (const auto& w : wanted_cols)
                if (iequal(w, name)) { found = true; break; }
            if (!found) continue;
        }

        ColInfo ci;
        ci.colnum = c;
        ci.name   = name;
        ci.unit   = unit;
        ci.type   = resolve_coltype(typecode);
        ci.repeat = repeat;
        cols.push_back(ci);
    }

    int ncols = (int)cols.size();
    if (ncols == 0)
        mexErrMsgTxt("No columns matched (or table is empty)");

    // ------------------------------------------------------------------
    // 5.  Build output struct  (cat)
    // ------------------------------------------------------------------
    std::vector<const char*> fieldnames(ncols);
    for (int i = 0; i < ncols; i++)
        fieldnames[i] = cols[i].name.c_str();

    mxArray* cat = mxCreateStructMatrix(1, 1, ncols, fieldnames.data());

    // ------------------------------------------------------------------
    // 6.  Read each column
    // ------------------------------------------------------------------
    for (int i = 0; i < ncols; i++) {
        const ColInfo& ci = cols[i];
        int anynul = 0;

        // Each element may itself be a vector (repeat > 1).
        // We store vector columns as nrows × repeat matrices.
        long repeat = (ci.type.is_string) ? 1 : ci.repeat;
        long nelems = nrows * repeat;

        if (ci.type.is_string) {
            // ----- string column ----------------------------------------
            // Find the max string width from the TFORM keyword
            char tform[FLEN_VALUE] = {0};
            int  status2 = 0;
            fits_get_acolparms(fptr, ci.colnum,
                               NULL, NULL, NULL, tform,
                               NULL, NULL, NULL, NULL,
                               &status2);
            // For binary tables use fits_get_coltype width
            long str_width = 1;
            {
                int  tc2 = 0; long rep2 = 1, wid2 = 0; int s2 = 0;
                fits_get_coltype(fptr, ci.colnum, &tc2, &rep2, &wid2, &s2);
                if (!s2 && wid2 > 0) str_width = wid2;
                else                 str_width = 80;  // ASCII fallback
            }

            // Allocate buffer for all strings
            long buflen = str_width + 1;
            std::vector<char> strbuf((size_t)(nrows * buflen), '\0');
            std::vector<char*> strptrs(nrows);
            for (long r = 0; r < nrows; r++)
                strptrs[r] = strbuf.data() + r * buflen;

            int s3 = 0;
            fits_read_col_str(fptr, ci.colnum,
                              row1, 1, nrows,
                              NULL,
                              strptrs.data(),
                              &anynul, &s3);
            if (s3) { s3 = 0; }

            mxArray* cell = mxCreateCellMatrix(nrows, 1);
            for (long r = 0; r < nrows; r++) {
                // Trim trailing spaces (common in FITS ASCII tables)
                std::string sv(strptrs[r]);
                size_t last = sv.find_last_not_of(' ');
                if (last != std::string::npos) sv = sv.substr(0, last + 1);
                else                            sv.clear();
                mxSetCell(cell, r, mxCreateString(sv.c_str()));
            }
            mxSetFieldByNumber(cat, 0, i, cell);

        } else if (ci.type.is_logical) {
            // ----- logical column ----------------------------------------
            mwSize dims[2] = { (mwSize)nrows, (mwSize)repeat };
            mxArray* arr = mxCreateLogicalArray(2, dims);
            mxLogical* data = mxGetLogicals(arr);

            // cfitsio reads logicals as char (0/1)
            std::vector<char> tmp(nelems, 0);
            int s4 = 0;
            fits_read_col(fptr, TLOGICAL, ci.colnum,
                          row1, 1, nelems,
                          NULL, tmp.data(), &anynul, &s4);
            if (s4) s4 = 0;
            for (long k = 0; k < nelems; k++)
                data[k] = (mxLogical)(tmp[k] != 0);

            mxSetFieldByNumber(cat, 0, i, arr);

        } else {
            // ----- numeric column ----------------------------------------
            mwSize dims[2] = { (mwSize)nrows, (mwSize)repeat };
            mxArray* arr = mxCreateNumericArray(2, dims,
                                                ci.type.mxclass, mxREAL);
            void* data = mxGetData(arr);

            int s5 = 0;
            fits_read_col(fptr, ci.type.cftype, ci.colnum,
                          row1, 1, nelems,
                          NULL, data, &anynul, &s5);
            if (s5) {
                // Fallback to double
                s5 = 0;
                mxDestroyArray(arr);
                arr  = mxCreateNumericArray(2, dims, mxDOUBLE_CLASS, mxREAL);
                data = mxGetData(arr);
                fits_read_col(fptr, TDOUBLE, ci.colnum,
                              row1, 1, nelems,
                              NULL, data, &anynul, &s5);
                if (s5) s5 = 0;
            }

            mxSetFieldByNumber(cat, 0, i, arr);
        }
    }

    // ------------------------------------------------------------------
    // 7.  Output 2: colnames  (Nx1 cell array)
    // ------------------------------------------------------------------
    mxArray* colnames = mxCreateCellMatrix(ncols, 1);
    for (int i = 0; i < ncols; i++)
        mxSetCell(colnames, i, mxCreateString(cols[i].name.c_str()));

    // ------------------------------------------------------------------
    // 8.  Output 3: units  (Nx1 cell array)
    // ------------------------------------------------------------------
    mxArray* units_cell = mxCreateCellMatrix(ncols, 1);
    for (int i = 0; i < ncols; i++)
        mxSetCell(units_cell, i, mxCreateString(cols[i].unit.c_str()));

    // ------------------------------------------------------------------
    // 9.  Close file & assign outputs
    // ------------------------------------------------------------------
    fits_close_file(fptr, &status);
    checkStatus(status);

    plhs[0] = cat;
    if (nlhs >= 2) plhs[1] = colnames;
    if (nlhs >= 3) plhs[2] = units_cell;
    if (nlhs >= 4) plhs[3] = mxCreateDoubleScalar((double)hdu);
}
