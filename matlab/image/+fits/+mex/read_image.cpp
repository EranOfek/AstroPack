// compiled with mex read_image.cpp -lcfitsio
// after sudo apt install libcfitsio-dev  
#include "mex.h"
#include "fitsio.h"
#include <vector>

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

mxArray* readHeaderCell(fitsfile* fptr)
{
    int status = 0, nkeys = 0;

    fits_get_hdrspace(fptr, &nkeys, NULL, &status);
    checkStatus(status);

    mxArray* cell = mxCreateCellMatrix(nkeys, 3);

    char key[FLEN_KEYWORD];
    char comment[FLEN_COMMENT];

    for (int i = 1; i <= nkeys; i++) {

        char value_str[FLEN_VALUE];
        fits_read_keyn(fptr, i, key, value_str, comment, &status);
        checkStatus(status);

        int row = i - 1;

        // --- Column 1: keyword ---
        mxSetCell(cell, row + 0*nkeys, mxCreateString(key));

        // --- Column 2: value (typed) ---
        mxArray* val = nullptr;

        int status2 = 0;

        // Try double
        double dval;
        if (fits_read_key(fptr, TDOUBLE, key, &dval, NULL, &status2) == 0) {
            val = mxCreateDoubleScalar(dval);
        }
        else {
            status2 = 0;

            // Try logical
            int lval;
            if (fits_read_key(fptr, TLOGICAL, key, &lval, NULL, &status2) == 0) {
                val = mxCreateLogicalScalar(lval);
            }
            else {
                status2 = 0;

                // Try string
                char sval[FLEN_VALUE];
                if (fits_read_key(fptr, TSTRING, key, sval, NULL, &status2) == 0) {
                    val = mxCreateString(sval);
                }
                else {
                    status2 = 0;
                    val = mxCreateString("");  // fallback
                }
            }
        }

        mxSetCell(cell, row + 1*nkeys, val);

        // --- Column 3: comment ---
        mxSetCell(cell, row + 2*nkeys, mxCreateString(comment));
    }

    return cell;
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
                plhs[1] = readHeaderCell(fptr);
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