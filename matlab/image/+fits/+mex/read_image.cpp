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

void mexFunction(int nlhs, mxArray* plhs[],
                 int nrhs, const mxArray* prhs[])
{
    if (nrhs != 2)
        mexErrMsgTxt("Usage: img = fits_read_image(filename, hdu)");

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

    std::vector<mwSize> dims(naxis);
    long nelements = 1;
    for (int i = 0; i < naxis; i++) {
        dims[i] = (mwSize)naxes[i];
        nelements *= naxes[i];
    }
    
    mexPrintf("Bitpix: %d \n",bitpix);

    int anynul = 0;

    // -------- Attempt native read --------
    mxArray* out = nullptr;
    void* data = nullptr;

    bool native_ok = true;
        
    if (bitpix == SHORT_IMG) {
        double bzero = 0.0;
        fits_read_key(fptr, TDOUBLE, "BZERO", &bzero, NULL, &status);
        if (status) status = 0;
        
        if (bzero == 32768.0) {
            // unsigned 16-bit path
            mxArray* out = mxCreateNumericArray(naxis, dims.data(), mxUINT16_CLASS, mxREAL);
            unsigned short* data = (unsigned short*)mxGetData(out);
            
            fits_read_img(fptr, TUSHORT, 1, nelements,
                    NULL, data, NULL, &status);
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

        fits_read_img(fptr, datatype, 1, nelements,
                      NULL, data, &anynul, &status_try);

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
        mexPrintf("Falling back to double read\n");

        out = mxCreateNumericArray(naxis, dims.data(), mxDOUBLE_CLASS, mxREAL);
        double* dptr = mxGetPr(out);

        fits_read_img(fptr, TDOUBLE, 1, nelements,
                      NULL, dptr, &anynul, &status);
        checkStatus(status);
    }

    fits_close_file(fptr, &status);
    checkStatus(status);

    plhs[0] = out;
}