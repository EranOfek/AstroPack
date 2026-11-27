/* mex_blosc2_read.c - read a 2-D float32 Blosc2 NDArray (.b2nd file), as written by python-blosc2
 * (blosc2.asarray(..., urlpath=...)), into MATLAB.  Called by io.blosc2.read1.
 *
 *   [Image, Header] = mex_blosc2_read(FileName)
 *   [Image, Header] = mex_blosc2_read(Bytes)     - Bytes: uint8 vector holding the whole file, already in memory
 *     Image  - single matrix, in the array's C (row-major) order, i.e. size [Ncol Nrow]; io.blosc2.read1 transposes it.
 *     Header - char, the 'FITS_HEADER' variable-length metadata (msgpack string), '' if absent.
 *
 * Build: io.blosc2.mex.build (links the C-Blosc2 library statically; see ../README.md). */
#include "mex.h"
#include <string.h>
#include "blosc2.h"
#include "b2nd.h"

static int Initialized = 0;
static void cleanup(void) { if (Initialized) { blosc2_destroy(); Initialized = 0; } }

/* python-blosc2 stores vlmeta values msgpack-encoded; a string is fixstr / str8 / str16 / str32 */
static mxArray *msgpackString(const uint8_t *P, int32_t N)
{
    int64_t Off = 0, Len = 0;
    if (N < 1) return mxCreateString("");
    if ((P[0] & 0xE0) == 0xA0) { Off = 1; Len = P[0] & 0x1F; }
    else if (P[0] == 0xD9 && N >= 2) { Off = 2; Len = P[1]; }
    else if (P[0] == 0xDA && N >= 3) { Off = 3; Len = ((int64_t)P[1] << 8) | P[2]; }
    else if (P[0] == 0xDB && N >= 5) { Off = 5; Len = ((int64_t)P[1] << 24) | ((int64_t)P[2] << 16) | ((int64_t)P[3] << 8) | P[4]; }
    else return mxCreateString("");
    if (Off + Len > N) Len = N - Off;
    {
        mwSize Dims[2] = {1, (mwSize)Len};
        mxArray *S = mxCreateCharArray(2, Dims);
        mxChar *C = mxGetChars(S);
        int64_t I;
        for (I = 0; I < Len; I++) C[I] = (mxChar)P[Off + I];
        return S;
    }
}

void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{
    b2nd_array_t *A = NULL;
    int Rc;
    int64_t Nbytes;
    mxArray *Out;

    if (nrhs != 1 || !(mxIsChar(prhs[0]) || mxIsUint8(prhs[0])))
        mexErrMsgIdAndTxt("io:blosc2:args", "Usage: [Image,Header] = mex_blosc2_read(FileName or uint8 Bytes)");
    /* blosc2 creates per-thread data whose destructor lives in this MEX file: keep the file loaded (mexLock), so
       the destructor still exists when MATLAB's threads exit (otherwise MATLAB R2026a crashes at exit) */
    if (!Initialized) { blosc2_init(); Initialized = 1; mexLock(); mexAtExit(cleanup); }

    if (mxIsChar(prhs[0])) {
        char *FileName = mxArrayToString(prhs[0]);
        Rc = b2nd_open(FileName, &A);
        mxFree(FileName);
    } else {
        /* the file's bytes are a contiguous Blosc2 frame; no copy: the bytes stay owned by MATLAB */
        Rc = b2nd_from_cframe((uint8_t *)mxGetData(prhs[0]), (int64_t)mxGetNumberOfElements(prhs[0]), false, &A);
    }
    if (Rc < 0 || A == NULL) mexErrMsgIdAndTxt("io:blosc2:open", "b2nd_open failed (%d)", Rc);
    if (A->ndim != 2 || A->sc->typesize != 4) {
        b2nd_free(A);
        mexErrMsgIdAndTxt("io:blosc2:type", "only 2-D float32 arrays are supported (ndim %d, typesize %d)", A->ndim, A->sc->typesize);
    }

    /* C order: shape = [Nrow Ncol], fastest index last -> MATLAB [Ncol Nrow] */
    Out = mxCreateNumericMatrix((mwSize)A->shape[1], (mwSize)A->shape[0], mxSINGLE_CLASS, mxREAL);
    Nbytes = A->nitems * 4;
    Rc = b2nd_to_cbuffer(A, mxGetData(Out), Nbytes);
    if (Rc < 0) { b2nd_free(A); mxDestroyArray(Out); mexErrMsgIdAndTxt("io:blosc2:read", "b2nd_to_cbuffer failed (%d)", Rc); }
    plhs[0] = Out;

    if (nlhs > 1) {
        uint8_t *Content = NULL;
        int32_t Len = 0;
        if (blosc2_vlmeta_get(A->sc, "FITS_HEADER", &Content, &Len) >= 0 && Content != NULL) {
            plhs[1] = msgpackString(Content, Len);
            free(Content);
        } else {
            plhs[1] = mxCreateString("");
        }
    }
    b2nd_free(A);
}
