// https://blogs.mathworks.com/developer/2019/07/11/cpp-interface/
// https://www.mathworks.com/help/matlab/matlab_external/how-to-define-and-publish-matlab-interface-for-cpp-library-sample.html
// https://www.mathworks.com/help/matlab/matlab_external/publish-interface-to-shared-c-library.html
// https://www.mathworks.com/matlabcentral/fileexchange/38964-example-matlab-class-wrapper-for-a-c-class
// https://www.mathworks.com/help/coder/ug/call-cc-code-from-matlab-code.html

#include <math.h>
#include <stdio.h>
#include <string.h>
#include <string>

// Must include BEFORE fitsio.h due to conflict with 'TBYTE' definition
#include "mf/mf_logfile.h"
#include "mf/mf_string.h"

#include "fitsio.h"
#include "../wcslib.h"
#include "../getwcstab.h"

#include "wcslib_class.h"

// Static, should be moved to class
static LogFile* logfile = NULL;

#ifdef _WIN32
#define TEST_IMAGE_FILENAME "C:\\Temp\\image1.fits"
#else
#define TEST_IMAGE_FILENAME "/home/chen/images/image1.fits"
#endif


#define prm() ((wcsprm*)wcsi_ptr)

//===========================================================================

Wcslib::Wcslib()
{
    debugMode = true;
    header = NULL;
    headerAlloc = false;
    status = 0;
    nwcs = 0;
    fptr_ptr = NULL;
    wcs_ptr = NULL;
    wcsi_ptr = NULL;
}


Wcslib::~Wcslib()
{
}


//bool Wcslib::openFits(const char* _filename, int _mode)
bool Wcslib::openFits(const char* _filename, int _mode)
{
    freeHeader();
    filename = std::string(_filename);
    log("openFits: " + filename);

    char* alt = 0x0;
    int  alts[27], i, ialt, nkeyrec, nreject;

    // Open the FITS file 
    fitsfile* fptr;
    fits_open_file(&fptr, filename.c_str(), _mode, &status);
    fptr_ptr = (char*)fptr;

    // Read the primary header
    status = fits_hdr2str((fitsfile*)fptr_ptr, 1, NULL, 0, &header, &nkeyrec, &status);
    if (status != 0) {
        log(Asprintf("openFits error: "));
        fits_report_error(stderr, status);
        return false;
    }

    // Basic steps required to interpret a FITS WCS header, including -TAB.

    // Parse the primary header of the FITS file
    wcsprm* wcs;
    if ((status = wcspih(header, nkeyrec, WCSHDR_all, 2, &nreject, &nwcs, &wcs))) {
        log(Asprintf("wcspih ERROR %d: %s", status, get_wcshdr_errmsg()[status]));
    }

    // Store pointer to allocated struct
    wcs_ptr = (char*)wcs;

    // Read coordinate arrays from the binary table extension.
    if ((status = fits_read_wcstab(fptr, wcs->nwtb, (wtbarr*)wcs->wtb, &status))) {
        fits_report_error(stderr, status);
        return false;
    }

    // Translate non-standard WCS keyvalues.
    if ((status = wcsfix(7, 0, wcs, stat))) {
        for (i = 0; i < NWCSFIX; i++) {
            if (stat[i] > 0) {
                log(Asprintf("wcsfix ERROR %d: %s", status, get_wcsfix_errmsg()[stat[i]]));
            }
        }

        return false;
    }
    
    // @Chen: Check this, why we need it???

    // Sort out alternates.
    i = 0;
    if (alt) {
        if ('0' <= *alt && *alt <= '9') {
            if ((i = atoi(alt)) > nwcs - 1) {
                log(Asprintf("WARNING, no alternate coordinate representation: %s", alt));
                return 1;
            }

        }
        else {
            wcsidx(nwcs, &wcs, alts);

            ialt = toupper(*alt);
            if (strlen(alt) > 1) {
                log(Asprintf("WARNING, alternate specifier is invalid: %s", alt));
                return false;

            }
            else if (*alt == ' ') {
                if (alts[0] == -1) {
                    log(Asprintf("WARNING, no primary coordinate representation"));
                    return false;
                }

            }
            else if (ialt < 'A' || ialt > 'Z') {
                log(Asprintf("WARNING, alternate specifier is invalid: %s", alt));
                return false;

            }
            else {
                if ((i = alts[ialt - 'A' + 1]) == -1) {
                    log(Asprintf("WARNING, no alternate coordinate representation: %s", alt));
                    return false;
                }
            }
        }
    }

    // Store pointer to allocated struct
    // The wcsprm struct is now ready for use    
    wcsi_ptr = (char*)(wcs + i);

    log("openFits done: " + filename);
    return true;
}

bool Wcslib::openTest()
{
    return openFits(TEST_IMAGE_FILENAME);
}


void Wcslib::closeFits()
{
    fits_close_file((fitsfile*)fptr_ptr, &status);
    //freeHeader();
    //fits_free_memory(header, &status);
}


bool Wcslib::readFits(const char* _filename)
{
    filename = std::string(_filename);

    if (filename.empty())
        filename = TEST_IMAGE_FILENAME;

    log("readFits: " + filename);
    bool result = openFits(_filename, 0);
    closeFits();
    return result;
}


char* Wcslib::readFitsHeader(const char* _filename, int& nkeyrec)
{
    filename = std::string(_filename);
    log("readFitsHeader: " + filename);

    char keyrec[81];
    FILE* fptr;

    // Allocate header buffer
    freeHeader();
    header = new char[288001];
    headerAlloc = true;

    // Set line buffering in case stdout is redirected to a file, otherwise
    // stdout and stderr messages will be jumbled (stderr is unbuffered).
    //setvbuf(stdout, NULL, _IOLBF, 0);

    if ((fptr = fopen(_filename, "r")) == 0) {
        return "";
    }

    int k = 0;
    int gotend = 0;
    nkeyrec = 0;
    for (int iblock = 0; iblock < 100; iblock++) {
        for (int ikeyrec = 0; ikeyrec < 36; ikeyrec++) {
            if (fgets(keyrec, 81, fptr) == 0) {
                break;
            }

            if (strncmp(keyrec, "        ", 8) == 0) continue;
            if (strncmp(keyrec, "COMMENT ", 8) == 0) continue;
            if (strncmp(keyrec, "HISTORY ", 8) == 0) continue;

            memcpy(header + k, keyrec, 80);
            k += 80;
            nkeyrec++;

            if (strncmp(keyrec, "END     ", 8) == 0) {
                // An END keyrecord was read, but read the rest of the block.
                gotend = 1;
            }
        }

        if (gotend) break;
    }
    fclose(fptr);

    header[k] = 0;
    return header;
}


bool Wcslib::loadHeader(const char* _headerText, int nkeyrec)
{
    //char  infile[] = "bth.fits";
    int   colsel[8], ctrl,  iwcs, keysel, nreject, relax;
    struct wcsprm* wcs;
    //char keyrec[81];

    log(Asprintf("loadHeaderFromText, nkeyrec: %d", nkeyrec));

    // Allocate header buffer
    freeHeader();
    header = new char[288001];
    headerAlloc = true;

    // Parse the header, allowing all recognized non-standard WCS keywords and usage.  
    // WCS keyrecords that are used are culled from the header, illegal ones are reported.
    relax = WCSHDR_all;
    ctrl = -2;
    keysel = 0;
    colsel[0] = 0;

    fprintf(stderr, "\nIllegal or extraneous WCS header keyrecords rejected by wcsbth():\n");
    if ((status = wcsbth((char*)_headerText, nkeyrec, relax, ctrl, keysel, colsel, &nreject, &nwcs, &wcs))) {
        fprintf(stderr, "wcsbth ERROR %d: %s.\n", status, get_wcs_errmsg()[status]);
    }
    if (!nreject) {
        fprintf(stderr, "(none)\n");
    }

    // Store pointer to allocated struct
    wcs_ptr = (char*)wcs;

    return true;
}


bool Wcslib::readHeader(const char* _filename)
{
    log(Asprintf("loadHeader, nkeyrec: %s", _filename));

    bool result = false;
    int nkeyrec = 0;
    char* h = readFitsHeader(_filename, nkeyrec);
    if (h) {
        result = loadHeader(h, nkeyrec);
    }
    return result;
}


void Wcslib::release()
{
    log(Asprintf("release"));
    wcsprm* wcs = (wcsprm*)wcs_ptr;
    setStatus(wcsvfree(&nwcs, &wcs));
    wcs_ptr = NULL;
}


bool Wcslib::pixToSky(double x, double y, double& sx, double& sy)
{
    double imgcrd[2], phi, pixcrd[2], theta, world[2];

    if (debugMode)
        log(Asprintf("pix2sky: x=%.3lf, y=%.3lf", x, y));

    pixcrd[0] = x;
    pixcrd[1] = y;
    status = wcsp2s((wcsprm*)wcsi_ptr, 1, 2, pixcrd, imgcrd, &phi, &theta, world, stat);

    if (debugMode)
        log(Asprintf("pix2sky: status=%d, x=%.3lf, y=%.3lf", status, world[0], world[1]));

    sx = world[0];
    sy = world[1];

    return true;
}


bool Wcslib::skyToPix(double x, double y, double& px, double& py)
{
    double imgcrd[2], phi, pixcrd[2], theta, world[2];

    if (debugMode)
        log(Asprintf("sky2pix: x=%.3lf, y=%.3lf", x, y));

    world[0] = x;
    world[1] = y;
    status = wcss2p((wcsprm*)wcsi_ptr, 1, 2, world, &phi, &theta, imgcrd, pixcrd, stat);

    if (debugMode)
        log(Asprintf("sky2pix: status=%d, x=%.3lf, y=%.3lf", status, world[0], world[1]));

    px = pixcrd[0];
    py = pixcrd[1];

    return true;
}

//---------------------------------------------------------------------------

void Wcslib::setStatus(int _status)
{
    status = _status;
}


void Wcslib::freeHeader()
{
    if (header && headerAlloc) {
        log(Asprintf("freeHeader: delete[]"));
        delete[] header;
        header = NULL;
        headerAlloc = false;
    }
    else if (header && !headerAlloc) {
        log(Asprintf("freeHeader: fits_free_memory"));
        fits_free_memory(header, &status);
        header = NULL;
    }
    else {
        log(Asprintf("freeHeader: nothing to do"));
    }
}


bool Wcslib::setprm()
{
    if (wcssetprm(prm())) {
        //wcsperr(&wcspol, 0x0);
        return false;
    }
    return true;
}


void Wcslib::log(const std::string& s)
{
    static LogFile* logfile = NULL;
    if (!logfile)
    {
        logfile = new LogFile("C:\\Temp\\wcs.log");
    }
    logfile->log(s);
}


//===========================================================================
//                         Properties - Getters
//===========================================================================

// See twcs.c

#define GET_PROP_VAL(prop) \
    double value = prm()->prop; \
    if (debugMode) log(Asprintf("%s: %.3lf", __func__, value)); \
    return value;

#define GET_PROP_INT(prop) \
    int value = prm()->prop; \
    if (debugMode) log(Asprintf("%s: %d", __func__, value)); \
    return value;


uint8_t Wcslib::getProp_NAXIS() 
{
    GET_PROP_INT(naxis);
}

double Wcslib::getProp_WCSAXES()
{
    return 0;
}

double Wcslib::getProp_LONPOLE()
{
    GET_PROP_VAL(lonpole);
}

double Wcslib::getProp_LATPOLE()
{
    GET_PROP_VAL(lat);
}

double Wcslib::getProp_EQUINOX()
{
    GET_PROP_VAL(equinox);
}

/* Not found in wcslib
double Wcslib::propGet_AlphaP(double value)
{

}

double Wcslib::propGet_DeltaP(double value)
{

}

double Wcslib::propGet_Alpha0(double value)
{

}

double Wcslib::propGet_Delta0(double value)
{

}

double Wcslib::propGet_Phi0(double value)
{

}

double Wcslib::propGet_Theta0(double value)
{

}

double Wcslib::propGet_PhiP(double value) 
{

}
*/
//===========================================================================
//                         Properties - Setters
//===========================================================================

#define SET_PROP_VAL(prop) \
    if (debugMode) log(Asprintf("%s: %.3lf", __func__, value)); \
    prm()->prop = value; \
    setprm(); \
    return true;


#define SET_PROP_INT(prop) \
    if (debugMode) log(Asprintf("%s: %d", __func__, value)); \
    prm()->lonpole = value; \
    setprm(); \
    return true;



// Properties - Setters
bool Wcslib::setProp_NAXIS(uint8_t value)
{
    SET_PROP_INT(naxis);
}

bool Wcslib::setProp_WCSAXES(double value)
{
    return true;
}

bool Wcslib::setProp_LONPOLE(double value)
{
    SET_PROP_VAL(lonpole);
}

bool Wcslib::setProp_LATPOLE(double value)
{
    SET_PROP_VAL(latpole);
}

bool Wcslib::setProp_EQUINOX(double value)
{
    SET_PROP_VAL(equinox);
}
//===========================================================================

//===========================================================================
// Static
bool Wcslib::unitTest(const char* fitsFileName)
{
    log(Asprintf("unitTest started"));

    bool result = true;

    // Use default file
    if (!fitsFileName) {
#ifdef _WIN32
        fitsFileName = TEST_IMAGE_FILENAME;
#else
#endif
    }

    std::string filename(fitsFileName);
    Wcslib wcs;
    wcs.openTest();

    //wcs.openFits(filename.c_str());

    // Convert coordinates
    int iters = 10;
    int okCount = 0;
    for (int i = 0; i < iters; i++) {

        double x = 10.0 * i;
        double y = 5.0 * i;
        double sx, sy, px, py;

        // Pixel to sky
        wcs.pixToSky(x, y, sx, sy);
        
        // Sky to pixel
        wcs.skyToPix(sx, sy, px, py);
        
        if (((int)px == x) && ((int)py == y)) {
            okCount++;
        }
        else {

        }
    }

    // All good
    if (okCount == iters) {
    }

    // Failed
    else {
        result = false;
    }

    if (result) {
        log(Asprintf("unitTest OK"));
    }
    else {
        log(Asprintf("unitTest FAILED!"));
    }

    return result;
}

