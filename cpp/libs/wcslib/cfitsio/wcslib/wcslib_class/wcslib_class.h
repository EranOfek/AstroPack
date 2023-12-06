//
// wcslib wrapper class
// Important: wcsset() conflicts with the function in string.h standard header, 
// on both Windows and Linux. It was renamed to wcssetprm()
//

#ifndef wcslib_class_h
#define wcslib_class_h

#include <math.h>
#include <stdio.h>
#include <string.h>
#include <string>

// We do NOT include here any fitsio or wcslib headers, so this header will be
// easily used by Matlab with the shared library (DLL/so) to generate the interface.
// https://www.mathworks.com/help/matlab/matlab_external/publish-interface-to-shared-c-library.html

//#include "fitsio.h"
//#include "../wcslib.h"
//#include "../getwcstab.h"

// Define export directive, originally in fitsio.h
#ifndef CFITS_API
    #define CFITS_API __declspec(dllexport)
#endif

//===========================================================================
// Wrapper class
class CFITS_API Wcslib {
public:

    // Constructor
    Wcslib();

    // Destructor()
    ~Wcslib();

    // Read header from FITS file (file is closed after reading)
    bool readFits(const char* filename);

    // Load header using readFitsHeader() and loadHeaderFromText()
    bool readHeader(const char* filename);

    // Load header from text
    bool loadHeader(const char* header_Text, int nkeyrec);

    // Open test file
    bool openTest();

private:
    // Open fits file and read header, using cfitsio. see test/twcshdr.c
    // The file is left open until calling closeFits()
    //bool openFits(const char* _filename, int mode = 0 /*READONLY*/);
    // Matlab mlx: Fix
    bool openFits(const char* filename, int mode = 0 /*READONLY*/);

    // Close FITS file, opened by openFits()
    void closeFits();

    // Read fits header from file WITHOUT using cfitsio, see test/tbth1.c
    // The file is closed after reading the header
    char* readFitsHeader(const char* filename, int& nkeyrec);

    // Release memory and cleanup
    void release();

public:
    // Convert pixel to sky
    // Currently to bypass Matlab interface issues, we return double and get the other double by getwy()
    bool pixToSky(double x, double y, double& sx, double& sy);

    // Convert sky to pixel
    // Currently to bypass Matlab interface issues, we return double and get the other double by getpixy()
    bool skyToPix(double x, double y, double& sx, double& sy);

    // Log message to file
    static void log(const std::string& s);

    // Unit-test
    static bool unitTest(const char* fits_filename = NULL);

//private:

    // Properties - Getters
    uint8_t getProp_NAXIS();
    double  getProp_WCSAXES();
    double  getProp_LONPOLE();
    double  getProp_LATPOLE();
    double  getProp_EQUINOX();

    // Not found in wcslib
    /*
    double propGet_AlphaP(double value);
    double propGet_DeltaP(double value);
    double propGet_Alpha0(double value);
    double propGet_Delta0(double value);
    double propGet_Phi0(double value);
    double propGet_Theta0(double value);
    double propGet_PhiP(double value);
    */

    // Properties - Setters
    bool setProp_NAXIS(uint8_t value);
    bool setProp_WCSAXES(double value);
    bool setProp_LONPOLE(double value);
    bool setProp_LATPOLE(double value);
    bool setProp_EQUINOX(double value);

    // Not found in wcslib
    /*
    void propSet_AlphaP(double value);
    void propSet_DeltaP(double value);
    void propSet_Alpha0(double value);
    void propSet_Delta0(double value);
    void propSet_Phi0(double value);
    void propSet_Theta0(double value);
    void propSet_PhiP(double value);
    */    

    /*
    NAXIS(1, 1)   uint8 = 2;
    WCSAXES(1, 1) uint8 = 2
    CTYPE(1, :)   cell = { '','' };
    CUNIT        cell = { 'deg','deg' };
    RADESYS      char = 'ICRS';
    LONPOLE      double = 0;
    LATPOLE      double = 90;
    EQUINOX = 2000;
    CRPIX(1, :)   double = [0 0];
    CRVAL(1, :)   double = [1 1];
    CDELT(1, :)   double = [1 1];
    CD           double = [1 0;0 1];
    PC           double = [];
    PV           cell = { zeros(0,2),zeros(0,2) };
    SIP          cell = { zeros(0,2),zeros(0,2) };
    end

        properties(GetAccess = public)
        ProjType     char = '';
    ProjClass    char = '';
    CooName(1, :) cell = { '','' };
    AlphaP(1, 1)  double = NaN;
    DeltaP(1, 1)  double = NaN;
    Alpha0(1, 1)  double = NaN;
    Delta0(1, 1)  double = NaN;
    Phi0(1, 1)    double = NaN;
    Theta0(1, 1)  double = NaN;
    PhiP(1, 1)    double = NaN;
    */


//private:

    // Set last status
    void setStatus(int _status);

    // Free header memory
    void freeHeader();

    //
    bool setprm();


    // Data
    bool            debugMode;
    std::string     filename;
    std::string     radesys;
    char*           header;
    bool            headerAlloc;
    int             status;
    int             nwcs;
    int             stat[7];  //NWCSFIX];

    // Use char* to eliminate including wcslib headers here
    // Will be casted in cpp
    char*   fptr_ptr;           // fitsfile*
    char*   wcs_ptr;            // wcsprm*
    char*   wcsi_ptr;           // wcsprm*
};

#endif
