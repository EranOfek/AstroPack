//
// wcslib wrapper class
// Important: wcsset() conflicts with the function in string.h standard header, 
// on both Windows and Linux. It was renamed to wcssetprm()
//

#ifndef wcslib_wrapper_matlab_h
#define wcslib_wrapper_matlab_h

#include <math.h>
#include <stdio.h>
#include <string.h>
#include <string>

//
// This file contains C style API, the C++ interface to Matlab has too many bugs,
// and Matlab crashes many times (R2020b).
//
// We do NOT include here any fitsio or wcslib headers, so this header will be
// easily used by Matlab with the shared library (DLL/so) to generate the interface.
// https://www.mathworks.com/help/matlab/matlab_external/publish-interface-to-shared-c-library.html
//

// Define export directive, originally in fitsio.h
#ifndef CFITS_API
    #define CFITS_API __declspec(dllexport)
#endif

//===========================================================================
//                          Test The C Interface
//===========================================================================

// OK
/*
getStringCopyDefinition = addFunction(libDef, ...
    "char const * getStringCopy(char const * str)", ...
    "MATLABName", "clib.wcslibPkg.getStringCopy", ...
    "Description", "clib.wcslibPkg.getStringCopy    Representation of C++ function getStringCopy.");% Modify help description values as needed.
    defineArgument(getStringCopyDefinition, "str", "string", "input", "nullTerminated"); % '<MLTYPE>' can be clib.array.wcslibPkg.Char, int8, string, or char
    defineOutput(getStringCopyDefinition, "RetVal", "string", "nullTerminated"); % '<MLTYPE>' can be clib.array.wcslibPkg.Char, int8, string, or char
    validate(getStringCopyDefinition);
*/
CFITS_API char const* tstStringCopy(char const* str);

// OK
bool CFITS_API tstGetBool();
bool CFITS_API tstSetBool(bool value);

// OK
int CFITS_API tstGetInt();
bool CFITS_API tstSetInt(int value);

// OK
double CFITS_API tstGetDouble();
bool CFITS_API tstSetDouble(double value);

CFITS_API char const* tstGetStr();
bool CFITS_API tstSetStr(const char* str);

void CFITS_API tstGetIntMat(int32_t* mat, size_t len);
void CFITS_API tstSetIntMat(int32_t* mat, size_t len);

// OK
void CFITS_API tstGetDoubleMat(double* mat, size_t len);
void CFITS_API tstSetDoubleMat(double* mat, size_t len);

//===========================================================================

// Wrapper class
bool CFITS_API wcsInit();

bool CFITS_API wcsReadFits(const char* fname);

bool CFITS_API wcsReadHeader(const char* fname);

void CFITS_API wcsPixToSky(double* x, double* y, size_t len);

void CFITS_API wcsSkyToPix(double* x, double* y, size_t len);

//===========================================================================
//                  Auto-Generated Code - Setters/Getters
//===========================================================================

//<code>
// Get/set property: flag (int)
int CFITS_API wcsGetProp_flag();
bool CFITS_API wcsSetProp_flag(int value);

// Get/set property: naxis (int)
int CFITS_API wcsGetProp_naxis();
bool CFITS_API wcsSetProp_naxis(int value);

// Get/set property: crpix (double*)
int CFITS_API wcsGetPropLen_crpix();
void CFITS_API wcsGetProp_crpix(double* value, size_t len);
void CFITS_API wcsSetProp_crpix(double* value, size_t len);

// Get/set property: pc (double*)
int CFITS_API wcsGetPropLen_pc();
void CFITS_API wcsGetProp_pc(double* value, size_t len);
void CFITS_API wcsSetProp_pc(double* value, size_t len);

// Get/set property: cdelt (double*)
int CFITS_API wcsGetPropLen_cdelt();
void CFITS_API wcsGetProp_cdelt(double* value, size_t len);
void CFITS_API wcsSetProp_cdelt(double* value, size_t len);

// Get/set property: crval (double*)
int CFITS_API wcsGetPropLen_crval();
void CFITS_API wcsGetProp_crval(double* value, size_t len);
void CFITS_API wcsSetProp_crval(double* value, size_t len);

// Get/set property: lonpole (double)
double CFITS_API wcsGetProp_lonpole();
bool CFITS_API wcsSetProp_lonpole(double value);

// Get/set property: latpole (double)
double CFITS_API wcsGetProp_latpole();
bool CFITS_API wcsSetProp_latpole(double value);

// Get/set property: restfrq (double)
double CFITS_API wcsGetProp_restfrq();
bool CFITS_API wcsSetProp_restfrq(double value);

// Get/set property: restwav (double)
double CFITS_API wcsGetProp_restwav();
bool CFITS_API wcsSetProp_restwav(double value);

// Get/set property: npv (int)
int CFITS_API wcsGetProp_npv();
bool CFITS_API wcsSetProp_npv(int value);

// Get/set property: npvmax (int)
int CFITS_API wcsGetProp_npvmax();
bool CFITS_API wcsSetProp_npvmax(int value);

// Get/set property: nps (int)
int CFITS_API wcsGetProp_nps();
bool CFITS_API wcsSetProp_nps(int value);

// Get/set property: npsmax (int)
int CFITS_API wcsGetProp_npsmax();
bool CFITS_API wcsSetProp_npsmax(int value);

// Get/set property: cd (double*)
int CFITS_API wcsGetPropLen_cd();
void CFITS_API wcsGetProp_cd(double* value, size_t len);
void CFITS_API wcsSetProp_cd(double* value, size_t len);

// Get/set property: crota (double*)
int CFITS_API wcsGetPropLen_crota();
void CFITS_API wcsGetProp_crota(double* value, size_t len);
void CFITS_API wcsSetProp_crota(double* value, size_t len);

// Get/set property: altlin (int)
int CFITS_API wcsGetProp_altlin();
bool CFITS_API wcsSetProp_altlin(int value);

// Get/set property: velref (int)
int CFITS_API wcsGetProp_velref();
bool CFITS_API wcsSetProp_velref(int value);

// Get/set property: alt (string)
CFITS_API char const*wcsGetProp_alt();
CFITS_API void wcsSetProp_alt(const char* value);

// Get/set property: colnum (int)
int CFITS_API wcsGetProp_colnum();
bool CFITS_API wcsSetProp_colnum(int value);

// Get/set property: colax (int*)
int CFITS_API wcsGetPropLen_colax();
void CFITS_API wcsGetProp_colax(int* value, size_t len);
void CFITS_API wcsSetProp_colax(int* value, size_t len);

// Get/set property: crder (double*)
int CFITS_API wcsGetPropLen_crder();
void CFITS_API wcsGetProp_crder(double* value, size_t len);
void CFITS_API wcsSetProp_crder(double* value, size_t len);

// Get/set property: csyer (double*)
int CFITS_API wcsGetPropLen_csyer();
void CFITS_API wcsGetProp_csyer(double* value, size_t len);
void CFITS_API wcsSetProp_csyer(double* value, size_t len);

// Get/set property: czphs (double*)
int CFITS_API wcsGetPropLen_czphs();
void CFITS_API wcsGetProp_czphs(double* value, size_t len);
void CFITS_API wcsSetProp_czphs(double* value, size_t len);

// Get/set property: cperi (double*)
int CFITS_API wcsGetPropLen_cperi();
void CFITS_API wcsGetProp_cperi(double* value, size_t len);
void CFITS_API wcsSetProp_cperi(double* value, size_t len);

// Get/set property: wcsname (string)
CFITS_API char const*wcsGetProp_wcsname();
CFITS_API void wcsSetProp_wcsname(const char* value);

// Get/set property: timesys (string)
CFITS_API char const*wcsGetProp_timesys();
CFITS_API void wcsSetProp_timesys(const char* value);

// Get/set property: trefpos (string)
CFITS_API char const*wcsGetProp_trefpos();
CFITS_API void wcsSetProp_trefpos(const char* value);

// Get/set property: trefdir (string)
CFITS_API char const*wcsGetProp_trefdir();
CFITS_API void wcsSetProp_trefdir(const char* value);

// Get/set property: plephem (string)
CFITS_API char const*wcsGetProp_plephem();
CFITS_API void wcsSetProp_plephem(const char* value);

// Get/set property: timeunit (string)
CFITS_API char const*wcsGetProp_timeunit();
CFITS_API void wcsSetProp_timeunit(const char* value);

// Get/set property: dateref (string)
CFITS_API char const*wcsGetProp_dateref();
CFITS_API void wcsSetProp_dateref(const char* value);

// Get/set property: mjdref (double*)
int CFITS_API wcsGetPropLen_mjdref();
void CFITS_API wcsGetProp_mjdref(double* value, size_t len);
void CFITS_API wcsSetProp_mjdref(double* value, size_t len);

// Get/set property: timeoffs (double)
double CFITS_API wcsGetProp_timeoffs();
bool CFITS_API wcsSetProp_timeoffs(double value);

// Get/set property: dateobs (string)
CFITS_API char const*wcsGetProp_dateobs();
CFITS_API void wcsSetProp_dateobs(const char* value);

// Get/set property: datebeg (string)
CFITS_API char const*wcsGetProp_datebeg();
CFITS_API void wcsSetProp_datebeg(const char* value);

// Get/set property: dateavg (string)
CFITS_API char const*wcsGetProp_dateavg();
CFITS_API void wcsSetProp_dateavg(const char* value);

// Get/set property: dateend (string)
CFITS_API char const*wcsGetProp_dateend();
CFITS_API void wcsSetProp_dateend(const char* value);

// Get/set property: mjdobs (double)
double CFITS_API wcsGetProp_mjdobs();
bool CFITS_API wcsSetProp_mjdobs(double value);

// Get/set property: mjdbeg (double)
double CFITS_API wcsGetProp_mjdbeg();
bool CFITS_API wcsSetProp_mjdbeg(double value);

// Get/set property: mjdavg (double)
double CFITS_API wcsGetProp_mjdavg();
bool CFITS_API wcsSetProp_mjdavg(double value);

// Get/set property: mjdend (double)
double CFITS_API wcsGetProp_mjdend();
bool CFITS_API wcsSetProp_mjdend(double value);

// Get/set property: jepoch (double)
double CFITS_API wcsGetProp_jepoch();
bool CFITS_API wcsSetProp_jepoch(double value);

// Get/set property: bepoch (double)
double CFITS_API wcsGetProp_bepoch();
bool CFITS_API wcsSetProp_bepoch(double value);

// Get/set property: tstart (double)
double CFITS_API wcsGetProp_tstart();
bool CFITS_API wcsSetProp_tstart(double value);

// Get/set property: tstop (double)
double CFITS_API wcsGetProp_tstop();
bool CFITS_API wcsSetProp_tstop(double value);

// Get/set property: xposure (double)
double CFITS_API wcsGetProp_xposure();
bool CFITS_API wcsSetProp_xposure(double value);

// Get/set property: telapse (double)
double CFITS_API wcsGetProp_telapse();
bool CFITS_API wcsSetProp_telapse(double value);

// Get/set property: timsyer (double)
double CFITS_API wcsGetProp_timsyer();
bool CFITS_API wcsSetProp_timsyer(double value);

// Get/set property: timrder (double)
double CFITS_API wcsGetProp_timrder();
bool CFITS_API wcsSetProp_timrder(double value);

// Get/set property: timedel (double)
double CFITS_API wcsGetProp_timedel();
bool CFITS_API wcsSetProp_timedel(double value);

// Get/set property: timepixr (double)
double CFITS_API wcsGetProp_timepixr();
bool CFITS_API wcsSetProp_timepixr(double value);

// Get/set property: obsgeo (double*)
int CFITS_API wcsGetPropLen_obsgeo();
void CFITS_API wcsGetProp_obsgeo(double* value, size_t len);
void CFITS_API wcsSetProp_obsgeo(double* value, size_t len);

// Get/set property: obsorbit (string)
CFITS_API char const*wcsGetProp_obsorbit();
CFITS_API void wcsSetProp_obsorbit(const char* value);

// Get/set property: radesys (string)
CFITS_API char const*wcsGetProp_radesys();
CFITS_API void wcsSetProp_radesys(const char* value);

// Get/set property: equinox (double)
double CFITS_API wcsGetProp_equinox();
bool CFITS_API wcsSetProp_equinox(double value);

// Get/set property: specsys (string)
CFITS_API char const*wcsGetProp_specsys();
CFITS_API void wcsSetProp_specsys(const char* value);

// Get/set property: ssysobs (string)
CFITS_API char const*wcsGetProp_ssysobs();
CFITS_API void wcsSetProp_ssysobs(const char* value);

// Get/set property: velosys (double)
double CFITS_API wcsGetProp_velosys();
bool CFITS_API wcsSetProp_velosys(double value);

// Get/set property: zsource (double)
double CFITS_API wcsGetProp_zsource();
bool CFITS_API wcsSetProp_zsource(double value);

// Get/set property: ssyssrc (string)
CFITS_API char const*wcsGetProp_ssyssrc();
CFITS_API void wcsSetProp_ssyssrc(const char* value);

// Get/set property: velangl (double)
double CFITS_API wcsGetProp_velangl();
bool CFITS_API wcsSetProp_velangl(double value);

// Get/set property: ntab (int)
int CFITS_API wcsGetProp_ntab();
bool CFITS_API wcsSetProp_ntab(int value);

// Get/set property: nwtb (int)
int CFITS_API wcsGetProp_nwtb();
bool CFITS_API wcsSetProp_nwtb(int value);

// Get/set property: lngtyp (string)
CFITS_API char const*wcsGetProp_lngtyp();
CFITS_API void wcsSetProp_lngtyp(const char* value);

// Get/set property: lattyp (string)
CFITS_API char const*wcsGetProp_lattyp();
CFITS_API void wcsSetProp_lattyp(const char* value);

// Get/set property: lng (int)
int CFITS_API wcsGetProp_lng();
bool CFITS_API wcsSetProp_lng(int value);

// Get/set property: lat (int)
int CFITS_API wcsGetProp_lat();
bool CFITS_API wcsSetProp_lat(int value);

// Get/set property: spec (int)
int CFITS_API wcsGetProp_spec();
bool CFITS_API wcsSetProp_spec(int value);

// Get/set property: cubeface (int)
int CFITS_API wcsGetProp_cubeface();
bool CFITS_API wcsSetProp_cubeface(int value);

// Get/set property: types (int*)
int CFITS_API wcsGetPropLen_types();
void CFITS_API wcsGetProp_types(int* value, size_t len);
void CFITS_API wcsSetProp_types(int* value, size_t len);

// Get/set property: m_flag (int)
int CFITS_API wcsGetProp_m_flag();
bool CFITS_API wcsSetProp_m_flag(int value);

// Get/set property: m_naxis (int)
int CFITS_API wcsGetProp_m_naxis();
bool CFITS_API wcsSetProp_m_naxis(int value);

// Get/set property: m_crpix (double*)
int CFITS_API wcsGetPropLen_m_crpix();
void CFITS_API wcsGetProp_m_crpix(double* value, size_t len);
void CFITS_API wcsSetProp_m_crpix(double* value, size_t len);

// Get/set property: m_pc (double*)
int CFITS_API wcsGetPropLen_m_pc();
void CFITS_API wcsGetProp_m_pc(double* value, size_t len);
void CFITS_API wcsSetProp_m_pc(double* value, size_t len);

// Get/set property: m_cdelt (double*)
int CFITS_API wcsGetPropLen_m_cdelt();
void CFITS_API wcsGetProp_m_cdelt(double* value, size_t len);
void CFITS_API wcsSetProp_m_cdelt(double* value, size_t len);

// Get/set property: m_crval (double*)
int CFITS_API wcsGetPropLen_m_crval();
void CFITS_API wcsGetProp_m_crval(double* value, size_t len);
void CFITS_API wcsSetProp_m_crval(double* value, size_t len);

// Get/set property: m_cd (double*)
int CFITS_API wcsGetPropLen_m_cd();
void CFITS_API wcsGetProp_m_cd(double* value, size_t len);
void CFITS_API wcsSetProp_m_cd(double* value, size_t len);

// Get/set property: m_crota (double*)
int CFITS_API wcsGetPropLen_m_crota();
void CFITS_API wcsGetProp_m_crota(double* value, size_t len);
void CFITS_API wcsSetProp_m_crota(double* value, size_t len);

// Get/set property: m_colax (int*)
int CFITS_API wcsGetPropLen_m_colax();
void CFITS_API wcsGetProp_m_colax(int* value, size_t len);
void CFITS_API wcsSetProp_m_colax(int* value, size_t len);

// Get/set property: m_crder (double*)
int CFITS_API wcsGetPropLen_m_crder();
void CFITS_API wcsGetProp_m_crder(double* value, size_t len);
void CFITS_API wcsSetProp_m_crder(double* value, size_t len);

// Get/set property: m_csyer (double*)
int CFITS_API wcsGetPropLen_m_csyer();
void CFITS_API wcsGetProp_m_csyer(double* value, size_t len);
void CFITS_API wcsSetProp_m_csyer(double* value, size_t len);

// Get/set property: m_czphs (double*)
int CFITS_API wcsGetPropLen_m_czphs();
void CFITS_API wcsGetProp_m_czphs(double* value, size_t len);
void CFITS_API wcsSetProp_m_czphs(double* value, size_t len);

// Get/set property: m_cperi (double*)
int CFITS_API wcsGetPropLen_m_cperi();
void CFITS_API wcsGetProp_m_cperi(double* value, size_t len);
void CFITS_API wcsSetProp_m_cperi(double* value, size_t len);


//</code>


#endif
