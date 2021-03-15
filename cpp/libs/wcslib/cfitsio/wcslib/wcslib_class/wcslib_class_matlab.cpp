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
#include "wcslib_class_matlab.h"


static Wcslib* wcs = NULL;

static void log(const std::string& s)
{
	Wcslib::log(s);
}


//===========================================================================
//                          Test The C Interface
//===========================================================================

static bool _b = false;
static int _i = 0;
static double _d = 0;
static int32_t _imat[100] = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
static int _imat_len = 10;

static double _dmat[100] = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
static int _dmat_len = 10;

static char _str[100] = "MyString";


char const* tstStringCopy(char const* str)
{
    static char s[256];
    strcpy(s, str);
    strcat(s, "_copy");

    return s;
}


bool tstGetBool()
{
    return _b;
}

bool tstSetBool(bool value)
{
    _b = value;
    return true;
}

int tstGetInt()
{
    return _i;
}

bool tstSetInt(int value) 
{
    _i = value;
    return true;
}

double tstGetDouble()
{
    return _d;
}

bool tstSetDouble(double value)
{
    _d = value;
    return true;
}


char const* tstGetStr()
{
    return _str;
}

bool tstSetStr(const char* str)
{
    strcpy(_str, str);
    return true;
}

void tstGetIntMat(int32_t* mat, size_t len)
{
    len = (size_t)_imat_len;
    for (int i = 0; i < len; i++)
        mat[i] = _imat[i] + 100;

}

void tstSetIntMat(int32_t* mat, size_t len)
{
    _imat_len = (int)len;
    for (int i = 0; i < len; i++)
        _imat[i] = mat[i];

}


void tstGetDoubleMat(double* mat, size_t len)
{
    len = (size_t)_imat_len;
    for (int i = 0; i < len; i++)
        mat[i] = _dmat[i] + 1000;

}

void tstSetDoubleMat(double* mat, size_t len)
{
    _imat_len = (int)len;
    for (int i = 0; i < len; i++)
        _dmat[i] = mat[i];

}

//===========================================================================
//
//===========================================================================
bool wcsInit()
{
    if (wcs) delete wcs;
    wcs = new Wcslib;
    return true;
}


bool wcsIsValid()
{
    return (wcs != NULL);
}


bool wcsReadFits(const char* fname)
{
    wcsInit();
    return wcs->readFits(fname);
}


bool wcsReadHeader(const char* fname)
{
    wcsInit();
    return wcs->readHeader(fname);
}


void wcsPixToSky(double* x, double* y, size_t len)
{
    if (!wcsIsValid())
        return;

    double sx, sy;
    for (int i = 0; i < len; i++) {
        wcs->pixToSky(x[i], y[i], sx, sy);
        x[i] = sx;
        y[i] = sy;
    }
}


void wcsSkyToPix(double* x, double* y, size_t len)
{
    if (!wcsIsValid())
        return;

    double px, py;
    for (int i = 0; i < len; i++) {
        wcs->skyToPix(x[i], y[i], px, py);
        x[i] = px;
        y[i] = py;
    }
}


#define prm() ((wcsprm*)wcs->wcsi_ptr)

#define STRINGIFY(s) #s

//===========================================================================
//                      Simple Types: int, double
//===========================================================================
#define GET_PROP_INT(prop) \
    int value = 0; \
    if (wcsIsValid()) { \
        value = prm()->prop; \
        if (wcs->debugMode) wcs->log(Asprintf("%s: %d", __func__, value)); \
    } \
    return value;


#define GET_PROP_DOUBLE(prop) \
    double value = 0; \
    if (wcsIsValid()) { \
        value = prm()->prop; \
        if (wcs->debugMode) wcs->log(Asprintf("%s: %.3lf", __func__, value)); \
    } \
    return value;


#define SET_PROP_INT(prop, value) \
    bool result = false; \
    if (wcsIsValid()) { \
        prm()->prop = value; \
        if (wcs->debugMode) wcs->log(Asprintf("%s: %d", __func__, value)); \
        result = true; \
    } \
    return result;


#define SET_PROP_DOUBLE(prop, value) \
    bool result = false; \
    if (wcsIsValid()) { \
        prm()->prop = value; \
        if (wcs->debugMode) wcs->log(Asprintf("%s: %.3lf", __func__, value)); \
        result = true; \
    } \
    return result;

//===========================================================================
//                        Matrix: int*, doudle*
//===========================================================================

#define GET_PROP_MAT_LEN(prop) \
    int len = 0; \
    if (wcsIsValid()) { \
        len = 5; \
        if (wcs->debugMode) wcs->log(Asprintf("%s: prop: %s, len: %d", __func__, STRINGIFY(prop), len)); \
    } \
    return len;


#define GET_PROP_INT_MAT(prop, value, len) \
    if (wcsIsValid()) { \
        value = prm()->prop; \
        if (wcs->debugMode) wcs->log(Asprintf("%s: %d", __func__, value)); \
        for (int i=0;  i < len;  i++) value[i] = prm()->prop[i]; \
    } \


#define GET_PROP_DOUBLE_MAT(prop, value, len) \
    if (wcsIsValid()) { \
        if (wcs->debugMode) wcs->log(Asprintf("%s: prop: %s, len: %d", __func__, STRINGIFY(prop), len)); \
        for (int i=0;  i < len;  i++) value[i] = prm()->prop[i]; \
    } \


#define SET_PROP_INT_MAT(prop, value, len) \
    bool result = false; \
    if (wcsIsValid()) { \
        if (wcs->debugMode) wcs->log(Asprintf("%s: prop: %s, len: %d", __func__, STRINGIFY(prop), len)); \
        for (int i=0;  i < len;  i++) prm()->prop[i] = value[i]; \
        result = true; \
    } \


#define SET_PROP_DOUBLE_MAT(prop, value, len) \
    bool result = false; \
    if (wcsIsValid()) { \
        if (wcs->debugMode) wcs->log(Asprintf("%s: prop: %s, len: %d", __func__, STRINGIFY(prop), len)); \
        for (int i=0;  i < len;  i++) prm()->prop[i] = value[i]; \
        result = true; \
    } \

//=================================================================
//                       String: const char*
//=================================================================

#define GET_PROP_STR(prop) \
    static char value[256] = { 0 }; \
    if (wcsIsValid()) { \
        if (wcs->debugMode) wcs->log(Asprintf("%s: %s", __func__, STRINGIFY(prop))); \
        strncpy(value, prm()->prop, sizeof(prm()->prop)); \
        value[sizeof(prm()->prop)] = 0; \
    } \
    return value;


#define SET_PROP_STR(prop, value) \
    if (wcsIsValid()) { \
        if (wcs->debugMode) wcs->log(Asprintf("%s: %s = %s", __func__, STRINGIFY(prop), value)); \
        strncpy(prm()->prop, value, sizeof(prm()->prop)); \
    } \

//===========================================================================
//                  Auto-Generated Code - Setters/Getters
//===========================================================================

//<code>
// Get/set property: flag (int)
int wcsGetProp_flag() { GET_PROP_INT(flag); }
bool wcsSetProp_flag(int value) { SET_PROP_INT(flag, value); }

// Get/set property: naxis (int)
int wcsGetProp_naxis() { GET_PROP_INT(naxis); }
bool wcsSetProp_naxis(int value) { SET_PROP_INT(naxis, value); }

// Get/set property: crpix (double*)
int wcsGetPropLen_crpix() { GET_PROP_MAT_LEN(crpix); }
void wcsGetProp_crpix(double* value, size_t len) { GET_PROP_DOUBLE_MAT(crpix, value, len); }
void wcsSetProp_crpix(double* value, size_t len) { SET_PROP_DOUBLE_MAT(crpix, value, len); }

// Get/set property: pc (double*)
int wcsGetPropLen_pc() { GET_PROP_MAT_LEN(pc); }
void wcsGetProp_pc(double* value, size_t len) { GET_PROP_DOUBLE_MAT(pc, value, len); }
void wcsSetProp_pc(double* value, size_t len) { SET_PROP_DOUBLE_MAT(pc, value, len); }

// Get/set property: cdelt (double*)
int wcsGetPropLen_cdelt() { GET_PROP_MAT_LEN(cdelt); }
void wcsGetProp_cdelt(double* value, size_t len) { GET_PROP_DOUBLE_MAT(cdelt, value, len); }
void wcsSetProp_cdelt(double* value, size_t len) { SET_PROP_DOUBLE_MAT(cdelt, value, len); }

// Get/set property: crval (double*)
int wcsGetPropLen_crval() { GET_PROP_MAT_LEN(crval); }
void wcsGetProp_crval(double* value, size_t len) { GET_PROP_DOUBLE_MAT(crval, value, len); }
void wcsSetProp_crval(double* value, size_t len) { SET_PROP_DOUBLE_MAT(crval, value, len); }

// Get/set property: lonpole (double)
double wcsGetProp_lonpole() { GET_PROP_DOUBLE(lonpole); }
bool wcsSetProp_lonpole(double value) { SET_PROP_DOUBLE(lonpole, value); }

// Get/set property: latpole (double)
double wcsGetProp_latpole() { GET_PROP_DOUBLE(latpole); }
bool wcsSetProp_latpole(double value) { SET_PROP_DOUBLE(latpole, value); }

// Get/set property: restfrq (double)
double wcsGetProp_restfrq() { GET_PROP_DOUBLE(restfrq); }
bool wcsSetProp_restfrq(double value) { SET_PROP_DOUBLE(restfrq, value); }

// Get/set property: restwav (double)
double wcsGetProp_restwav() { GET_PROP_DOUBLE(restwav); }
bool wcsSetProp_restwav(double value) { SET_PROP_DOUBLE(restwav, value); }

// Get/set property: npv (int)
int wcsGetProp_npv() { GET_PROP_INT(npv); }
bool wcsSetProp_npv(int value) { SET_PROP_INT(npv, value); }

// Get/set property: npvmax (int)
int wcsGetProp_npvmax() { GET_PROP_INT(npvmax); }
bool wcsSetProp_npvmax(int value) { SET_PROP_INT(npvmax, value); }

// Get/set property: nps (int)
int wcsGetProp_nps() { GET_PROP_INT(nps); }
bool wcsSetProp_nps(int value) { SET_PROP_INT(nps, value); }

// Get/set property: npsmax (int)
int wcsGetProp_npsmax() { GET_PROP_INT(npsmax); }
bool wcsSetProp_npsmax(int value) { SET_PROP_INT(npsmax, value); }

// Get/set property: cd (double*)
int wcsGetPropLen_cd() { GET_PROP_MAT_LEN(cd); }
void wcsGetProp_cd(double* value, size_t len) { GET_PROP_DOUBLE_MAT(cd, value, len); }
void wcsSetProp_cd(double* value, size_t len) { SET_PROP_DOUBLE_MAT(cd, value, len); }

// Get/set property: crota (double*)
int wcsGetPropLen_crota() { GET_PROP_MAT_LEN(crota); }
void wcsGetProp_crota(double* value, size_t len) { GET_PROP_DOUBLE_MAT(crota, value, len); }
void wcsSetProp_crota(double* value, size_t len) { SET_PROP_DOUBLE_MAT(crota, value, len); }

// Get/set property: altlin (int)
int wcsGetProp_altlin() { GET_PROP_INT(altlin); }
bool wcsSetProp_altlin(int value) { SET_PROP_INT(altlin, value); }

// Get/set property: velref (int)
int wcsGetProp_velref() { GET_PROP_INT(velref); }
bool wcsSetProp_velref(int value) { SET_PROP_INT(velref, value); }

// Get/set property: alt (string)
char const* wcsGetProp_alt() { GET_PROP_STR(alt); }
void wcsSetProp_alt(const char* value) { SET_PROP_STR(alt, value); }

// Get/set property: colnum (int)
int wcsGetProp_colnum() { GET_PROP_INT(colnum); }
bool wcsSetProp_colnum(int value) { SET_PROP_INT(colnum, value); }

// Get/set property: colax (int*)
int wcsGetPropLen_colax() { GET_PROP_MAT_LEN(colax); }
void wcsGetProp_colax(int* value, size_t len) { GET_PROP_INT_MAT(colax, value, len); }
void wcsSetProp_colax(int* value, size_t len) { SET_PROP_INT_MAT(colax, value, len); }

// Get/set property: crder (double*)
int wcsGetPropLen_crder() { GET_PROP_MAT_LEN(crder); }
void wcsGetProp_crder(double* value, size_t len) { GET_PROP_DOUBLE_MAT(crder, value, len); }
void wcsSetProp_crder(double* value, size_t len) { SET_PROP_DOUBLE_MAT(crder, value, len); }

// Get/set property: csyer (double*)
int wcsGetPropLen_csyer() { GET_PROP_MAT_LEN(csyer); }
void wcsGetProp_csyer(double* value, size_t len) { GET_PROP_DOUBLE_MAT(csyer, value, len); }
void wcsSetProp_csyer(double* value, size_t len) { SET_PROP_DOUBLE_MAT(csyer, value, len); }

// Get/set property: czphs (double*)
int wcsGetPropLen_czphs() { GET_PROP_MAT_LEN(czphs); }
void wcsGetProp_czphs(double* value, size_t len) { GET_PROP_DOUBLE_MAT(czphs, value, len); }
void wcsSetProp_czphs(double* value, size_t len) { SET_PROP_DOUBLE_MAT(czphs, value, len); }

// Get/set property: cperi (double*)
int wcsGetPropLen_cperi() { GET_PROP_MAT_LEN(cperi); }
void wcsGetProp_cperi(double* value, size_t len) { GET_PROP_DOUBLE_MAT(cperi, value, len); }
void wcsSetProp_cperi(double* value, size_t len) { SET_PROP_DOUBLE_MAT(cperi, value, len); }

// Get/set property: wcsname (string)
char const* wcsGetProp_wcsname() { GET_PROP_STR(wcsname); }
void wcsSetProp_wcsname(const char* value) { SET_PROP_STR(wcsname, value); }

// Get/set property: timesys (string)
char const* wcsGetProp_timesys() { GET_PROP_STR(timesys); }
void wcsSetProp_timesys(const char* value) { SET_PROP_STR(timesys, value); }

// Get/set property: trefpos (string)
char const* wcsGetProp_trefpos() { GET_PROP_STR(trefpos); }
void wcsSetProp_trefpos(const char* value) { SET_PROP_STR(trefpos, value); }

// Get/set property: trefdir (string)
char const* wcsGetProp_trefdir() { GET_PROP_STR(trefdir); }
void wcsSetProp_trefdir(const char* value) { SET_PROP_STR(trefdir, value); }

// Get/set property: plephem (string)
char const* wcsGetProp_plephem() { GET_PROP_STR(plephem); }
void wcsSetProp_plephem(const char* value) { SET_PROP_STR(plephem, value); }

// Get/set property: timeunit (string)
char const* wcsGetProp_timeunit() { GET_PROP_STR(timeunit); }
void wcsSetProp_timeunit(const char* value) { SET_PROP_STR(timeunit, value); }

// Get/set property: dateref (string)
char const* wcsGetProp_dateref() { GET_PROP_STR(dateref); }
void wcsSetProp_dateref(const char* value) { SET_PROP_STR(dateref, value); }

// Get/set property: mjdref (double*)
int wcsGetPropLen_mjdref() { return 2; }
void wcsGetProp_mjdref(double* value, size_t len) { GET_PROP_DOUBLE_MAT(mjdref, value, len); }
void wcsSetProp_mjdref(double* value, size_t len) { SET_PROP_DOUBLE_MAT(mjdref, value, len); }

// Get/set property: timeoffs (double)
double wcsGetProp_timeoffs() { GET_PROP_DOUBLE(timeoffs); }
bool wcsSetProp_timeoffs(double value) { SET_PROP_DOUBLE(timeoffs, value); }

// Get/set property: dateobs (string)
char const* wcsGetProp_dateobs() { GET_PROP_STR(dateobs); }
void wcsSetProp_dateobs(const char* value) { SET_PROP_STR(dateobs, value); }

// Get/set property: datebeg (string)
char const* wcsGetProp_datebeg() { GET_PROP_STR(datebeg); }
void wcsSetProp_datebeg(const char* value) { SET_PROP_STR(datebeg, value); }

// Get/set property: dateavg (string)
char const* wcsGetProp_dateavg() { GET_PROP_STR(dateavg); }
void wcsSetProp_dateavg(const char* value) { SET_PROP_STR(dateavg, value); }

// Get/set property: dateend (string)
char const* wcsGetProp_dateend() { GET_PROP_STR(dateend); }
void wcsSetProp_dateend(const char* value) { SET_PROP_STR(dateend, value); }

// Get/set property: mjdobs (double)
double wcsGetProp_mjdobs() { GET_PROP_DOUBLE(mjdobs); }
bool wcsSetProp_mjdobs(double value) { SET_PROP_DOUBLE(mjdobs, value); }

// Get/set property: mjdbeg (double)
double wcsGetProp_mjdbeg() { GET_PROP_DOUBLE(mjdbeg); }
bool wcsSetProp_mjdbeg(double value) { SET_PROP_DOUBLE(mjdbeg, value); }

// Get/set property: mjdavg (double)
double wcsGetProp_mjdavg() { GET_PROP_DOUBLE(mjdavg); }
bool wcsSetProp_mjdavg(double value) { SET_PROP_DOUBLE(mjdavg, value); }

// Get/set property: mjdend (double)
double wcsGetProp_mjdend() { GET_PROP_DOUBLE(mjdend); }
bool wcsSetProp_mjdend(double value) { SET_PROP_DOUBLE(mjdend, value); }

// Get/set property: jepoch (double)
double wcsGetProp_jepoch() { GET_PROP_DOUBLE(jepoch); }
bool wcsSetProp_jepoch(double value) { SET_PROP_DOUBLE(jepoch, value); }

// Get/set property: bepoch (double)
double wcsGetProp_bepoch() { GET_PROP_DOUBLE(bepoch); }
bool wcsSetProp_bepoch(double value) { SET_PROP_DOUBLE(bepoch, value); }

// Get/set property: tstart (double)
double wcsGetProp_tstart() { GET_PROP_DOUBLE(tstart); }
bool wcsSetProp_tstart(double value) { SET_PROP_DOUBLE(tstart, value); }

// Get/set property: tstop (double)
double wcsGetProp_tstop() { GET_PROP_DOUBLE(tstop); }
bool wcsSetProp_tstop(double value) { SET_PROP_DOUBLE(tstop, value); }

// Get/set property: xposure (double)
double wcsGetProp_xposure() { GET_PROP_DOUBLE(xposure); }
bool wcsSetProp_xposure(double value) { SET_PROP_DOUBLE(xposure, value); }

// Get/set property: telapse (double)
double wcsGetProp_telapse() { GET_PROP_DOUBLE(telapse); }
bool wcsSetProp_telapse(double value) { SET_PROP_DOUBLE(telapse, value); }

// Get/set property: timsyer (double)
double wcsGetProp_timsyer() { GET_PROP_DOUBLE(timsyer); }
bool wcsSetProp_timsyer(double value) { SET_PROP_DOUBLE(timsyer, value); }

// Get/set property: timrder (double)
double wcsGetProp_timrder() { GET_PROP_DOUBLE(timrder); }
bool wcsSetProp_timrder(double value) { SET_PROP_DOUBLE(timrder, value); }

// Get/set property: timedel (double)
double wcsGetProp_timedel() { GET_PROP_DOUBLE(timedel); }
bool wcsSetProp_timedel(double value) { SET_PROP_DOUBLE(timedel, value); }

// Get/set property: timepixr (double)
double wcsGetProp_timepixr() { GET_PROP_DOUBLE(timepixr); }
bool wcsSetProp_timepixr(double value) { SET_PROP_DOUBLE(timepixr, value); }

// Get/set property: obsgeo (double*)
int wcsGetPropLen_obsgeo() { return 6; }
void wcsGetProp_obsgeo(double* value, size_t len) { GET_PROP_DOUBLE_MAT(obsgeo, value, len); }
void wcsSetProp_obsgeo(double* value, size_t len) { SET_PROP_DOUBLE_MAT(obsgeo, value, len); }

// Get/set property: obsorbit (string)
char const* wcsGetProp_obsorbit() { GET_PROP_STR(obsorbit); }
void wcsSetProp_obsorbit(const char* value) { SET_PROP_STR(obsorbit, value); }

// Get/set property: radesys (string)
char const* wcsGetProp_radesys() { GET_PROP_STR(radesys); }
void wcsSetProp_radesys(const char* value) { SET_PROP_STR(radesys, value); }

// Get/set property: equinox (double)
double wcsGetProp_equinox() { GET_PROP_DOUBLE(equinox); }
bool wcsSetProp_equinox(double value) { SET_PROP_DOUBLE(equinox, value); }

// Get/set property: specsys (string)
char const* wcsGetProp_specsys() { GET_PROP_STR(specsys); }
void wcsSetProp_specsys(const char* value) { SET_PROP_STR(specsys, value); }

// Get/set property: ssysobs (string)
char const* wcsGetProp_ssysobs() { GET_PROP_STR(ssysobs); }
void wcsSetProp_ssysobs(const char* value) { SET_PROP_STR(ssysobs, value); }

// Get/set property: velosys (double)
double wcsGetProp_velosys() { GET_PROP_DOUBLE(velosys); }
bool wcsSetProp_velosys(double value) { SET_PROP_DOUBLE(velosys, value); }

// Get/set property: zsource (double)
double wcsGetProp_zsource() { GET_PROP_DOUBLE(zsource); }
bool wcsSetProp_zsource(double value) { SET_PROP_DOUBLE(zsource, value); }

// Get/set property: ssyssrc (string)
char const* wcsGetProp_ssyssrc() { GET_PROP_STR(ssyssrc); }
void wcsSetProp_ssyssrc(const char* value) { SET_PROP_STR(ssyssrc, value); }

// Get/set property: velangl (double)
double wcsGetProp_velangl() { GET_PROP_DOUBLE(velangl); }
bool wcsSetProp_velangl(double value) { SET_PROP_DOUBLE(velangl, value); }

// Get/set property: ntab (int)
int wcsGetProp_ntab() { GET_PROP_INT(ntab); }
bool wcsSetProp_ntab(int value) { SET_PROP_INT(ntab, value); }

// Get/set property: nwtb (int)
int wcsGetProp_nwtb() { GET_PROP_INT(nwtb); }
bool wcsSetProp_nwtb(int value) { SET_PROP_INT(nwtb, value); }

// Get/set property: lngtyp (string)
char const* wcsGetProp_lngtyp() { GET_PROP_STR(lngtyp); }
void wcsSetProp_lngtyp(const char* value) { SET_PROP_STR(lngtyp, value); }

// Get/set property: lattyp (string)
char const* wcsGetProp_lattyp() { GET_PROP_STR(lattyp); }
void wcsSetProp_lattyp(const char* value) { SET_PROP_STR(lattyp, value); }

// Get/set property: lng (int)
int wcsGetProp_lng() { GET_PROP_INT(lng); }
bool wcsSetProp_lng(int value) { SET_PROP_INT(lng, value); }

// Get/set property: lat (int)
int wcsGetProp_lat() { GET_PROP_INT(lat); }
bool wcsSetProp_lat(int value) { SET_PROP_INT(lat, value); }

// Get/set property: spec (int)
int wcsGetProp_spec() { GET_PROP_INT(spec); }
bool wcsSetProp_spec(int value) { SET_PROP_INT(spec, value); }

// Get/set property: cubeface (int)
int wcsGetProp_cubeface() { GET_PROP_INT(cubeface); }
bool wcsSetProp_cubeface(int value) { SET_PROP_INT(cubeface, value); }

// Get/set property: types (int*)
int wcsGetPropLen_types() { GET_PROP_MAT_LEN(types); }
void wcsGetProp_types(int* value, size_t len) { GET_PROP_INT_MAT(types, value, len); }
void wcsSetProp_types(int* value, size_t len) { SET_PROP_INT_MAT(types, value, len); }

// Get/set property: m_flag (int)
int wcsGetProp_m_flag() { GET_PROP_INT(m_flag); }
bool wcsSetProp_m_flag(int value) { SET_PROP_INT(m_flag, value); }

// Get/set property: m_naxis (int)
int wcsGetProp_m_naxis() { GET_PROP_INT(m_naxis); }
bool wcsSetProp_m_naxis(int value) { SET_PROP_INT(m_naxis, value); }

// Get/set property: m_crpix (double*)
int wcsGetPropLen_m_crpix() { GET_PROP_MAT_LEN(m_crpix); }
void wcsGetProp_m_crpix(double* value, size_t len) { GET_PROP_DOUBLE_MAT(m_crpix, value, len); }
void wcsSetProp_m_crpix(double* value, size_t len) { SET_PROP_DOUBLE_MAT(m_crpix, value, len); }

// Get/set property: m_pc (double*)
int wcsGetPropLen_m_pc() { GET_PROP_MAT_LEN(m_pc); }
void wcsGetProp_m_pc(double* value, size_t len) { GET_PROP_DOUBLE_MAT(m_pc, value, len); }
void wcsSetProp_m_pc(double* value, size_t len) { SET_PROP_DOUBLE_MAT(m_pc, value, len); }

// Get/set property: m_cdelt (double*)
int wcsGetPropLen_m_cdelt() { GET_PROP_MAT_LEN(m_cdelt); }
void wcsGetProp_m_cdelt(double* value, size_t len) { GET_PROP_DOUBLE_MAT(m_cdelt, value, len); }
void wcsSetProp_m_cdelt(double* value, size_t len) { SET_PROP_DOUBLE_MAT(m_cdelt, value, len); }

// Get/set property: m_crval (double*)
int wcsGetPropLen_m_crval() { GET_PROP_MAT_LEN(m_crval); }
void wcsGetProp_m_crval(double* value, size_t len) { GET_PROP_DOUBLE_MAT(m_crval, value, len); }
void wcsSetProp_m_crval(double* value, size_t len) { SET_PROP_DOUBLE_MAT(m_crval, value, len); }

// Get/set property: m_cd (double*)
int wcsGetPropLen_m_cd() { GET_PROP_MAT_LEN(m_cd); }
void wcsGetProp_m_cd(double* value, size_t len) { GET_PROP_DOUBLE_MAT(m_cd, value, len); }
void wcsSetProp_m_cd(double* value, size_t len) { SET_PROP_DOUBLE_MAT(m_cd, value, len); }

// Get/set property: m_crota (double*)
int wcsGetPropLen_m_crota() { GET_PROP_MAT_LEN(m_crota); }
void wcsGetProp_m_crota(double* value, size_t len) { GET_PROP_DOUBLE_MAT(m_crota, value, len); }
void wcsSetProp_m_crota(double* value, size_t len) { SET_PROP_DOUBLE_MAT(m_crota, value, len); }

// Get/set property: m_colax (int*)
int wcsGetPropLen_m_colax() { GET_PROP_MAT_LEN(m_colax); }
void wcsGetProp_m_colax(int* value, size_t len) { GET_PROP_INT_MAT(m_colax, value, len); }
void wcsSetProp_m_colax(int* value, size_t len) { SET_PROP_INT_MAT(m_colax, value, len); }

// Get/set property: m_crder (double*)
int wcsGetPropLen_m_crder() { GET_PROP_MAT_LEN(m_crder); }
void wcsGetProp_m_crder(double* value, size_t len) { GET_PROP_DOUBLE_MAT(m_crder, value, len); }
void wcsSetProp_m_crder(double* value, size_t len) { SET_PROP_DOUBLE_MAT(m_crder, value, len); }

// Get/set property: m_csyer (double*)
int wcsGetPropLen_m_csyer() { GET_PROP_MAT_LEN(m_csyer); }
void wcsGetProp_m_csyer(double* value, size_t len) { GET_PROP_DOUBLE_MAT(m_csyer, value, len); }
void wcsSetProp_m_csyer(double* value, size_t len) { SET_PROP_DOUBLE_MAT(m_csyer, value, len); }

// Get/set property: m_czphs (double*)
int wcsGetPropLen_m_czphs() { GET_PROP_MAT_LEN(m_czphs); }
void wcsGetProp_m_czphs(double* value, size_t len) { GET_PROP_DOUBLE_MAT(m_czphs, value, len); }
void wcsSetProp_m_czphs(double* value, size_t len) { SET_PROP_DOUBLE_MAT(m_czphs, value, len); }

// Get/set property: m_cperi (double*)
int wcsGetPropLen_m_cperi() { GET_PROP_MAT_LEN(m_cperi); }
void wcsGetProp_m_cperi(double* value, size_t len) { GET_PROP_DOUBLE_MAT(m_cperi, value, len); }
void wcsSetProp_m_cperi(double* value, size_t len) { SET_PROP_DOUBLE_MAT(m_cperi, value, len); }


//</code>
