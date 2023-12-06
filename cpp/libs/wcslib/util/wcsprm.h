// Original wcsprm
struct wcsprm {
  // Initialization flag (see the prologue above).
  //--------------------------------------------------------------------------
  int    flag;			// Set to zero to force initialization.

  // FITS header keyvalues to be provided (see the prologue above).
  //--------------------------------------------------------------------------
  int    naxis;			// Number of axes (pixel and coordinate).
  double *crpix;		// CRPIXja keyvalues for each pixel axis.
  double *pc;			// PCi_ja  linear transformation matrix.
  double *cdelt;		// CDELTia keyvalues for each coord axis.
  double *crval;		// CRVALia keyvalues for each coord axis.

  char   (*cunit)[72];		// CUNITia keyvalues for each coord axis.
  char   (*ctype)[72];		// CTYPEia keyvalues for each coord axis.

  double lonpole;		// LONPOLEa keyvalue.
  double latpole;		// LATPOLEa keyvalue.

  double restfrq;		// RESTFRQa keyvalue.
  double restwav;		// RESTWAVa keyvalue.

  int    npv;			// Number of PVi_ma keywords, and the
  int    npvmax;		// number for which space was allocated.
  struct pvcard *pv;		// PVi_ma keywords for each i and m.

  int    nps;			// Number of PSi_ma keywords, and the
  int    npsmax;		// number for which space was allocated.
  struct pscard *ps;		// PSi_ma keywords for each i and m.

  // Alternative header keyvalues (see the prologue above).
  //--------------------------------------------------------------------------
  double *cd;			// CDi_ja linear transformation matrix.
  double *crota;		// CROTAi keyvalues for each coord axis.
  int    altlin;		// Alternative representations
				//   Bit 0: PCi_ja  is present,
				//   Bit 1: CDi_ja  is present,
				//   Bit 2: CROTAi is present.
  int    velref;		// AIPS velocity code, VELREF.

  // Auxiliary coordinate system information of a general nature.  Not
  // used by WCSLIB.  Refer to the prologue comments above for a brief
  // explanation of these values.
  char   alt[4];
  int    colnum;
  int    *colax;
				// Auxiliary coordinate axis information.
  char   (*cname)[72];
  double *crder;
  double *csyer;
  double *czphs;
  double *cperi;

  char   wcsname[72];
				// Time reference system and measurement.
  char   timesys[72], trefpos[72], trefdir[72], plephem[72];
  char   timeunit[72];
  char   dateref[72];
  double mjdref[2];
  double timeoffs;
				// Data timestamps and durations.
  char   dateobs[72], datebeg[72], dateavg[72], dateend[72];
  double mjdobs, mjdbeg, mjdavg, mjdend;
  double jepoch, bepoch;
  double tstart, tstop;
  double xposure, telapse;
				// Timing accuracy.
  double timsyer, timrder;
  double timedel, timepixr;
				// Spatial & celestial reference frame.
  double obsgeo[6];
  char   obsorbit[72];
  char   radesys[72];
  double equinox;
  char   specsys[72];
  char   ssysobs[72];
  double velosys;
  double zsource;
  char   ssyssrc[72];
  double velangl;

  // Additional auxiliary coordinate system information of a specialist
  // nature.  Not used by WCSLIB.  Refer to the prologue comments above.
  struct auxprm *aux;

  // Coordinate lookup tables (see the prologue above).
  //--------------------------------------------------------------------------
  int    ntab;			// Number of separate tables.
  int    nwtb;			// Number of wtbarr structs.
  struct tabprm *tab;		// Tabular transformation parameters.
  struct wtbarr *wtb;		// Array of wtbarr structs.

  //--------------------------------------------------------------------------
  // Information derived from the FITS header keyvalues by wcsset().
  //--------------------------------------------------------------------------
  char   lngtyp[8], lattyp[8];	// Celestial axis types, e.g. RA, DEC.
  int    lng, lat, spec;	// Longitude, latitude and spectral axis
				// indices (0-relative).
  int    cubeface;		// True if there is a CUBEFACE axis.
  int    *types;		// Coordinate type codes for each axis.

  struct linprm lin;		//    Linear transformation parameters.
  struct celprm cel;		// Celestial transformation parameters.
  struct spcprm spc;		//  Spectral transformation parameters.

  //--------------------------------------------------------------------------
  //             THE REMAINDER OF THE WCSPRM STRUCT IS PRIVATE.
  //--------------------------------------------------------------------------

  // Error handling, if enabled.
  //--------------------------------------------------------------------------
  struct wcserr *err;

  // Memory management.
  //--------------------------------------------------------------------------
  int    m_flag, m_naxis;
  double *m_crpix, *m_pc, *m_cdelt, *m_crval;
  char  (*m_cunit)[72], (*m_ctype)[72];
  struct pvcard *m_pv;
  struct pscard *m_ps;
  double *m_cd, *m_crota;
  int    *m_colax;
  char  (*m_cname)[72];
  double *m_crder, *m_csyer, *m_czphs, *m_cperi;
  struct auxprm *m_aux;
  struct tabprm *m_tab;
  struct wtbarr *m_wtb;
};
