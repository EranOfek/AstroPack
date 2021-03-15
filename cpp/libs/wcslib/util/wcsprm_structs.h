

not implemented yet:   struct pvcard *pv;		// PVi_ma keywords for each i and m.
not implemented yet:   struct pscard *ps;		// PSi_ma keywords for each i and m.
not implemented yet:   struct auxprm *aux;
not implemented yet:   struct tabprm *tab;		// Tabular transformation parameters.
not implemented yet:   struct wtbarr *wtb;		// Array of wtbarr structs.
not implemented yet:   struct linprm lin;		//    Linear transformation parameters.
not implemented yet:   struct celprm cel;		// Celestial transformation parameters.
not implemented yet:   struct spcprm spc;		//  Spectral transformation parameters.
not implemented yet:   struct wcserr *err;
not implemented yet:   struct pvcard *m_pv;
not implemented yet:   struct pscard *m_ps;
not implemented yet:   struct auxprm *m_aux;
not implemented yet:   struct tabprm *m_tab;
not implemented yet:   struct wtbarr *m_wtb;




// Struct used for storing PVi_ma keywords.
struct pvcard {
  int i;			// Axis number, as in PVi_ma (1-relative).
  int m;			// Parameter number, ditto  (0-relative).
  double value;			// Parameter value.
};

// Size of the pvcard struct in int units, used by the Fortran wrappers.
#define PVLEN (sizeof(struct pvcard)/sizeof(int))

// Struct used for storing PSi_ma keywords.
struct pscard {
  int i;			// Axis number, as in PSi_ma (1-relative).
  int m;			// Parameter number, ditto  (0-relative).
  char value[72];		// Parameter value.
};

// Size of the pscard struct in int units, used by the Fortran wrappers.
#define PSLEN (sizeof(struct pscard)/sizeof(int))

// Struct used to hold additional auxiliary parameters.
struct auxprm {
  double rsun_ref;              // Solar radius.
  double dsun_obs;              // Distance from Sun centre to observer.
  double crln_obs;              // Carrington heliographic lng of observer.
  double hgln_obs;              // Stonyhurst heliographic lng of observer.
  double hglt_obs;              // Heliographic latitude of observer.
};

// Size of the auxprm struct in int units, used by the Fortran wrappers.
#define AUXLEN (sizeof(struct auxprm)/sizeof(int))



struct wcserr {
  int  status;			// Status code for the error.
  int  line_no;			// Line number where the error occurred.
  const char *function;		// Function name.
  const char *file;		// Source file name.
  char *msg;			// Informative error message.
};





struct tabprm {
  // Initialization flag (see the prologue above).
  //--------------------------------------------------------------------------
  int    flag;			// Set to zero to force initialization.

  // Parameters to be provided (see the prologue above).
  //--------------------------------------------------------------------------
  int    M;			// Number of tabular coordinate axes.
  int    *K;			// Vector of length M whose elements
				// (K_1, K_2,... K_M) record the lengths of
				// the axes of the coordinate array and of
				// each indexing vector.
  int    *map;			// Vector of length M usually such that
				// map[m-1] == i-1 for coordinate array
				// axis m and image axis i (see above).
  double *crval;		// Vector of length M containing the index
				// value for the reference pixel for each
				// of the tabular coordinate axes.
  double **index;		// Vector of pointers to M indexing vectors
				// of lengths (K_1, K_2,... K_M).
  double *coord;		// (1+M)-dimensional tabular coordinate
				// array (see above).

  // Information derived from the parameters supplied.
  //--------------------------------------------------------------------------
  int    nc;			// Number of coordinate vectors (of length
				// M) in the coordinate array.
  int    padding;		// (Dummy inserted for alignment purposes.)
  int    *sense;		// Vector of M flags that indicate whether
				// the Mth indexing vector is monotonic
				// increasing, or else decreasing.
  int    *p0;			// Vector of M indices.
  double *delta;		// Vector of M increments.
  double *extrema;		// (1+M)-dimensional array of coordinate
				// extrema.

  // Error handling
  //--------------------------------------------------------------------------
  struct wcserr *err;

  // Private - the remainder are for memory management.
  //--------------------------------------------------------------------------
  int    m_flag, m_M, m_N;
  int    set_M;
  int    *m_K, *m_map;
  double *m_crval, **m_index, **m_indxs, *m_coord;
};





struct wtbarr {
  int  i;			// Image axis number.
  int  m;			// Array axis number for index vectors.
  int  kind;			// wcstab array type.
  char extnam[72];		// EXTNAME of binary table extension.
  int  extver;			// EXTVER  of binary table extension.
  int  extlev;			// EXTLEV  of binary table extension.
  char ttype[72];		// TTYPEn of column containing the array.
  long row;			// Table row number.
  int  ndim;			// Expected wcstab array dimensionality.
  int  *dimlen;			// Where to write the array axis lengths.
  double **arrayp;		// Where to write the address of the array
				// allocated to store the wcstab array.
};



struct linprm {
  // Initialization flag (see the prologue above).
  //--------------------------------------------------------------------------
  int flag;			// Set to zero to force initialization.

  // Parameters to be provided (see the prologue above).
  //--------------------------------------------------------------------------
  int naxis;			// The number of axes, given by NAXIS.
  double *crpix;		// CRPIXja keywords for each pixel axis.
  double *pc;			// PCi_ja  linear transformation matrix.
  double *cdelt;		// CDELTia keywords for each coord axis.
  struct disprm *dispre;	// Prior   distortion parameters, if any.
  struct disprm *disseq;	// Sequent distortion parameters, if any.

  // Information derived from the parameters supplied.
  //--------------------------------------------------------------------------
  double *piximg;		// Product of CDELTia and PCi_ja matrices.
  double *imgpix;		// Inverse of the piximg matrix.
  int    i_naxis;		// Dimension of piximg and imgpix.
  int    unity;			// True if the PCi_ja matrix is unity.
  int    affine;		// True if there are no distortions.
  int    simple;		// True if unity and no distortions.

  // Error handling, if enabled.
  //--------------------------------------------------------------------------
  struct wcserr *err;

  // Private - the remainder are for internal use.
  //--------------------------------------------------------------------------
  double *tmpcrd;

  int    m_flag, m_naxis;
  double *m_crpix, *m_pc, *m_cdelt;
  struct disprm *m_dispre, *m_disseq;
};


struct celprm {
  // Initialization flag (see the prologue above).
  //--------------------------------------------------------------------------
  int    flag;			// Set to zero to force initialization.

  // Parameters to be provided (see the prologue above).
  //--------------------------------------------------------------------------
  int    offset;		// Force (x,y) = (0,0) at (phi_0,theta_0).
  double phi0, theta0;		// Native coordinates of fiducial point.
  double ref[4];		// Celestial coordinates of fiducial
                                // point and native coordinates of
                                // celestial pole.

  struct prjprm prj;		// Projection parameters (see prj.h).

  // Information derived from the parameters supplied.
  //--------------------------------------------------------------------------
  double euler[5];		// Euler angles and functions thereof.
  int    latpreq;		// LATPOLEa requirement.
  int    isolat;		// True if |latitude| is preserved.

  // Error handling
  //--------------------------------------------------------------------------
  struct wcserr *err;

  // Private
  //--------------------------------------------------------------------------
  void   *padding;		// (Dummy inserted for alignment purposes.)
};

// Size of the celprm struct in int units, used by the Fortran wrappers.
#define CELLEN (sizeof(struct celprm)/sizeof(int))





struct spcprm {
  // Initialization flag (see the prologue above).
  //--------------------------------------------------------------------------
  int    flag;			// Set to zero to force initialization.

  // Parameters to be provided (see the prologue above).
  //--------------------------------------------------------------------------
  char   type[8];		// Four-letter spectral variable type.
  char   code[4];		// Three-letter spectral algorithm code.

  double crval;			// Reference value (CRVALia), SI units.
  double restfrq;		// Rest frequency, Hz.
  double restwav;		// Rest wavelength, m.

  double pv[7];			// Grism parameters:
				//   0: G, grating ruling density.
				//   1: m, interference order.
				//   2: alpha, angle of incidence.
				//   3: n_r, refractive index at lambda_r.
				//   4: n'_r, dn/dlambda at lambda_r.
				//   5: epsilon, grating tilt angle.
				//   6: theta, detector tilt angle.

  // Information derived from the parameters supplied.
  //--------------------------------------------------------------------------
  double w[6];			// Intermediate values.
				//   0: Rest frequency or wavelength (SI).
				//   1: CRVALX (SI units).
				//   2: CDELTX/CDELTia = dX/dS (SI units).
				// The remainder are grism intermediates.

  int    isGrism;		// Grism coordinates?  1: vacuum, 2: air.
  int    padding1;		// (Dummy inserted for alignment purposes.)

  // Error handling
  //--------------------------------------------------------------------------
  struct wcserr *err;

  // Private
  //--------------------------------------------------------------------------
  void   *padding2;		// (Dummy inserted for alignment purposes.)
  int (*spxX2P)(SPX_ARGS);	// Pointers to the transformation functions
  int (*spxP2S)(SPX_ARGS);	// in the two-step algorithm chain in the
				// pixel-to-spectral direction.

  int (*spxS2P)(SPX_ARGS);	// Pointers to the transformation functions
  int (*spxP2X)(SPX_ARGS);	// in the two-step algorithm chain in the
				// spectral-to-pixel direction.
};

