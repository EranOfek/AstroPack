/*============================================================================
  WCSLIB 7.3 - an implementation of the FITS WCS standard.
  Copyright (C) 1995-2020, Mark Calabretta

  This file is part of WCSLIB.

  WCSLIB is free software: you can redistribute it and/or modify it under the
  terms of the GNU Lesser General Public License as published by the Free
  Software Foundation, either version 3 of the License, or (at your option)
  any later version.

  WCSLIB is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for
  more details.

  You should have received a copy of the GNU Lesser General Public License
  along with WCSLIB.  If not, see http://www.gnu.org/licenses.

  Direct correspondence concerning WCSLIB to mark@calabretta.id.au

  Author: Mark Calabretta, Australia Telescope National Facility, CSIRO.
  http://www.atnf.csiro.au/people/Mark.Calabretta
  $Id: tpih2.c,v 7.3.1.2 2020/08/17 11:20:48 mcalabre Exp mcalabre $
*=============================================================================
*
* tpih2 tests wcspih(), the WCS FITS parser for image headers, by reading a
* test header and using pgsbox() to plot coordinate graticules for the
* resulting wcsprm structs.
*
* Input comes from file "pih.fits" using either fits_hdr2str() from CFITSIO
* if the DO_CFITSIO preprocessor is defined, or read directly using fgets()
* otherwise.
*
*---------------------------------------------------------------------------*/

#include <wcsconfig_tests.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#if defined HAVE_CFITSIO && defined DO_CFITSIO
#include <fitsio.h>
#endif

#include <cpgplot.h>
#include <cpgsbox.h>

#include <wcshdr.h>
#include <wcs.h>

int main()

{
  char infile[] = "pih.fits";
  char devtyp[16], idents[3][80], nlcprm[1], opt[2];
  int  c0[] = {-1, -1, -1, -1, -1, -1, -1};
  int  i, ic, gcode[2], naxis[2], nkeyrec, nreject, nwcs, relax, status;
  float  blc[2], trc[2];
  double cache[257][4], grid1[1], grid2[1], nldprm[1];
  struct wcsprm *wcs;
  nlfunc_t pgwcsl_;
#if defined HAVE_CFITSIO && defined DO_CFITSIO
  char *header;
  fitsfile *fptr;
#else
  char keyrec[81], header[28801];
  int  gotend, j, k;
  FILE *fptr;
#endif


  // Set line buffering in case stdout is redirected to a file, otherwise
  // stdout and stderr messages will be jumbled (stderr is unbuffered).
  setvbuf(stdout, NULL, _IOLBF, 0);

  printf("Testing WCSLIB parser for FITS image headers (tpih2.c)\n"
         "------------------------------------------------------\n\n");

  // Read in the FITS header, excluding COMMENT and HISTORY keyrecords.
#if defined HAVE_CFITSIO && defined DO_CFITSIO
  status = 0;

  if (fits_open_file(&fptr, infile, READONLY, &status)) {
    fits_report_error(stderr, status);
    return 1;
  }

  if (fits_hdr2str(fptr, 1, NULL, 0, &header, &nkeyrec, &status)) {
    fits_report_error(stderr, status);
    return 1;
  }

  fits_close_file(fptr, &status);
#else
  if ((fptr = fopen(infile, "r")) == 0x0) {
    printf("ERROR opening %s\n", infile);
    return 1;
  }

  k = 0;
  nkeyrec = 0;
  gotend = 0;
  for (j = 0; j < 10; j++) {
    for (i = 0; i < 36; i++) {
      if (fgets(keyrec, 81, fptr) == 0) {
        break;
      }

      if (strncmp(keyrec, "        ", 8) == 0) continue;
      if (strncmp(keyrec, "COMMENT ", 8) == 0) continue;
      if (strncmp(keyrec, "HISTORY ", 8) == 0) continue;

      memcpy(header+k, keyrec, 80);
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
#endif

  fprintf(stderr, "Found %d non-comment header keyrecords.\n", nkeyrec);

  relax = WCSHDR_all;
  if ((status = wcspih(header, nkeyrec, relax, 2, &nreject, &nwcs, &wcs))) {
    fprintf(stderr, "wcspih ERROR %d: %s.\n", status, wcs_errmsg[status]);
  }
#if defined HAVE_CFITSIO && defined DO_CFITSIO
  fits_free_memory(header, &status);
#endif

  // Plot setup.
  naxis[0] = 1024;
  naxis[1] = 1024;

  blc[0] = 0.5f;
  blc[1] = 0.5f;
  trc[0] = naxis[0] + 0.5f;
  trc[1] = naxis[1] + 0.5f;

  strcpy(devtyp, "/XWINDOW");
  cpgbeg(0, devtyp, 1, 1);
  cpgvstd();

  cpgwnad(0.0f, 1.0f, 0.0f, 1.0f);
  cpgask(1);
  cpgpage();

  // Annotation.
  strcpy(idents[0], "Right ascension");
  strcpy(idents[1], "Declination");

  opt[0] = 'G';
  opt[1] = 'E';

  // Compact lettering.
  cpgsch(0.8f);

  // Draw full grid lines.
  cpgsci(1);
  gcode[0] = 2;
  gcode[1] = 2;
  grid1[0] = 0.0;
  grid2[0] = 0.0;

  for (i = 0; i < nwcs; i++) {
    if ((status = wcssetprm(wcs+i))) {
      fprintf(stderr, "wcsset ERROR %d: %s.\n", status, wcs_errmsg[status]);
      continue;
    }

    // Get WCSNAME out of the wcsprm struct.
    strcpy(idents[2], (wcs+i)->wcsname);
    printf("\n%s\n", idents[2]);

    // Draw the celestial grid.  The grid density is set for each world
    // coordinate by specifying LABDEN = 1224.
    ic = -1;
    cpgsbox(blc, trc, idents, opt, 0, 1224, c0, gcode, 0.0, 0, grid1, 0,
      grid2, 0, pgwcsl_, 1, WCSLEN, 1, nlcprm, (int *)(wcs+i), nldprm, 256,
      &ic, cache, &status);

    // Draw the frame.
    cpgbox("BC", 0.0f, 0, "BC", 0.0f, 0);

    cpgpage();
  }

  status = wcsvfree(&nwcs, &wcs);

  cpgask(0);
  cpgend();

  return 0;
}
