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
  $Id: tbth1.c,v 7.3.1.2 2020/08/17 11:20:48 mcalabre Exp mcalabre $
*=============================================================================
*
* tbth1 tests wcsbth(), the WCS FITS parser for binary table headers, and
* wcsfix(), which translates non-standard constructs.  It reads a test header
* and uses wcsprt() to print the resulting wcsprm structs.
*
* Input comes from file "bth.fits" using either fits_hdr2str() from CFITSIO
* if the DO_CFITSIO preprocessor is defined, or read directly using fgets()
* otherwise.
*
*---------------------------------------------------------------------------*/

//#include <wcsconfig_tests.h>

#include <stdio.h>
#include <string.h>

#if defined HAVE_CFITSIO && defined DO_CFITSIO
#include <fitsio.h>
#endif

#include <wcs.h>
#include <wcshdr.h>
#include <wcsfix.h>


int main_tbth1()

{
  char  infile[] = "D:\\Ultrasat\\wcs\\Images\\ztf_20190709485764_000600_zg_c04_o_q4_sciimg.fits";  //"bth.fits";
  char  a, *hptr;
  short alts[1000][28];
  int   colsel[8], ctrl, ialt, iblock, icol, ifix, ikeyrec, iwcs, keysel,
        nkeyrec, nreject, nwcs, relax, stat[NWCSFIX], status;
  struct wcsprm *wcs;
#if defined HAVE_CFITSIO && defined DO_CFITSIO
  char *header;
  fitsfile *fptr;
#else
  char keyrec[81], header[288001];
  int  gotend, k;
  FILE *fptr;
#endif


  // Set line buffering in case stdout is redirected to a file, otherwise
  // stdout and stderr messages will be jumbled (stderr is unbuffered).
  //setvbuf(stdout, NULL, _IOLBF, 0);

  printf("Testing WCSLIB parser for FITS binary table headers (tbth1.c)\n"
         "-------------------------------------------------------------\n\n");

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
  if ((fptr = fopen(infile, "r")) == 0) {
    fprintf(stderr, "ERROR opening %s\n", infile);
    return 1;
  }

  k = 0;
  nkeyrec = 0;
  gotend = 0;
  for (iblock = 0; iblock < 100; iblock++) {
    for (ikeyrec = 0; ikeyrec < 36; ikeyrec++) {
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

  fprintf(stderr, "Found %d non-comment header keyrecords.\n\n", nkeyrec);


  // Parse the header, allowing all recognized non-standard WCS keywords and
  // usage.  WCS keyrecords that are used are culled from the header, illegal
  // ones are reported.
  relax = WCSHDR_all;
  ctrl  = -2;
  keysel = 0;
  colsel[0] = 0;

  fprintf(stderr, "\nIllegal or extraneous WCS header keyrecords rejected "
                  "by wcsbth():\n");
  if ((status = wcsbth(header, nkeyrec, relax, ctrl, keysel, colsel,
                       &nreject, &nwcs, &wcs))) {
    fprintf(stderr, "wcsbth ERROR %d: %s.\n", status, get_wcs_errmsg()[status]);
  }
  if (!nreject) fprintf(stderr, "(none)\n");


  // List the remaining keyrecords.
  printf("\n\nNon-WCS header keyrecords ignored by wcsbth():\n");
  hptr = header;
  while (*hptr) {
    printf("%.80s\n", hptr);
    hptr += 80;
  }
#if defined HAVE_CFITSIO && defined DO_CFITSIO
  fits_free_memory(header, &status);
#endif


  // Summarize what was found.
  printf("\n\nExtracted %d coordinate description%s:\n", nwcs,
    (nwcs == 1) ? "" : "s");

  printf("\n  Unattached image header(s)");
  status = wcsbdx(nwcs, &wcs, 0, alts);
  if (alts[0][27]) {
    printf(" with indices:\n        -");
    for (a = 'A'; a <= 'Z'; a++) {
      printf("%2c", a);
    }

    printf("\n       ");
    for (ialt = 0; ialt < 27; ialt++) {
      if (alts[0][ialt] < 0) {
         printf(" -");
      } else {
         printf("%2d", alts[0][ialt]);
      }
    }
    printf("\n");
  } else {
    printf(": (none)\n");
  }


  printf("\n  Binary table image array(s)");
  for (icol = 1; icol <= 999; icol++) {
    if (alts[icol][27]) {
      printf(" with indices:\n  Col.  -");
      for (a = 'A'; a <= 'Z'; a++) {
        printf("%2c", a);
      }
      printf("\n");

      for (icol = 1; icol <= 999; icol++) {
        for (ialt = 0; ialt < 27; ialt++) {
          if (alts[icol][ialt] >= 0) {
            printf("%5d: ", icol);
            for (ialt = 0; ialt < 27; ialt++) {
              if (alts[icol][ialt] < 0) {
                printf(" -");
              } else {
                printf("%2d", alts[icol][ialt]);
              }
            }
            printf("\n");
            break;
          }
        }
      }

      icol = 9999;
    }
  }

  if (icol < 9999) {
    printf(": (none)\n");
  }


  printf("\n  Pixel list(s)");
  status = wcsbdx(nwcs, &wcs, 1, alts);
  for (icol = 1; icol <= 999; icol++) {
    if (alts[icol][27]) {
      printf(" with indices:\n  Col.  -");
      for (a = 'A'; a <= 'Z'; a++) {
        printf("%2c", a);
      }
      printf("\n");

      for (icol = 1; icol <= 999; icol++) {
        for (ialt = 0; ialt < 27; ialt++) {
          if (alts[icol][ialt] >= 0) {
            printf("%5d: ", icol);
            for (ialt = 0; ialt < 27; ialt++) {
              if (alts[icol][ialt] < 0) {
                printf(" -");
              } else {
                printf("%2d", alts[icol][ialt]);
              }
            }
            printf("\n");
            break;
          }
        }
      }

      icol = 9999;
    }
  }

  if (icol < 9999) {
    printf(": (none)\n");
  }


  // Fix non-standard usage and print each of the wcsprm structs.
  for (iwcs = 0; iwcs < nwcs; iwcs++) {
    printf("\n------------------------------------"
           "------------------------------------\n");

    // Fix non-standard WCS keyvalues.
    if ((status = wcsfix(7, 0, wcs+iwcs, stat))) {
      printf("wcsfix ERROR, status returns: (");
      for (ifix = 0; ifix < NWCSFIX; ifix++) {
        printf(ifix ? ", %d" : "%d", stat[ifix]);
      }
      printf(")\n\n");
    }

    if ((status = wcssetprm(wcs+iwcs))) {
      fprintf(stderr, "wcsset ERROR %d: %s.\n", status, get_wcs_errmsg()[status]);
      continue;
    }

    if ((status = wcsprt(wcs+iwcs))) {
      fprintf(stderr, "wcsprt ERROR %d: %s.\n", status, get_wcs_errmsg()[status]);
    }
  }

  status = wcsvfree(&nwcs, &wcs);


  // Do it again to check that wcsbth() can handle multiple invokations.
  printf("\nInvoking wcsbth() a second time on the same header...\n");
  ctrl = 0;
  if ((status = wcsbth(header, nkeyrec, relax, 0, keysel, colsel, &nreject,
                       &nwcs, &wcs))) {
    fprintf(stderr, "wcsbth ERROR %d: %s.\n", status, get_wcs_errmsg()[status]);
  } else {
    printf("OK, extracted %d coordinate description%s.\n", nwcs,
        (nwcs == 1) ? "" : "s");
  }

  return 0;
}
