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
  $Id: twcstab.c,v 7.3.1.2 2020/08/17 11:20:48 mcalabre Exp mcalabre $
*=============================================================================
*
* twcstab tests wcstab() and also provides sample code for using it in
* conjunction with wcspih() and fits_read_wcstab().  Although this example
* and fits_read_wcstab() are based on the CFITSIO library, wcstab() itself
* is completely independent of it.
*
* The input FITS file is constructed by create_input() from a list of header
* keyrecords in wcstab.keyrec together with some hard-coded parameters.  The
* output fits file, wcstab.fits, is left for inspection.
*
*===========================================================================*/

#include <math.h>
#include <stdio.h>
#include <string.h>

#include <fitsio.h>

#include <wcslib.h>
#include <getwcstab.h>

int create_input();
int do_wcs_stuff(fitsfile *fptr, struct wcsprm *wcs);
double lcprng();

int main()

{
  char *header;
  int  i, nkeyrec, nreject, nwcs, stat[NWCSFIX], status = 0;
  fitsfile *fptr;
  struct wcsprm *wcs;


  // Set line buffering in case stdout is redirected to a file, otherwise
  // stdout and stderr messages will be jumbled (stderr is unbuffered).
  setvbuf(stdout, NULL, _IOLBF, 0);

  printf("Testing -TAB interpreter (twcstab.c)\n"
         "------------------------------------\n\n");

  // Create the input FITS test file.
  if (create_input()) {
    fprintf(stderr, "Failed to create FITS test file.");
    return 1;
  }

  // Open the FITS test file and read the primary header.
  fits_open_file(&fptr, "wcstab.fits", READONLY, &status);
  if ((status = fits_hdr2str(fptr, 1, NULL, 0, &header, &nkeyrec,
                             &status))) {
    fits_report_error(stderr, status);
    return 1;
  }


  //-------------------------------------------------------------------------
  // Basic steps required to interpret a FITS WCS header, including -TAB.
  //-------------------------------------------------------------------------

  // Parse the primary header of the FITS file.
  if ((status = wcspih(header, nkeyrec, WCSHDR_all, 2, &nreject, &nwcs,
                       &wcs))) {
    fprintf(stderr, "wcspih ERROR %d: %s.\n", status,wcshdr_errmsg[status]);
  }

  // Read coordinate arrays from the binary table extension.
  if ((status = fits_read_wcstab(fptr, wcs->nwtb, (wtbarr *)wcs->wtb,
                                 &status))) {
    fits_report_error(stderr, status);
    return 1;
  }

  // Translate non-standard WCS keyvalues.
  if ((status = wcsfix(7, 0, wcs, stat))) {
    for (i = 0; i < NWCSFIX; i++) {
      if (stat[i] > 0) {
        fprintf(stderr, "wcsfix ERROR %d: %s.\n", status,
                wcsfix_errmsg[stat[i]]);
      }
    }

    return 1;
  }

  //-------------------------------------------------------------------------
  // The wcsprm struct is now ready for use.
  //-------------------------------------------------------------------------

  // Do something with it.
  do_wcs_stuff(fptr, wcs);

  // Finished with the FITS file.
  fits_close_file(fptr, &status);
  fits_free_memory(header, &status);

  // Clean up.
  status = wcsvfree(&nwcs, &wcs);

  return 0;
}

//----------------------------------------------------------------------------

// The celestial plane is 256 x 128 with the table indexed at every fourth
// pixel on each axis, but the image is rotated by 5 deg so the table needs
// to be a bit wider than 65 x 33.

#define K1 70L
#define K2 40L

int create_input()

{
  const double TWOPI = 2.0 * 3.14159265358979323846;

  // These must match wcstab.keyrec.
  const char infile[] = "test/wcstab.keyrec";
  const long NAXIS1 = 256;
  const long NAXIS2 = 128;
  const long NAXIS3 =   4;
  const char *ttype1[3] = {"CelCoords", "RAIndex", "DecIndex"};
  const char *tform1[3] = {"5600E", "70E", "40E"};
  const char *tunit1[3] = {"deg", "", ""};
  const char *ttype2[4] = {"WaveIndex", "WaveCoord",
                           "TimeIndex", "TimeCoord"};
  const char *tform2[4] = {"8E", "8D", "8E", "8D"};
  const char *tunit2[4] = {"", "m", "", "a"};

  // The remaining parameters may be chosen at will.
  float  refval[] = {150.0f, -30.0f};
  float  span[]   = {5.0f, (5.0f*K2)/K1};
  float  sigma[]  = {0.10f, 0.05f};
  double wcoord[] = {0.21106114, 0.21076437, 2.0e-6, 2.2e-6,
                     500.0e-9, 650.0e-9, 1.24e-9, 2.48e-9};
  double windex[] = {0.5, 1.5, 1.5, 2.5, 2.5, 3.5, 3.5, 4.5};
  double tcoord[] = {1997.84512, 1997.84631, 1993.28451, 1993.28456,
                     2001.59234, 2001.59239, 2002.18265, 2002.18301};
  double tindex[] = {0.0, 1.0, 1.0, 2.0, 2.0, 3.0, 3.0, 4.0};

  char   keyrec[84];
  int    status;
  size_t iz;
  long   dummy, firstelem, k1, k2, p1, p2, p3;
  float  array[2*K1*K2], *fp, image[256];
  double s, t, x1, x2, z, z1, z2;
  FILE   *stream;
  fitsfile *fptr;

  // Look for the input header keyrecords.
  if ((stream = fopen(infile+5, "r")) == 0x0) {
    if ((stream = fopen(infile, "r")) == 0x0) {
      printf("ERROR opening %s\n", infile);
      return 1;
    }
  }

  // Create the FITS output file, deleting any pre-existing file.
  status = 0;
  fits_create_file(&fptr, "!wcstab.fits", &status);

  // Convert header keyrecords to FITS.
  while (fgets(keyrec, 82, stream) != NULL) {
    // Ignore meta-comments (copyright information, etc.).
    if (keyrec[0] == '#') continue;

    // Strip off the newline.
    iz = strlen(keyrec) - 1;
    if (keyrec[iz] == '\n') keyrec[iz] = '\0';

    fits_write_record(fptr, keyrec, &status);
  }
  fclose(stream);

  // Create and write some phoney image data.
  firstelem = 1;
  for (p3 = 0; p3 < NAXIS3; p3++) {
    for (p2 = 0; p2 < NAXIS2; p2++) {
      fp = image;
      s = (p3 + 1) * cos(0.2 * p2);
      t = cos(0.8*p2);
      for (p1 = 0; p1 < NAXIS1; p1++) {
        // Do not adjust your set!
        *(fp++) = sin(0.1*(p1+p2) + s) * cos(0.4*p1) * t;
      }

      fits_write_img_flt(fptr, 0L, firstelem, NAXIS1, image, &status);
      firstelem += NAXIS1;
    }
  }


  // Add the first binary table extension.
  fits_create_tbl(fptr, BINARY_TBL, 1L, 3L, (char **)ttype1, (char **)tform1,
     (char **)tunit1, NULL, &status);

  // Write EXTNAME and EXTVER near the top, after TFIELDS.
  fits_read_key_lng(fptr, "TFIELDS", &dummy, NULL, &status);
  fits_insert_key_str(fptr, "EXTNAME", "WCS-TABLE",
    "WCS Coordinate lookup table", &status);
  fits_insert_key_lng(fptr, "EXTVER", 1L, "Table number 1", &status);

  // Write the TDIM1 keyrecord after TFORM1.
  fits_read_key_str(fptr, "TFORM1", keyrec, NULL, &status);
  sprintf(keyrec, "(2,%ld,%ld)", K1, K2);
  fits_insert_key_str(fptr, "TDIM1", keyrec, "Dimensions of 3-D array",
    &status);

  // Plate carrée projection with a bit of noise for the sake of realism.
  fp = array;
  for (k2 = 0; k2 < K2; k2++) {
    for (k1 = 0; k1 < K1; k1++) {
      // Box-Muller transformation: uniform -> normal distribution.
      x1 = lcprng();
      x2 = lcprng();
      if (x1 == 0.0) x1 = 1.0;
      z  = sqrt(-2.0 * log(x1));
      x2 *= TWOPI;
      z1 = z * cos(x2);
      z2 = z * sin(x2);

      *(fp++) = refval[0] + span[0] * (k1/(K1-1.0) - 0.5) + z1 * sigma[0];
      *(fp++) = refval[1] + span[1] * (k2/(K2-1.0) - 0.5) + z2 * sigma[1];
    }
  }
  fits_write_col_flt(fptr, 1, 1L, 1L, 2*K1*K2, array, &status);

  fp = array;
  for (k1 = 0; k1 < K1; k1++) {
    *(fp++) = 4.0f * k1;
  }
  fits_write_col_flt(fptr, 2, 1L, 1L, K1, array, &status);

  fp = array;
  for (k2 = 0; k2 < K2; k2++) {
    *(fp++) = 4.0f * k2;
  }
  fits_write_col_flt(fptr, 3, 1L, 1L, K2, array, &status);


  // Add the second binary table extension.
  if (fits_create_tbl(fptr, BINARY_TBL, 1L, 4L, (char **)ttype2,
     (char **)tform2, (char **)tunit2, NULL, &status)) {
    fits_report_error(stderr, status);
    return 1;
  }

  // Write EXTNAME and EXTVER near the top, after TFIELDS.
  fits_read_key_lng(fptr, "TFIELDS", &dummy, NULL, &status);
  fits_insert_key_str(fptr, "EXTNAME", "WCS-TABLE",
    "WCS Coordinate lookup table", &status);
  fits_insert_key_lng(fptr, "EXTVER", 2L, "Table number 2", &status);

  // Write the TDIM2 keyrecord after TFORM2.
  fits_read_key_str(fptr, "TFORM2", keyrec, NULL, &status);
  fits_insert_key_str(fptr, "TDIM2", "(1,8)", "Dimensions of 2-D array",
    &status);

  // Write the TDIM4 keyrecord after TFORM4.
  fits_read_key_str(fptr, "TFORM4", keyrec, NULL, &status);
  fits_insert_key_str(fptr, "TDIM4", "(1,8)", "Dimensions of 2-D array",
    &status);


  fits_write_col_dbl(fptr, 1, 1L, 1L, 8L, windex, &status);
  fits_write_col_dbl(fptr, 2, 1L, 1L, 8L, wcoord, &status);
  fits_write_col_dbl(fptr, 3, 1L, 1L, 8L, tindex, &status);
  fits_write_col_dbl(fptr, 4, 1L, 1L, 8L, tcoord, &status);

  fits_close_file(fptr, &status);

  if (status) {
    fits_report_error(stderr, status);
    return 1;
  }

  return 0;
}

/*----------------------------------------------------------------------------
* A simple linear congruential pseudo-random number generator that produces
* the same results on all systems so that the test output can be compared.
* It produces a fixed sequence of uniformly distributed numbers in [0,1].
* Adapted from the example in Numerical Recipes in C.
*---------------------------------------------------------------------------*/

double lcprng()
{
  static unsigned long next = 137UL;

  next = next * 1664525UL + 1013904223UL;
  return (double)(next % 1073741824UL) / 1073741823.0;
}

//----------------------------------------------------------------------------

int do_wcs_stuff(fitsfile *fptr, struct wcsprm *wcs)

{
  int    i1, i2, i3, k, naxis1, naxis2, naxis3, stat[8], status;
  double phi[8], pixcrd[8][4], imgcrd[8][4], theta[8], world[8][4],
         x1, x2, x3;

  // Initialize the wcsprm struct, also taking control of memory allocated by
  // fits_read_wcstab().
  if ((status = wcssetprm(wcs))) {
    fprintf(stderr, "wcsset ERROR %d: %s.\n", status, wcs_errmsg[status]);
    return 1;
  }

  // Print the struct.
  if ((status = wcsprt(wcs))) return status;

  // Compute coordinates in the corners.
  fits_read_key(fptr, TINT, "NAXIS1", &naxis1, NULL, &status);
  fits_read_key(fptr, TINT, "NAXIS2", &naxis2, NULL, &status);
  fits_read_key(fptr, TINT, "NAXIS3", &naxis3, NULL, &status);

  k = 0;
  x3 = 1.0f;
  for (i3 = 0; i3 < 2; i3++) {
    x2 = 0.5f;

    for (i2 = 0; i2 < 2; i2++) {
      x1 = 0.5f;

      for (i1 = 0; i1 < 2; i1++) {
        pixcrd[k][0] = x1;
        pixcrd[k][1] = x2;
        pixcrd[k][2] = x3;
        pixcrd[k][3] = 1.0f;

        k++;
        x1 = naxis1 + 0.5f;
      }

      x2 = naxis2 + 0.5f;
    }

    x3 = naxis3;
  }

  if ((status = wcsp2s(wcs, 8, 4, pixcrd[0], imgcrd[0], phi, theta, world[0],
                       stat))) {
    fprintf(stderr, "\n\nwcsp2s ERROR %d: %s.\n", status,
            wcs_errmsg[status]);

    // Invalid pixel coordinates.
    if (status == 8) status = 0;
  }

  if (status == 0) {
    printf("\n\nCorner world coordinates:\n"
           "            Pixel                              World\n");
    for (k = 0; k < 8; k++) {
      printf("  (%5.1f,%6.1f,%4.1f,%4.1f) -> (%7.3f,%8.3f,%9g,%11.5f)",
             pixcrd[k][0], pixcrd[k][1], pixcrd[k][2], pixcrd[k][3],
             world[k][0],  world[k][1],  world[k][2],  world[k][3]);
      if (stat[k]) printf("  (BAD)");
      printf("\n");
    }
  }

  return 0;
}
