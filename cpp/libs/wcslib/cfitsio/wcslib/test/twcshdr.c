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
  $Id: twcshdr.c,v 7.3.1.2 2020/08/17 11:20:48 mcalabre Exp mcalabre $
*=============================================================================
*
* twcshdr illustrates the steps required to read WCS information (including
* -TAB coordinates) from a FITS header using the CFITSIO library
*
* Options:
*   -a<alt>:
*       Specify an alternate coordinate representation to be used (ignored if
*       there is only one).  Can also be specified as a 0-relative index in
*       the range 0 to 26, where the alternates are sequenced alphabetically
*       following the primary representation.
*
*   -h: Uses wcshdo() to translate the wcsprm struct into a FITS header and
*       prints it.
*
*   -p: Asks the user for a pixel coordinate which it transforms to world
*       coordinates and prints.
*
*   -w: Asks the user for a world coordinate which it transforms to pixel
*       coordinates and prints.
*
* If none of the above options are specified it uses wcsprt() to print the
* wcsprm struct itself.
*
* Input comes from a user-specified FITS file.
*
*===========================================================================*/

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "fitsio.h"
#include "wcslib.h"
#include "getwcstab.h"

int main_twchdr(int argc, char *argv[])

{
  char *alt = 0x0, *header, *hptr;
  int  dohdr = 0, dopixel = 0, doworld = 0;
  int  alts[27], i, ialt, nkeyrec, nreject, nwcs, stat[NWCSFIX], status = 0;
  double imgcrd[2], phi, pixcrd[2], theta, world[2];
  fitsfile *fptr;
  struct wcsprm *wcs, *wcsi;


  // Parse options.
  for (i = 1; i < argc && argv[i][0] == '-'; i++) {
    if (!argv[i][1]) break;

    switch (argv[i][1]) {
    case 'a':
      // Select an alternate WCS.
      alt = argv[i]+2;
      break;

    case 'h':
      // Print header using wcshdo().
      dohdr = 1;
      break;

    case 'p':
      // Transform pixel coordinate.
      dopixel = 1;
      break;

    case 'w':
      // Transform world coordinate.
      doworld = 1;
      break;

    default:
      fprintf(stderr, "Usage: twcshdr [-a<alt>] [-h | -p | -w] <file>\n");
      return 1;
    }
  }

  if (i != (argc-1)) {
    fprintf(stderr, "Usage: twcshdr [-h | -p | -w] <file>\n");
    return 1;
  }

  // Open the FITS test file and read the primary header.
  fits_open_file(&fptr, argv[i], READONLY, &status);
  if ((status = fits_hdr2str(fptr, 1, NULL, 0, &header, &nkeyrec, &status))) {
    fits_report_error(stderr, status);
    return 1;
  }


  //-------------------------------------------------------------------------
  // Basic steps required to interpret a FITS WCS header, including -TAB.
  //-------------------------------------------------------------------------

  // Parse the primary header of the FITS file.
  if ((status = wcspih(header, nkeyrec, WCSHDR_all, -3, &nreject, &nwcs,
                       &wcs))) {
    fprintf(stderr, "wcspih ERROR %d: %s.\n", status, get_wcshdr_errmsg()[status]);
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
                get_wcsfix_errmsg()[stat[i]]);
      }
    }

    return 1;
  }

  // Sort out alternates.
  i = 0;
  if (alt) {
    if ('0' <= *alt && *alt <= '9') {
      if ((i = atoi(alt)) > nwcs-1) {
        wcsfprintf(stderr, "WARNING, no alternate coordinate "
          "representation \"%s\".\n", alt);
        return 1;
      }

    } else {
      wcsidx(nwcs, &wcs, alts);

      ialt = toupper(*alt);
      if (strlen(alt) > 1) {
        wcsfprintf(stderr, "WARNING, alternate specifier \"%s\" is "
          "invalid.\n", alt);
        return 1;

      } else if (*alt == ' ') {
        if (alts[0] == -1) {
          wcsfprintf(stderr, "WARNING, no primary coordinate "
            "representation.\n");
          return 1;
        }

      } else if (ialt < 'A' || ialt > 'Z') {
        wcsfprintf(stderr, "WARNING, alternate specifier \"%s\" is "
          "invalid.\n", alt);
        return 1;

      } else {
        if ((i = alts[ialt - 'A' + 1]) == -1) {
          wcsfprintf(stderr, "WARNING, no alternate coordinate "
            "representation \"%s\".\n", alt);
          return 1;
        }
      }
    }
  }

  wcsi = wcs + i;

  //-------------------------------------------------------------------------
  // The wcsprm struct is now ready for use.
  //-------------------------------------------------------------------------

  // Finished with the FITS file.
  fits_close_file(fptr, &status);
  fits_free_memory(header, &status);

  // Initialize the wcsprm struct, also taking control of memory allocated by
  // fits_read_wcstab().
  if ((status = wcssetprm(wcsi))) {
    fprintf(stderr, "wcsset ERROR %d: %s.\n", status, get_wcs_errmsg()[status]);
    return 1;
  }

  if (dohdr) {
    if ((status = wcshdo(WCSHDO_all, wcsi, &nkeyrec, &header))) {
      return 1;
    }

    hptr = header;
    printf("\n");
    for (i = 0; i < nkeyrec; i++, hptr += 80) {
      printf("%.80s\n", hptr);
    }

    wcsdealloc(header);

  } else if (dopixel) {
    while (1) {
      printf("Enter pixel coordinates: ");
      if (scanf("%lf%*[ ,]%lf", pixcrd, pixcrd+1) != wcsi->naxis) break;
      status = wcsp2s(wcsi, 1, 2, pixcrd, imgcrd, &phi, &theta, world, stat);
      printf("  (%20.15f, %20.15f) ->\n  (%20.15f, %20.15f)\n\n",
        pixcrd[0], pixcrd[1], world[0], world[1]);
    }

  } else if (doworld) {
    while (1) {
      printf("Enter world coordinates: ");
      if (scanf("%lf%*[ ,]%lf", world, world+1) != wcsi->naxis) break;
      status = wcss2p(wcsi, 1, 2, world, &phi, &theta, imgcrd, pixcrd, stat);
      printf("  (%20.15f, %20.15f) ->\n  (%20.15f, %20.15f)\n\n",
        world[0], world[1], pixcrd[0], pixcrd[1]);
    }

  } else {
    // Print the struct.
    if ((status = wcsprt(wcsi))) {
      return 1;
    }
  }

  // Clean up.
  status = wcsvfree(&nwcs, &wcs);

  return 0;
}
