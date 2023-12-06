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
  $Id: tdisiter.c,v 7.3.1.2 2020/08/17 11:20:48 mcalabre Exp mcalabre $
*=============================================================================
*
* tdisiter tests the external setting of ITERMAX in disx2p(), particularly
* for distortions such as SIP that may optionally provide an approximation
* of their inverses.
*
*---------------------------------------------------------------------------*/

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <wcserr.h>
#include <wcshdr.h>
#include <wcsprintf.h>
#include <dis.h>

int disitermax(int itermax);

#define HEADER_SIZE 36000


int main(int argc, char *argv[])

{
  char *infile = "SIP.fits";

  char keyrec[81], header[288001];
  int  gotend, iblock, ikeyrec, j, k, n, naxis[4], nkeyrec, nreject, nwcs;
  double discrd[4], rawcrd[4];
  FILE   *fptr;
  struct disprm *dis;
  struct wcsprm *wcs;


  wcserr_enable(1);
  wcsprintf_set(stdout);

  // Set line buffering in case stdout is redirected to a file, otherwise
  // stdout and stderr messages will be jumbled (stderr is unbuffered).
  setvbuf(stdout, NULL, _IOLBF, 0);

  // Optional file name specified?
  if (1 < argc) {
    infile = argv[1];
  }


  // Read in the FITS header, excluding COMMENT and HISTORY keyrecords.
  if ((fptr = fopen(infile, "r")) == 0) {
    wcsprintf("ERROR opening %s\n", infile);
    return 1;
  }

  memset(naxis, 0, 2*sizeof(int));

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

      if (strncmp(keyrec, "NAXIS", 5) == 0) {
        if (keyrec[5] == ' ') {
          sscanf(keyrec+10, "%d", &n);
          if (4 < n) {
            wcsprintf("ERROR, can't handle more than 4 axes.\n");
            return 1;
          }
          continue;
        }

        sscanf(keyrec+5, "%d = %d", &j, &n);
        naxis[j-1] = n;
        continue;
      }

      strncpy(header+k, keyrec, 80);
      k += 80;
      nkeyrec++;

      if (strncmp(keyrec, "END       ", 10) == 0) {
        // An END keyrecord was read, but read the rest of the block.
        gotend = 1;
      }
    }

    if (gotend) break;
  }
  fclose(fptr);


  // Parse the header.
  if ((wcspih(header, nkeyrec, WCSHDR_none, 2, &nreject, &nwcs, &wcs))) {
    wcsperr(wcs, 0x0);
    return 1;
  }

  if (wcssetprm(wcs)) {
    wcsperr(wcs, 0x0);
    return 1;
  }

  dis = wcs->lin.dispre;

  disprt(dis);
  wcsprintf("\n");

  rawcrd[0] = 1;
  rawcrd[1] = 1;
  rawcrd[2] = 1;
  rawcrd[3] = 1;
  if (disp2x(dis, rawcrd, discrd)) {
    disperr(dis, 0x0);
    return 1;
  }

  wcsprintf("    pix: %18.12f %18.12f\n", rawcrd[0], rawcrd[1]);
  wcsprintf(" -> img: %18.12f %18.12f\n", discrd[0], discrd[1]);

  disitermax(32);
  if (disx2p(dis, discrd, rawcrd)) {
    disperr(dis, 0x0);
    return 1;
  }

  wcsprintf(" -> pix: %18.12f %18.12f  (ITERMAX == %d)\n",
    rawcrd[0], rawcrd[1], disitermax(-1));

  disitermax(0);
  if (disx2p(dis, discrd, rawcrd)) {
    disperr(dis, 0x0);
    return 1;
  }

  wcsprintf(" -> pix: %18.12f %18.12f  (ITERMAX == %d)\n",
    rawcrd[0], rawcrd[1], disitermax(-1));


  wcsvfree(&nwcs, &wcs);

  return 0;
}
