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

  Author: Mark Calabretta, Australia Telescope National Facility, CSIRO,
     and: Michael Droetboom, Space Telescope Science Institute
  http://www.atnf.csiro.au/people/Mark.Calabretta
  $Id: twcscompare.c,v 7.3.1.2 2020/08/17 11:20:48 mcalabre Exp mcalabre $
*=============================================================================
*
* Test wcscompare().
*
*---------------------------------------------------------------------------*/

#include <stdio.h>
#include <string.h>

#include <wcs.h>


int main()

{
  int    cmp, equal, status;
  struct wcsprm a, b;

  printf(
    "Testing WCSLIB comparison routine (twcscompare.c)\n"
    "-------------------------------------------------\n");

  a.flag = -1;
  b.flag = -1;
  wcsini(1, 2, &a);
  wcsini(1, 2, &b);

  if ((status = wcscompare(0, 0.0, &a, &b, &equal))) {
    printf("wcscompare ERROR %d: %s.\n", status, wcs_errmsg[status]);
    return 1;
  } else if (!equal) {
    printf("FAIL: Equal wcsprms tested unequal.\n");
    return 1;
  }

  strncpy(b.dateobs, "2014-01-01T00:00:00", 72);

  if ((status = wcscompare(0, 0.0, &a, &b, &equal))) {
    printf("wcscompare ERROR %d: %s.\n", status, wcs_errmsg[status]);
    return 1;
  } else if (equal) {
    printf("FAIL: Unequal wcsprms tested equal.\n");
    return 1;
  }

  if ((status = wcscompare(WCSCOMPARE_ANCILLARY, 0.0, &a, &b, &equal))) {
    printf("wcscompare ERROR %d: %s.\n", status, wcs_errmsg[status]);
    return 1;
  } else if (!equal) {
    printf("FAIL: Ancillary keyword not ignored.\n");
    return 1;
  }

  b.crpix[0] = 12.5;
  b.crpix[1] = 12.5;

  if ((status = wcscompare(WCSCOMPARE_ANCILLARY, 0.0, &a, &b, &equal))) {
    printf("wcscompare ERROR %d: %s.\n", status, wcs_errmsg[status]);
    return 1;
  } else if (equal) {
    printf("FAIL: Unequal wcsprms tested equal.\n");
    return 1;
  }

  cmp = WCSCOMPARE_ANCILLARY | WCSCOMPARE_TILING;
  if ((status = wcscompare(cmp, 0.0, &a, &b, &equal))) {
    printf("wcscompare ERROR %d: %s.\n", status, wcs_errmsg[status]);
    return 1;
  } else if (equal) {
    printf("FAIL: Non-integral translation equates as a tiling.\n");
    return 1;
  }

  cmp = WCSCOMPARE_ANCILLARY | WCSCOMPARE_CRPIX;
  if ((status = wcscompare(cmp, 0.0, &a, &b, &equal))) {
    printf("wcscompare ERROR %d: %s.\n", status, wcs_errmsg[status]);
    return 1;
  } else if (!equal) {
    printf("FAIL: Translation not ignored.\n");
    return 1;
  }

  printf("\nPASS: All comparisons returned as expected.\n");

  wcsfree(&a);
  wcsfree(&b);

  return 0;
}
