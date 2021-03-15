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
  $Id: tspcaips.c,v 7.3.1.2 2020/08/17 11:20:48 mcalabre Exp mcalabre $
*=============================================================================
*
* tspcaips does a quick test of spcaips().  Not part of the official test
* suite.
*
*---------------------------------------------------------------------------*/

#include <stdio.h>
#include <spc.h>

int main()

{
  const char *(ctypes[]) = {"FREQ", "VELO", "FELO"};
  const char *(frames[]) = {"-LSR", "-HEL", "-OBS", "    "};

  char ctype[9], ctypeA[9], specsys[9];
  int  i, j, status, v1, v2, velref;

  for (i = 0; i < 3; i++) {
    for (j = 0; j < 4; j++) {
      sprintf(ctypeA, "%s%s", ctypes[i], frames[j]);

      for (v1 = 0; v1 <= 8; v1++) {
        velref = v1;
        for (v2 = 0; v2 < 3; v2++) {
          status = spcaips(ctypeA, velref, ctype, specsys);
          printf("'%s'  %3d  %2d  '%s'  '%s'\n", ctypeA, velref,
            status, ctype, specsys);
          velref += 256;
        }
      }

      printf("\n");
    }
  }

  return 0;
}
