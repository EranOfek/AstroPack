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
  $Id: tofits.c,v 7.3.1.2 2020/08/17 11:20:48 mcalabre Exp mcalabre $
*=============================================================================
*
* tofits turns a list of FITS header keyrecords, one per line, into a proper
* FITS header by padding them with blanks to 80 characters and stripping out
* newline characters.  It also pads the header to an integral number of 2880-
* byte blocks if necessary.
*
* It operates as a filter, e.g.:
*
*     tofits < infile > outfile
*
* Input lines beginning with '#' are treated as comments.
*
*===========================================================================*/

#include <stdio.h>

int main()

{
  int c, i = 0, nkeyrec = 0;

  while ((c = getchar()) != EOF) {
    if (c == '\n') {
      // Blank-fill the keyrecord.
      while (i++ < 80) {
        putchar(' ');
      }
      i = 0;
      nkeyrec++;

    } else if (c == '#' && i == 0) {
      // Discard comments.
      while ((c = getchar()) != EOF) {
        if (c == '\n') break;
      }

    } else {
      putchar(c);
      i++;
    }
  }

  // Pad to a multiple of 2880-bytes.
  if (nkeyrec %= 36) {
    while (nkeyrec++ < 36) {
      i = 0;
      while (i++ < 80) {
        putchar(' ');
      }
    }
  }

  return 0;
}
