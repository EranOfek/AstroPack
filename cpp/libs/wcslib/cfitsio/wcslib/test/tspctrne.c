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
  $Id: tspctrne.c,v 7.3.1.2 2020/08/17 11:20:48 mcalabre Exp mcalabre $
*=============================================================================
*
* tspctrne does a quick test of spctrne().  Not part of the official test
* suite.
*
*---------------------------------------------------------------------------*/

#include <stdio.h>
#include <string.h>

#include <spc.h>
#include <wcserr.h>

int main()

{
  const char ctypeS1[] = "VOPT-F2W";
  const double crvalS1 = 1e6;
  const double cdeltS1 = 1e3;
  const double restfrq = 0.0;
  const double restwav = 0.0;

  int    status;
  char   ctypeS2[9];
  double cdeltS2, crvalS2;
  struct wcserr *err;

  strcpy(ctypeS2, "VRAD-???");

  wcserr_enable(1);
  if (spctrne(ctypeS1, crvalS1, cdeltS1, restfrq, restwav,
              ctypeS2, &crvalS2, &cdeltS2, &err)) {
    wcserr_prt(err, 0x0);
    return err->status;
  }

  printf("'%8s'  %12.6e  %12.6e\n'%8s'  %12.6e  %12.6e\n",
    ctypeS1, crvalS1, cdeltS1, ctypeS2, crvalS2, cdeltS2);

  return 0;
}
