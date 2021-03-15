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
  $Id: tspx.c,v 7.3.1.2 2020/08/17 11:20:48 mcalabre Exp mcalabre $
*=============================================================================
*
* tspx tests the spectral transformation routines for closure.
*
*---------------------------------------------------------------------------*/

#include <math.h>
#include <stdio.h>

#include <spx.h>

#define NSPEC 9991

const double C = 2.99792458e8;
const double tol = 1.0e-9;

int closure(const char *from, const char *to, double parm,
            int (*fwd)(SPX_ARGS), int (*rev)(SPX_ARGS), const double spec1[],
            double spec2[]);


int main()

{
  int    nFail = 0, stat[NSPEC], status;
  double restfrq, restwav, step;
  double awav[NSPEC], freq[NSPEC], spc1[NSPEC], spc2[NSPEC], velo[NSPEC],
         wave[NSPEC];
  struct spxprm spx;

  register int j, k;


  printf(
    "Testing closure of WCSLIB spectral transformation routines (tspx.c)\n"
    "-------------------------------------------------------------------\n");

  // List status return messages.
  printf("\nList of spx status return values:\n");
  for (status = 1; status <= 4; status++) {
    printf("%4d: %s.\n", status, spx_errmsg[status]);
  }

  restfrq = 1420.40595e6;
  restwav = C/restfrq;


  // Exercise specx().
  printf("\nTesting spectral cross-conversions (specx).\n\n");
  if ((status = specx("VELO", 4.3e5, restfrq, restwav, &spx))) {
    printf("specx ERROR %d: %s.\n", status, spx_errmsg[status]);
    return 1;
  }

  printf("    restfrq:%20.12e\n", spx.restfrq);
  printf("    restwav:%20.12e\n", spx.restwav);
  printf("   wavetype:%3d\n",     spx.wavetype);
  printf("   velotype:%3d\n",     spx.velotype);
  printf("\n");
  printf("       freq:%20.12e\n", spx.freq);
  printf("       afrq:%20.12e\n", spx.afrq);
  printf("       ener:%20.12e\n", spx.ener);
  printf("       wavn:%20.12e\n", spx.wavn);
  printf("       vrad:%20.12e\n", spx.vrad);
  printf("       wave:%20.12e\n", spx.wave);
  printf("       vopt:%20.12e\n", spx.vopt);
  printf("       zopt:%20.12e\n", spx.zopt);
  printf("       awav:%20.12e\n", spx.awav);
  printf("       velo:%20.12e\n", spx.velo);
  printf("       beta:%20.12e\n", spx.beta);
  printf("\n");

  printf("dfreq/dafrq:%20.12e\n", spx.dfreqafrq);
  printf("dafrq/dfreq:%20.12e\n", spx.dafrqfreq);

  printf("dfreq/dener:%20.12e\n", spx.dfreqener);
  printf("dener/dfreq:%20.12e\n", spx.denerfreq);

  printf("dfreq/dwavn:%20.12e\n", spx.dfreqwavn);
  printf("dwavn/dfreq:%20.12e\n", spx.dwavnfreq);

  printf("dfreq/dvrad:%20.12e\n", spx.dfreqvrad);
  printf("dvrad/dfreq:%20.12e\n", spx.dvradfreq);

  printf("dfreq/dwave:%20.12e\n", spx.dfreqwave);
  printf("dwave/dfreq:%20.12e\n", spx.dwavefreq);

  printf("dfreq/dawav:%20.12e\n", spx.dfreqawav);
  printf("dawav/dfreq:%20.12e\n", spx.dawavfreq);

  printf("dfreq/dvelo:%20.12e\n", spx.dfreqvelo);
  printf("dvelo/dfreq:%20.12e\n", spx.dvelofreq);

  printf("dwave/dvopt:%20.12e\n", spx.dwavevopt);
  printf("dvopt/dwave:%20.12e\n", spx.dvoptwave);

  printf("dwave/dzopt:%20.12e\n", spx.dwavezopt);
  printf("dzopt/dwave:%20.12e\n", spx.dzoptwave);

  printf("dwave/dawav:%20.12e\n", spx.dwaveawav);
  printf("dawav/dwave:%20.12e\n", spx.dawavwave);

  printf("dwave/dvelo:%20.12e\n", spx.dwavevelo);
  printf("dvelo/dwave:%20.12e\n", spx.dvelowave);

  printf("dawav/dvelo:%20.12e\n", spx.dawavvelo);
  printf("dvelo/dawav:%20.12e\n", spx.dveloawav);

  printf("dvelo/dbeta:%20.12e\n", spx.dvelobeta);
  printf("dbeta/dvelo:%20.12e\n", spx.dbetavelo);
  printf("\n");


  // Construct a linear velocity spectrum.
  step = (2.0*C/NSPEC) / 2.0;
  for (j = 0, k = -NSPEC; j < NSPEC; j++, k += 2) {
    velo[j] = (k+1)*step;
  }
  printf("\nVelocity range: %.3f to %.3f km/s, step: %.3f km/s\n",
         velo[0]*1e-3, velo[NSPEC-1]*1e-3, (velo[1] - velo[0])*1e-3);

  // Convert it to frequency.
  velofreq(restfrq, NSPEC, 1, 1, velo, freq, stat);

  // Test closure of all two-way combinations.
  nFail += closure("freq", "afrq", 0.0,     freqafrq, afrqfreq, freq, spc1);
  nFail += closure("afrq", "freq", 0.0,     afrqfreq, freqafrq, spc1, spc2);

  nFail += closure("freq", "ener", 0.0,     freqener, enerfreq, freq, spc1);
  nFail += closure("ener", "freq", 0.0,     enerfreq, freqener, spc1, spc2);

  nFail += closure("freq", "wavn", 0.0,     freqwavn, wavnfreq, freq, spc1);
  nFail += closure("wavn", "freq", 0.0,     wavnfreq, freqwavn, spc1, spc2);

  nFail += closure("freq", "vrad", restfrq, freqvrad, vradfreq, freq, spc1);
  nFail += closure("vrad", "freq", restfrq, vradfreq, freqvrad, spc1, spc2);

  nFail += closure("freq", "wave", 0.0,     freqwave, wavefreq, freq, wave);
  nFail += closure("wave", "freq", 0.0,     wavefreq, freqwave, wave, spc2);

  nFail += closure("freq", "awav", 0.0,     freqawav, awavfreq, freq, awav);
  nFail += closure("awav", "freq", 0.0,     awavfreq, freqawav, awav, spc2);

  nFail += closure("freq", "velo", restfrq, freqvelo, velofreq, freq, velo);
  nFail += closure("velo", "freq", restfrq, velofreq, freqvelo, velo, spc2);

  nFail += closure("wave", "vopt", restwav, wavevopt, voptwave, wave, spc1);
  nFail += closure("vopt", "wave", restwav, voptwave, wavevopt, spc1, spc2);

  nFail += closure("wave", "zopt", restwav, wavezopt, zoptwave, wave, spc1);
  nFail += closure("zopt", "wave", restwav, zoptwave, wavezopt, spc1, spc2);

  nFail += closure("wave", "awav", 0.0,     waveawav, awavwave, wave, spc1);
  nFail += closure("awav", "wave", 0.0,     awavwave, waveawav, spc1, spc2);

  nFail += closure("wave", "velo", restwav, wavevelo, velowave, wave, spc1);
  nFail += closure("velo", "wave", restwav, velowave, wavevelo, spc1, spc2);

  nFail += closure("awav", "velo", restwav, awavvelo, veloawav, awav, spc1);
  nFail += closure("velo", "awav", restwav, veloawav, awavvelo, spc1, spc2);

  nFail += closure("velo", "beta", 0.0,     velobeta, betavelo, velo, spc1);
  nFail += closure("beta", "velo", 0.0,     betavelo, velobeta, spc1, spc2);


  if (nFail) {
    printf("\nFAIL: %d closure residuals exceed reporting tolerance.\n",
      nFail);
  } else {
    printf("\nPASS: All closure residuals are within reporting tolerance.\n");
  }

  return nFail;
}

//----------------------------------------------------------------------------

int closure (
  const char *from,
  const char *to,
  double parm,
  int (*fwd)(SPX_ARGS),
  int (*rev)(SPX_ARGS),
  const double spec1[],
  double spec2[])

{
  static char skip = '\0';
  int nFail = 0, stat1[NSPEC], stat2[NSPEC], status;
  register int j;
  double clos[NSPEC], resid, residmax;

  // Convert the first to the second.
  if ((status = fwd(parm, NSPEC, 1, 1, spec1, spec2, stat1))) {
    printf("%s%s ERROR %d: %s.\n", from, to, status, spx_errmsg[status]);
  }

  // Convert the second back to the first.
  if ((status = rev(parm, NSPEC, 1, 1, spec2, clos, stat2))) {
    printf("%s%s ERROR %d: %s.\n", to, from, status, spx_errmsg[status]);
  }

  residmax = 0.0;

  // Test closure.
  for (j = 0; j < NSPEC; j++) {
    if (stat1[j]) {
      printf("%c%s%s: %s = %.12e -> %s = ???, stat = %d\n", skip, from, to,
             from, spec1[j], to, stat1[j]);
      skip = '\0';
      continue;
    }

    if (stat2[j]) {
      printf("%c%s%s: %s = %.12e -> %s = %.12e -> %s = ???, stat = %d\n",
             skip, to, from, from, spec1[j], to, spec2[j], from, stat2[j]);
      skip = '\0';
      continue;
    }

    if (spec1[j] == 0.0) {
      resid = fabs(clos[j] - spec1[j]);
    } else {
      resid = fabs((clos[j] - spec1[j])/spec1[j]);
      if (resid > residmax) residmax = resid;
    }

    if (resid > tol) {
      nFail++;
      printf("%c%s%s: %s = %.12e -> %s = %.12e ->\n          %s = %.12e,  "
             "resid = %.1e\n", skip, from, to, from, spec1[j], to,
             spec2[j], from, clos[j], resid);
      skip = '\0';
    }
  }

  printf("%s%s: Maximum closure residual = %.1e\n", from, to, residmax);
  if (residmax > tol) {
    printf("\n");
    skip = '\0';
  } else {
    skip = '\n';
  }

  return nFail;
}
