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
  $Id: tprj1.c,v 7.3.1.2 2020/08/17 11:20:48 mcalabre Exp mcalabre $
*=============================================================================
*
* tproj1 tests spherical projections for closure.
*
*---------------------------------------------------------------------------*/

#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <wcstrig.h>
#include <prj.h>

int projex(char pcode[4], struct prjprm *prj, int north, int south,
           double tol);

int main()

{
  int  nFail = 0, status;
  const double tol = 1.0e-9;
  struct prjprm prj;


  printf(
    "Testing closure of WCSLIB spherical projection routines (tprj1.c)\n"
    "-----------------------------------------------------------------\n");

  // List status return messages.
  printf("\nList of prj status return values:\n");
  for (status = 1; status <= 4; status++) {
    printf("%4d: %s.\n", status, prj_errmsg[status]);
  }

  printf("\n");


  prjini(&prj);

  // AZP: zenithal/azimuthal perspective.
  prj.pv[1] =   0.5;
  prj.pv[2] =  30.0;
  nFail += projex("AZP", &prj, 90, 5, tol);

  // SZP: slant zenithal perspective.
  prj.pv[1] =   0.5;
  prj.pv[2] = 210.0;
  prj.pv[3] =  60.0;
  nFail += projex("SZP", &prj, 90, -90, tol);

  // TAN: gnomonic.
  nFail += projex("TAN", &prj, 90, 5, tol);

  // STG: stereographic.
  nFail += projex("STG", &prj, 90, -85, tol);

  // SIN: orthographic/synthesis.
  prj.pv[1] = -0.3;
  prj.pv[2] =  0.5;
  nFail += projex("SIN", &prj, 90, 45, tol);

  // ARC: zenithal/azimuthal equidistant.
  nFail += projex("ARC", &prj, 90, -90, tol);

  // ZPN: zenithal/azimuthal polynomial.
  prj.pv[0] =  0.00000;
  prj.pv[1] =  0.95000;
  prj.pv[2] = -0.02500;
  prj.pv[3] = -0.15833;
  prj.pv[4] =  0.00208;
  prj.pv[5] =  0.00792;
  prj.pv[6] = -0.00007;
  prj.pv[7] = -0.00019;
  prj.pv[8] =  0.00000;
  prj.pv[9] =  0.00000;
  nFail += projex("ZPN", &prj, 90, 10, tol);

  // ZEA: zenithal/azimuthal equal area.
  nFail += projex("ZEA", &prj, 90, -85, tol);

  // AIR: Airy's zenithal projection.
  prj.pv[1] = 45.0;
  nFail += projex("AIR", &prj, 90, -85, tol);

  // CYP: cylindrical perspective.
  prj.pv[1] = 3.0;
  prj.pv[2] = 0.8;
  nFail += projex("CYP", &prj, 90, -90, tol);

  // CEA: cylindrical equal area.
  prj.pv[1] = 0.75;
  nFail += projex("CEA", &prj, 90, -90, tol);

  // CAR: plate carree.
  nFail += projex("CAR", &prj, 90, -90, tol);

  // MER: Mercator's.
  nFail += projex("MER", &prj, 85, -85, tol);

  // SFL: Sanson-Flamsteed.
  nFail += projex("SFL", &prj, 90, -90, tol);

  // PAR: parabolic.
  nFail += projex("PAR", &prj, 90, -90, tol);

  // MOL: Mollweide's projection.
  nFail += projex("MOL", &prj, 90, -90, tol);

  // AIT: Hammer-Aitoff.
  nFail += projex("AIT", &prj, 90, -90, tol);

  // COP: conic perspective.
  prj.pv[1] =  60.0;
  prj.pv[2] =  15.0;
  nFail += projex("COP", &prj, 90, -25, tol);

  // COE: conic equal area.
  prj.pv[1] =  60.0;
  prj.pv[2] = -15.0;
  nFail += projex("COE", &prj, 90, -90, tol);

  // COD: conic equidistant.
  prj.pv[1] = -60.0;
  prj.pv[2] =  15.0;
  nFail += projex("COD", &prj, 90, -90, tol);

  // COO: conic orthomorphic.
  prj.pv[1] = -60.0;
  prj.pv[2] = -15.0;
  nFail += projex("COO", &prj, 85, -90, tol);

  // BON: Bonne's projection.
  prj.pv[1] = 30.0;
  nFail += projex("BON", &prj, 90, -90, tol);

  // PCO: polyconic.
  nFail += projex("PCO", &prj, 90, -90, tol);

  // TSC: tangential spherical cube.
  nFail += projex("TSC", &prj, 90, -90, tol);

  // CSC: COBE quadrilateralized spherical cube.
  nFail += projex("CSC", &prj, 90, -90, 4.0e-2);

  // QSC: quadrilateralized spherical cube.
  nFail += projex("QSC", &prj, 90, -90, tol);

  // HPX: HEALPix projection.
  prj.pv[1] = 4.0;
  prj.pv[2] = 3.0;
  nFail += projex("HPX", &prj, 90, -90, tol);

  // XPH: HEALPix polar, aka "butterfly" projection.
  nFail += projex("XPH", &prj, 90, -90, tol);


  if (nFail) {
    printf("\nFAIL: %d closure residuals exceed reporting tolerance.\n",
      nFail);
  } else {
    printf("\nPASS: All closure residuals are within reporting tolerance.\n");
  }

  return nFail;
}


/*----------------------------------------------------------------------------
*   projex() exercises the spherical projection routines.
*
*   Given:
*      pcode[4]  char     Projection code.
*      north     int      Northern cutoff latitude, degrees.
*      south     int      Southern cutoff latitude, degrees.
*      tol       double   Reporting tolerance, degrees.
*
*   Given and returned:
*      prj       prjprm*  Projection parameters.
*
*   Function return value:
*                int      Number of results exceeding reporting tolerance.
*---------------------------------------------------------------------------*/

int projex(
  char pcode[4],
  struct prjprm *prj,
  int north,
  int south,
  double tol)

{
  int    lat, lng, nFail = 0, stat1[361], stat2[361], statr[25][25], status;
  register int i, j;
  double dlat, dlatmx, dlng, dlngmx, dr, drmax, dx, dy;
  double lat1, lat2[361], lng1[361], lng2[361], x[361], y[361];
  double latr[25][25], lngr[25][25], r, x1[25], x2[25][25], y1[25],
         y2[25][25];


  strcpy(prj->code, pcode);

  // Uncomment the next line to test alternative initializations of
  // projection parameters.
  // prj->r0 = R2D;

  printf("Testing %s; latitudes%3d to%4d, reporting tolerance%8.1e deg.\n",
    prj->code, north, south, tol);

  dlngmx = 0.0;
  dlatmx = 0.0;

  prjset(prj);
  for (lat = north; lat >= south; lat--) {
    lat1 = (double)lat;

    for (j = 0, lng = -180; lng <= 180; lng++, j++) {
      lng1[j] = (double)lng;
    }

    if (prj->prjs2x(prj, 361, 1, 1, 1, lng1, &lat1, x, y, stat1) == 1) {
      printf("   %3s(S2X) ERROR 1: %s\n", pcode, prj_errmsg[1]);
      continue;
    }

    if (prj->prjx2s(prj, 361, 0, 1, 1, x, y, lng2, lat2, stat2) == 1) {
      printf("   %3s(X2S) ERROR 1: %s\n", pcode, prj_errmsg[1]);
      continue;
    }

    for (j = 0; j < 361; j++) {
      if (stat1[j]) continue;

      if (stat2[j]) {
        nFail++;
        printf("   %3s(X2S): lng1 =%20.15f  lat1 =%20.15f\n",
          pcode, lng1[j], lat1);
        printf("                x =%20.15f     y =%20.15f\n",
          x[j], y[j]);
        printf("             lng2 =%20.15f  lat2 =%20.15f  ERROR%3d\n",
          lng2[j], lat2[j], stat2[j]);
        continue;
      }

      dlng = fabs(lng2[j] - lng1[j]);
      if (dlng > 180.0) dlng = fabs(dlng-360.0);
      if (abs(lat) != 90 && dlng > dlngmx) dlngmx = dlng;
      dlat = fabs(lat2[j] - lat1);
      if (dlat > dlatmx) dlatmx = dlat;

      if (dlat > tol) {
        nFail++;
        printf("        %3s: lng1 =%20.15f  lat1 =%20.15f\n",
          pcode, lng1[j], lat1);
        printf("                x =%20.15f     y =%20.15f\n",
          x[j], y[j]);
        printf("             lng2 =%20.15f  lat2 =%20.15f\n",
          lng2[j], lat2[j]);
      } else if (abs(lat) != 90) {
        if (dlng > tol) {
          nFail++;
          printf("        %3s: lng1 =%20.15f  lat1 =%20.15f\n",
            pcode, lng1[j], lat1);
          printf("                x =%20.15f     y =%20.15f\n",
            x[j], y[j]);
          printf("             lng2 =%20.15f  lat2 =%20.15f\n",
            lng2[j], lat2[j]);
        }
      }
    }
  }

  printf("             Maximum residual (sky): lng%8.1e  lat%8.1e\n",
    dlngmx, dlatmx);


  // Test closure in the neighbourhood of the reference point.
  r = 1.0;
  x1[12] = y1[12] = 0.0;
  for (i = 0; i < 12; i++) {
    x1[i] = -r;
    y1[i] = -r;
    x1[24-i] = r;
    y1[24-i] = r;

    r /= 10.0;
  }

  if ((status = prj->prjx2s(prj, 25, 25, 1, 1, x1, y1, lngr[0], latr[0],
                            statr[0]))) {
    printf("   %3s(X2S) ERROR: %s\n", pcode, prj_errmsg[status]);

  } else if ((status = prj->prjs2x(prj, 625, 0, 1, 1, lngr[0], latr[0],
                                   x2[0], y2[0], statr[0]))) {
    printf("   %3s(S2X) ERROR: %s\n", pcode, prj_errmsg[status]);

  } else {
    drmax = 0.0;
    for (j = 0; j < 25; j++) {
      for (i = 0; i < 25; i++) {
        dx = x2[j][i] - x1[i];
        dy = y2[j][i] - y1[j];
        dr = sqrt(dx*dx + dy*dy);

        if (dr > drmax) drmax = dr;
        if (dr > tol) {
          nFail++;
          printf("        %3s:   x1 =%20.15f    y1 =%20.15f\n",
            pcode, x1[i], y1[j]);
          printf("              lng =%20.15f   lat =%20.15f\n",
            lngr[j][i], latr[j][i]);
          printf("               x2 =%20.15f    y2 =%20.15f\n",
            x2[j][i], y2[j][i]);
        }
      }
    }

    printf("             Maximum residual (ref):  dR%8.1e\n", drmax);
  }

  prjini(prj);

  return nFail;
}
