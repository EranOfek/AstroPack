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
  $Id: tprj2.c,v 7.3.1.2 2020/08/17 11:20:48 mcalabre Exp mcalabre $
*=============================================================================
*
* tproj2 tests projection routines by plotting test graticules using PGPLOT.
*
*---------------------------------------------------------------------------*/

#include <cpgplot.h>
#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <prj.h>


int main()

{
  void prjplt();
  int  status;
  char text[80], text1[80], text2[80];
  struct prjprm prj;


  printf("Testing WCSLIB spherical projection routines (tprj2.c)\n"
         "------------------------------------------------------\n");

  // List status return messages.
  printf("\nList of prj status return values:\n");
  for (status = 1; status <= 4; status++) {
    printf("%4d: %s.\n", status, prj_errmsg[status]);
  }

  printf("\n");


  // PGPLOT initialization.
  strcpy(text, "/xwindow");
  cpgbeg(0, text, 1, 1);

  // Define pen colours.
  cpgscr(0, 0.00f, 0.00f, 0.00f);
  cpgscr(1, 1.00f, 1.00f, 0.00f);
  cpgscr(2, 1.00f, 1.00f, 1.00f);
  cpgscr(3, 0.50f, 0.50f, 0.80f);
  cpgscr(4, 0.80f, 0.50f, 0.50f);
  cpgscr(5, 0.80f, 0.80f, 0.80f);
  cpgscr(6, 0.50f, 0.50f, 0.80f);
  cpgscr(7, 0.80f, 0.50f, 0.50f);
  cpgscr(8, 0.30f, 0.50f, 0.30f);

  strcpy(text1, "\n%s projection\n");
  strcpy(text2, "\n%s projection\nParameters:");

  prjini(&prj);

  // AZP: zenithal/azimuthal perspective.
  prj.pv[1] =   2.0;
  prj.pv[2] =  30.0;
  printf(text2, "Zenithal/azimuthal perspective");
  printf("%12.5f%12.5f\n", prj.pv[1], prj.pv[2]);
  prjplt("AZP", 90, -90, &prj);

  // SZP: slant zenithal perspective.
  prj.pv[1] =   2.0;
  prj.pv[2] = 210.0;
  prj.pv[3] =  60.0;
  printf(text2, "Slant zenithal perspective");
  printf("%12.5f%12.5f%12.5f\n", prj.pv[1], prj.pv[2], prj.pv[3]);
  prjplt("SZP", 90, -90, &prj);

  // TAN: gnomonic.
  printf(text1, "Gnomonic");
  prjplt("TAN", 90,   5, &prj);

  // STG: stereographic.
  printf(text1, "Stereographic");
  prjplt("STG", 90, -85, &prj);

  // SIN: orthographic.
  prj.pv[1] = -0.3;
  prj.pv[2] =  0.5;
  printf(text2, "Orthographic/synthesis");
  printf("%12.5f%12.5f\n", prj.pv[1], prj.pv[2]);
  prjplt("SIN", 90, -90, &prj);

  // ARC: zenithal/azimuthal equidistant.
  printf(text1, "Zenithal/azimuthal equidistant");
  prjplt("ARC", 90, -90, &prj);

  // ZPN: zenithal/azimuthal polynomial.
  prj.pv[0] =  0.05000;
  prj.pv[1] =  0.95000;
  prj.pv[2] = -0.02500;
  prj.pv[3] = -0.15833;
  prj.pv[4] =  0.00208;
  prj.pv[5] =  0.00792;
  prj.pv[6] = -0.00007;
  prj.pv[7] = -0.00019;
  prj.pv[8] =  0.00000;
  prj.pv[9] =  0.00000;
  printf(text2, "Zenithal/azimuthal polynomial");
  printf("%12.5f%12.5f%12.5f%12.5f%12.5f\n",
    prj.pv[0], prj.pv[1], prj.pv[2], prj.pv[3], prj.pv[4]);
  printf("           %12.5f%12.5f%12.5f%12.5f%12.5f\n",
    prj.pv[5], prj.pv[6], prj.pv[7], prj.pv[8], prj.pv[9]);
  prjplt("ZPN", 90,  10, &prj);

  // ZEA: zenithal/azimuthal equal area.
  printf(text1, "Zenithal/azimuthal equal area");
  prjplt("ZEA", 90, -90, &prj);

  // AIR: Airy's zenithal projection.
  prj.pv[1] = 45.0;
  printf(text2, "Airy's zenithal");
  printf("%12.5f\n", prj.pv[1]);
  prjplt("AIR", 90, -85, &prj);

  // CYP: cylindrical perspective.
  prj.pv[1] = 3.0;
  prj.pv[2] = 0.8;
  printf(text2, "Cylindrical perspective");
  printf("%12.5f%12.5f\n", prj.pv[1], prj.pv[2]);
  prjplt("CYP", 90, -90, &prj);

  // CEA: cylindrical equal area.
  prj.pv[1] = 0.75;
  printf(text2, "Cylindrical equal area");
  printf("%12.5f\n", prj.pv[1]);
  prjplt("CEA", 90, -90, &prj);

  // CAR: plate carree.
  printf(text1, "Plate carree");
  prjplt("CAR", 90, -90, &prj);

  // MER: Mercator's.
  printf(text1, "Mercator's");
  prjplt("MER", 85, -85, &prj);

  // SFL: Sanson-Flamsteed.
  printf(text1, "Sanson-Flamsteed (global sinusoid)");
  prjplt("SFL", 90, -90, &prj);

  // PAR: parabolic.
  printf(text1, "Parabolic");
  prjplt("PAR", 90, -90, &prj);

  // MOL: Mollweide's projection.
  printf(text1, "Mollweide's");
  prjplt("MOL", 90, -90, &prj);

  // AIT: Hammer-Aitoff.
  printf(text1, "Hammer-Aitoff");
  prjplt("AIT", 90, -90, &prj);

  // COP: conic perspective.
  prj.pv[1] =  60.0;
  prj.pv[2] =  15.0;
  printf(text2, "Conic perspective");
  printf("%12.5f%12.5f\n", prj.pv[1], prj.pv[2]);
  prjplt("COP", 90, -25, &prj);

  // COE: conic equal area.
  prj.pv[1] =  60.0;
  prj.pv[2] = -15.0;
  printf(text2, "Conic equal area");
  printf("%12.5f%12.5f\n", prj.pv[1], prj.pv[2]);
  prjplt("COE", 90, -90, &prj);

  // COD: conic equidistant.
  prj.pv[1] = -60.0;
  prj.pv[2] =  15.0;
  printf(text2, "Conic equidistant");
  printf("%12.5f%12.5f\n", prj.pv[1], prj.pv[2]);
  prjplt("COD", 90, -90, &prj);

  // COO: conic orthomorphic.
  prj.pv[1] = -60.0;
  prj.pv[2] = -15.0;
  printf(text2, "Conic orthomorphic");
  printf("%12.5f%12.5f\n", prj.pv[1], prj.pv[2]);
  prjplt("COO", 85, -90, &prj);

  // BON: Bonne's projection.
  prj.pv[1] = 30.0;
  printf(text2, "Bonne's");
  printf("%12.5f\n", prj.pv[1]);
  prjplt("BON", 90, -90, &prj);

  // PCO: polyconic.
  printf(text1, "Polyconic");
  prjplt("PCO", 90, -90, &prj);

  // TSC: tangential spherical cube.
  printf(text1, "Tangential spherical cube");
  prjplt("TSC", 90, -90, &prj);

  // CSC: COBE quadrilateralized spherical cube.
  printf(text1, "COBE quadrilateralized spherical cube");
  prjplt("CSC", 90, -90, &prj);

  // QSC: quadrilateralized spherical cube.
  printf(text1, "Quadrilateralized spherical cube");
  prjplt("QSC", 90, -90, &prj);

  // HPX: HEALPix projection.
  prj.pv[1] = 4.0;
  prj.pv[2] = 3.0;
  printf(text1, "HEALPix");
  prjplt("HPX", 90, -90, &prj);

  // XPH: HEALPix polar, aka "butterfly" projection.
  printf(text1, "Butterfly");
  prjplt("XPH", 90, -90, &prj);

  cpgask(0);
  cpgend();

  return 0;
}


/*----------------------------------------------------------------------------
*   PRJPLT draws a 15 degree coordinate graticule.
*
*   Given:
*      pcode[4]  char     Projection mnemonic.
*      north     int      Northern cutoff latitude, degrees.
*      south     int      Southern cutoff latitude, degrees.
*
*   Given and returned:
*      prj       prjprm*  Projection parameters.
*---------------------------------------------------------------------------*/

void prjplt(pcode, north, south, prj)

char   pcode[4];
int    north, south;
struct prjprm *prj;

{
  char   text[80];
  int    ci, h, ilat, ilng, interrupted, len, stat[361];
  register int j, k;
  float  hx, hy, sx, sy, xr[512], yr[512];
  double lat[361], lng[361], x[361], y[361];

  strcpy(prj->code, pcode);

  printf("Plotting %s; latitudes%3d to%4d.\n", prj->code, north, south);

  cpgask(0);

  prjset(prj);
  if (prj->category == QUADCUBE) {
    // Draw the perimeter of the quadcube projection.
    cpgenv(-335.0f, 65.0f, -200.0f, 200.0f, 1, -2);
    cpgsci(2);
    sprintf(text,"%s - 15 degree graticule", pcode);
    cpgtext(-340.0f, -220.0f, text);

    cpgsci(8);

    xr[0] =      45.0 + prj->x0;
    yr[0] =      45.0 - prj->y0;
    xr[1] =      45.0 + prj->x0;
    yr[1] =  3.0*45.0 - prj->y0;
    xr[2] =     -45.0 + prj->x0;
    yr[2] =  3.0*45.0 - prj->y0;
    xr[3] =     -45.0 + prj->x0;
    yr[3] = -3.0*45.0 - prj->y0;
    xr[4] =      45.0 + prj->x0;
    yr[4] = -3.0*45.0 - prj->y0;
    xr[5] =      45.0 + prj->x0;
    yr[5] =      45.0 - prj->y0;
    xr[6] = -7.0*45.0 + prj->x0;
    yr[6] =      45.0 - prj->y0;
    xr[7] = -7.0*45.0 + prj->x0;
    yr[7] =     -45.0 - prj->y0;
    xr[8] =      45.0 + prj->x0;
    yr[8] =     -45.0 - prj->y0;
    cpgline(9, xr, yr);

  } else {
    cpgenv(-200.0f, 200.0f, -200.0f, 200.0f, 1, -2);
    cpgsci(2);
    sprintf(text,"%s - 15 degree graticule", pcode);
    cpgtext(-240.0f, -220.0f, text);

    if (prj->category == HEALPIX) {
      cpgsci(8);

      if (strcmp(pcode, "HPX") == 0) {
        // Draw the perimeter of the HEALPix projection.
        h = (int)(prj->pv[1] + 0.5);
        sx = 180.0f / h;
        sy = sx * (int)(prj->pv[2] + 1.5) / 2.0f;

        hx = 180.0f + prj->x0;
        hy = sy - sx - prj->y0;
        cpgmove(hx, hy);

        for (j = 0; j < h; j++) {
          hx -= sx;
          hy += sx;
          cpgdraw(hx, hy);

          hx -= sx;
          hy -= sx;
          cpgdraw(hx, hy);
        }

        hx = 180.0f + prj->x0;
        hy = -sy + sx - prj->y0;

        k = ((int)prj->pv[2])%2 ? 1 : -1;
        if (k == -1) hy -= sx;

        cpgmove(hx, hy);

        for (j = 0; j < h; j++) {
          hx -= sx;
          hy -= k*sx;
          cpgdraw(hx, hy);

          hx -= sx;
          hy += k*sx;
          cpgdraw(hx, hy);
        }

      } else if (strcmp(pcode, "XPH") == 0) {
        for (ilng = -90; ilng <= 180; ilng+=90) {
          lng[0] = (double)ilng - 0.0001;

          for (j = 0, ilat = 90; ilat >= -90; ilat--, j++) {
            lat[j] = (double)ilat;
          }

          prj->prjs2x(prj, 1, 181, 1, 1, lng, lat, x, y, stat);

          for (j = 0; j < 181; j++) {
            xr[j] = -x[j];
            yr[j] =  y[j];
          }

          cpgline(181, xr, yr);
        }
      }
    }
  }


  ci = 1;
  for (ilng = -180; ilng <= 180; ilng+=15) {
    if (++ci > 7) ci = 2;

    lng[0] = (double)ilng;

    cpgsci(ilng?ci:1);

    len = north - south + 1;
    for (j = 0, ilat = north; ilat >= south; ilat--, j++) {
      lat[j] = (double)ilat;
    }

    prj->prjs2x(prj, 1, len, 1, 1, lng, lat, x, y, stat);

    k = 0;
    for (j = 0; j < len; j++) {
      if (stat[j]) {
        if (k > 1) cpgline(k, xr, yr);
        k = 0;
        continue;
      }

      if (prj->category == QUADCUBE && j > 0) {
        if (fabs(x[j] - x[j-1]) > 2.0 || fabs(y[j] - y[j-1]) > 5.0) {
          if (k > 1) cpgline(k, xr, yr);
          k = 0;
        }
      } else if (strcmp(pcode, "HPX") == 0 && ilng == 180) {
        if (x[j] > 180.0) {
          continue;
        }
      }

      xr[k] = -x[j];
      yr[k] =  y[j];
      k++;
    }

    cpgline(k, xr, yr);
  }

  ci = 1;
  interrupted = (prj->category == QUADCUBE || prj->category == HEALPIX);
  for (ilat = -90; ilat <= 90; ilat += 15) {
    if (++ci > 7) ci = 2;

    if (ilat > north) continue;
    if (ilat < south) continue;

    lat[0] = (double)ilat;

    cpgsci(ilat?ci:1);

    for (j = 0, ilng = -180; ilng <= 180; ilng++, j++) {
      lng[j] = (double)ilng;
    }

    prj->prjs2x(prj, 361, 1, 1, 1, lng, lat, x, y, stat);

    k = 0;
    for (j = 0; j <= 360; j++) {
      if (stat[j]) {
        if (k > 1) cpgline(k, xr, yr);
        k = 0;
        continue;
      }

      if (interrupted && j > 0) {
        if (fabs(x[j] - x[j-1]) > 2.0 || fabs(y[j] - y[j-1]) > 5.0) {
          if (k > 1) cpgline(k, xr, yr);
          k = 0;
        }
      }

      xr[k] = -x[j];
      yr[k] =  y[j];
      k++;
    }

    cpgline(k, xr, yr);
  }

  cpgsci(1);
  xr[0] = 0.0f;
  yr[0] = 0.0f;
  cpgpt(1, xr, yr, 21);


  cpgask(1);
  cpgpage();

  prjini(prj);

  return;
}
