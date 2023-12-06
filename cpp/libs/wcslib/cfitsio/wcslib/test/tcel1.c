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
  $Id: tcel1.c,v 7.3.1.2 2020/08/17 11:20:48 mcalabre Exp mcalabre $
*=============================================================================
*
* tcel1 tests the spherical projection driver routines supplied with WCSLIB by
* drawing native and celestial coordinate graticules for Bonne's projection.
*
*---------------------------------------------------------------------------*/

#include <cpgplot.h>
#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <cel.h>

int main()

{
  char   text[80];
  int    ci, crval1, crval2, ilat, ilng, j, k, latpole, lonpole, stat[361],
         status;
  float  xr[512], yr[512];
  double lat[181], lng[361], phi[361], theta[361], x[361], y[361];
  struct celprm native, celestial;


  printf(
  "Testing WCSLIB celestial coordinate transformation routines (tcel1.c)\n"
  "---------------------------------------------------------------------\n");

  // List status return messages.
  printf("\nList of cel status return values:\n");
  for (status = 1; status <= 6; status++) {
    printf("%4d: %s.\n", status, cel_errmsg[status]);
  }

  printf("\n");


  // Initialize.
  celini(&native);

  // Reference angles for the native graticule (in fact, the defaults).
  native.ref[0] = 0.0;
  native.ref[1] = 0.0;

  // Set up Bonne's projection with conformal latitude at +35.
  strcpy(native.prj.code, "BON");
  native.prj.pv[1] = 35.0;


  // Celestial graticule.
  celini(&celestial);
  celestial.prj = native.prj;


  // PGPLOT initialization.
  strcpy(text, "/xwindow");
  cpgbeg(0, text, 1, 1);

  // Define pen colours.
  cpgscr(0, 0.0f, 0.0f, 0.0f);
  cpgscr(1, 1.0f, 1.0f, 0.0f);
  cpgscr(2, 1.0f, 1.0f, 1.0f);
  cpgscr(3, 0.5f, 0.5f, 0.8f);
  cpgscr(4, 0.8f, 0.5f, 0.5f);
  cpgscr(5, 0.8f, 0.8f, 0.8f);
  cpgscr(6, 0.5f, 0.5f, 0.8f);
  cpgscr(7, 0.8f, 0.5f, 0.5f);
  cpgscr(8, 0.3f, 0.5f, 0.3f);

  // Define PGPLOT viewport.
  cpgenv(-180.0f, 180.0f, -90.0f, 140.0f, 1, -2);

  // Loop over CRVAL2, LONPOLE, and LATPOLE with CRVAL1 incrementing by
  // 15 degrees each time (it has an uninteresting effect).
  crval1 = -180;
  for (crval2 = -90; crval2 <=  90; crval2 += 30) {
    for (lonpole = -180; lonpole <= 180; lonpole += 30) {
      for (latpole = -1; latpole <= 1; latpole += 2) {
        // For the celestial graticule, set the celestial coordinates of the
        // reference point of the projection (which for Bonne's projection is
        // at the intersection of the native equator and prime meridian), the
	// native longitude of the celestial pole, and extra information
	// needed to determine the celestial latitude of the native pole.
	// These correspond to FITS keywords CRVAL1, CRVAL2, LONPOLE, and
	// LATPOLE.
        celestial.ref[0] = (double)crval1;
        celestial.ref[1] = (double)crval2;
        celestial.ref[2] = (double)lonpole;
        celestial.ref[3] = (double)latpole;

        // Skip invalid values of LONPOLE.
        if (celset(&celestial)) {
          continue;
        }

        // Skip redundant values of LATPOLE.
        if (latpole == 1 && fabs(celestial.ref[3]) < 0.1) {
          continue;
        }

        // Buffer PGPLOT output.
        cpgbbuf();
        cpgeras();

        // Write a descriptive title.
        sprintf(text, "Bonne's projection (BON) - 15 degree graticule");
        printf("\n%s\n", text);
        cpgtext(-180.0f, -100.0f, text);

        sprintf(text, "centred on celestial coordinates (%7.2f,%6.2f)",
          celestial.ref[0], celestial.ref[1]);
        printf("%s\n", text);
        cpgtext (-180.0f, -110.0f, text);

        sprintf(text, "with north celestial pole at native coordinates "
          "(%7.2f,%7.2f)", celestial.ref[2], celestial.ref[3]);
        printf("%s\n", text);
        cpgtext(-180.0f, -120.0f, text);


        // Draw the native graticule faintly in the background.
        cpgsci(8);

        // Draw native meridians of longitude.
        for (j = 0, ilat = -90; ilat <= 90; ilat++, j++) {
          lat[j] = (double)ilat;
        }

        for (ilng = -180; ilng <= 180; ilng += 15) {
          lng[0] = (double)ilng;
          if (ilng == -180) lng[0] = -179.99;
          if (ilng ==  180) lng[0] =  179.99;

          // Dash the longitude of the celestial pole.
          if ((ilng-lonpole)%360 == 0) {
            cpgsls(2);
            cpgslw(5);
          }

          cels2x(&native, 1, 181, 1, 1, lng, lat, phi, theta, x, y, stat);

          k = 0;
          for (j = 0; j < 181; j++) {
            if (stat[j]) {
              if (k > 1) cpgline(k, xr, yr);
              k = 0;
              continue;
            }

            xr[k] = -x[j];
            yr[k] =  y[j];
            k++;
          }

          cpgline(k, xr, yr);
          cpgsls(1);
          cpgslw(1);
        }

        // Draw native parallels of latitude.
        lng[0]   = -179.99;
        lng[360] =  179.99;
        for (j = 1, ilng = -179; ilng < 180; ilng++, j++) {
          lng[j] = (double)ilng;
        }

        for (ilat = -90; ilat <= 90; ilat += 15) {
          lat[0] = (double)ilat;

          cels2x(&native, 361, 1, 1, 1, lng, lat, phi, theta, x, y, stat);

          k = 0;
          for (j = 0; j < 361; j++) {
            if (stat[j]) {
              if (k > 1) cpgline(k, xr, yr);
              k = 0;
              continue;
            }

            xr[k] = -x[j];
            yr[k] =  y[j];
            k++;
          }

          cpgline(k, xr, yr);
        }


        // Draw a colour-coded celestial coordinate graticule.
        ci = 1;

        // Draw celestial meridians of longitude.
        for (j = 0, ilat = -90; ilat <= 90; ilat++, j++) {
          lat[j] = (double)ilat;
        }

        for (ilng = -180; ilng <= 180; ilng += 15) {
          lng[0] = (double)ilng;

          if (++ci > 7) ci = 2;
          cpgsci(ilng?ci:1);

          // Dash the reference longitude.
          if ((ilng-crval1)%360 == 0) {
            cpgsls(2);
            cpgslw(5);
          }

          cels2x(&celestial, 1, 181, 1, 1, lng, lat, phi, theta, x, y, stat);

          k = 0;
          for (j = 0; j < 181; j++) {
            if (stat[j]) {
              if (k > 1) cpgline(k, xr, yr);
              k = 0;
              continue;
            }

            // Test for discontinuities.
            if (j > 0) {
              if (fabs(x[j]-x[j-1]) > 4.0 || fabs(y[j]-y[j-1]) > 4.0) {
                if (k > 1) cpgline(k, xr, yr);
                k = 0;
              }
            }

            xr[k] = -x[j];
            yr[k] =  y[j];
            k++;
          }

          cpgline(k, xr, yr);
          cpgsls(1);
          cpgslw(1);
        }

        // Draw celestial parallels of latitude.
        for (j = 0, ilng = -180; ilng <= 180; ilng++, j++) {
          lng[j] = (double)ilng;
        }

        ci = 1;
        for (ilat = -90; ilat <= 90; ilat += 15) {
          lat[0] = (double)ilat;

          if (++ci > 7) ci = 2;
          cpgsci(ilat?ci:1);

          // Dash the reference latitude.
          if (ilat == crval2) {
            cpgsls(2);
            cpgslw(5);
          }

          cels2x(&celestial, 361, 1, 1, 1, lng, lat, phi, theta, x, y, stat);

          k = 0;
          for (j = 0; j < 361; j++) {
            if (stat[j]) {
              if (k > 1) cpgline(k, xr, yr);
              k = 0;
              continue;
            }

            // Test for discontinuities.
            if (j > 0) {
              if (fabs(x[j]-x[j-1]) > 4.0 || fabs(y[j]-y[j-1]) > 4.0) {
                if (k > 1) cpgline(k, xr, yr);
                k = 0;
              }
            }

            xr[k] = -x[j];
            yr[k] =  y[j];
            k++;
          }

          cpgline(k, xr, yr);
          cpgsls(1);
          cpgslw(1);
        }

        // Flush PGPLOT buffer.
        cpgebuf();
        printf(" Type <RETURN> for next page: ");
        getc(stdin);

        // Cycle through celestial longitudes.
        if ((crval1 += 15) > 180) crval1 = -180;

        // Skip boring celestial latitudes.
        if (crval2 == 0) break;
      }

      if (crval2 == 0) break;
    }
  }

  cpgask(0);
  cpgend();

  return 0;
}
