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
  $Id: tcel2.c,v 7.3.1.2 2020/08/17 11:20:48 mcalabre Exp mcalabre $
*=============================================================================
*
* tcel2 thoroughly tests the WCSLIB celestial coordinate transformation
* routines, particularly celset(), by plotting oblique test grids for a wide
* variety of transformation parameters.  A simple user interface provides
* limited control of the path taken through this parameter space.
*
*---------------------------------------------------------------------------*/

#include <cpgplot.h>
#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <cel.h>

#define nint(x) ((int)(x + (((x) > 0.0) ? 0.5 : -0.5)))

int main()

{
  char   answer[16], ctrl, text[128];
  int    ci, crval1, crval1_j, crval2, crval2_i, first, ilat, ilng, iprj, j,
         k, latpole, lonpole, lonpole_i, lonpole_j, phi_p, stat[361], status;
  float  xr[512], yr[512];
  double alpha_p, lat[181], lng[361], phi[361], theta[361], x[361], y[361];
  struct celprm native, celestial;


  printf(
  "Testing WCSLIB celestial coordinate transformation routines (tcel2.c)\n"
  "---------------------------------------------------------------------\n");

  // List status return messages.
  printf("\nList of cel status return values:\n");
  for (status = 1; status <= 6; status++) {
    printf("%4d: %s.\n", status, cel_errmsg[status]);
  }

  printf("\n\nLegend (in the order drawn)\n---------------------------\n");

  printf("Native graticule in dark green with the meridian containing the "
         "celestial\n   pole (LONPOLE) thicker and in green.  Also tagged "
         "beyond the perimeter.\n");

  printf("Celestial graticule colour-coded, the direction of increasing "
         "celestial\n   longitude and latitude is white -> blue -> red, "
         "with the equator and\n   prime meridian in yellow.\n");

  printf("Celestial meridian (CRVAL1) and parallel (CRVAL2) through the "
         "reference point\n   is thicker and dashed.\n");

  printf("Reference point of the projection (phi0,theta0) is marked with "
         "a green circle\n   with red centre.  It should coincide with the "
         "dashed celestial meridian and\n   parallel.\n");

  printf("Celestial pole (LONPOLE,LATPOLE) marked with a green circle with "
         "black centre.\n");

  printf("Celestial prime meridian expected for \"change of origin\" case "
         "marked with\n   an open yellow circle (where applicable).  Should "
         "coincide with the prime\n   meridian.\n");

  printf("\n\n");

  printf("Loop control; LONPOLE changes fastest, then CRVAL1, then CRVAL2\n"
         "---------------------------------------------------------------\n"
         "       next: do next plot\n"
         "       skip: skip past invalid values of LONPOLE\n"
         "      break: break out of  inner loop on LONPOLE\n"
         "   continue: cycle through inner loop on LONPOLE\n");
  printf("       proj: skip to next projection\n"
         "        inc: LONPOLE++, preserving CRVAL1 & CRVAL2\n"
         "       jump: CRVAL2++,  preserving CRVAL1 & LONPOLE\n"
         "       exit: terminate execution\n"
         "       quit: terminate execution\n"
         "Capital letter kills query.\n");

  printf("\n\n");


  // PGPLOT initialization.
  strcpy(text, "/xwindow");
  cpgbeg(0, text, 1, 1);

  // Define pen colours.
  cpgscr( 0, 0.0f, 0.0f, 0.0f);		// Black
  cpgscr( 1, 1.0f, 1.0f, 0.0f);		// Yellow
  cpgscr( 2, 1.0f, 1.0f, 1.0f);		// White
  cpgscr( 3, 0.5f, 0.5f, 0.8f);		// Mauve
  cpgscr( 4, 0.8f, 0.5f, 0.5f);		// Pink
  cpgscr( 5, 0.8f, 0.8f, 0.8f);		// Grey
  cpgscr( 6, 0.5f, 0.5f, 0.8f);		// Mauve
  cpgscr( 7, 0.8f, 0.5f, 0.5f);		// Pink
  cpgscr( 8, 0.3f, 0.5f, 0.3f);		// Dark green
  cpgscr( 9, 0.0f, 1.0f, 0.0f);		// Green
  cpgscr(10, 1.0f, 0.0f, 0.0f);		// Red

  // Define PGPLOT viewport.
  cpgenv(-195.0f, 195.0f, -195.0f, 195.0f, 1, -2);
  cpgsch(0.8f);


  ctrl = 'n';
  for (iprj = 0; iprj < 4; iprj++) {
    // Initialize.
    celini(&native);
    celini(&celestial);

    // Reference coordinates for the native graticule.
    if (iprj == 0) {
      // Set up a zenithal equidistant projection.
      strcpy(native.prj.code, "ARC");

      native.ref[0] =   0.0;
      native.ref[1] =  90.0;
      native.ref[2] = 180.0;

      celestial.phi0   =  0.0;
      celestial.theta0 = 90.0;

    } else if (iprj == 1) {
      // Set up a conic equidistant projection.
      strcpy(native.prj.code, "COD");
      native.prj.pv[1] = 45.0;
      native.prj.pv[2] = 25.0;

      native.ref[0] =   0.0;
      native.ref[1] =  45.0;
      native.ref[2] = 180.0;

      celestial.phi0   = 60.0;
      celestial.theta0 = 45.0;

    } else if (iprj == 2) {
      // Set up a Sanson-Flamsteed projection as Bonne's equatorial.
      strcpy(native.prj.code, "BON");
      native.prj.pv[1] = 0.0;

      native.ref[0] = 0.0;
      native.ref[1] = 0.0;
      native.ref[2] = 0.0;

      celestial.phi0   = -30.0;
      celestial.theta0 =   0.0;

    } else if (iprj == 3) {
      // Set up a polyconic projection.
      strcpy(native.prj.code, "PCO");

      native.ref[0] = 0.0;
      native.ref[1] = 0.0;
      native.ref[2] = 0.0;

      celestial.phi0   = -60.0;
      celestial.theta0 = -90.0;
    }

    celestial.prj = native.prj;

    // Loop over CRVAL2, CRVAL1 and LONPOLE.
    crval1_j  = -180;
    crval2_i  =   45;
    lonpole_i =   15;
    lonpole_j = -180;
    for (crval2 = -90; crval2 <=  90; crval2 += crval2_i) {
      for (crval1 = -180; crval1 <= 180; crval1 += 90) {
        first = 1;
        for (lonpole = -180; lonpole <= 180; lonpole += lonpole_i) {
          // lonpole = 999;
          latpole = 999;

          // if (crval2 < 0) latpole = -999;
          // if (crval2 > 0) latpole =  999;

          if (ctrl == 'j' || ctrl == 'J') {
            // Restore CRVAL1 and LONPOLE from last time.
            crval1  = crval1_j;
            lonpole = lonpole_j;
          }

          celestial.ref[0] = (double)crval1;
          celestial.ref[1] = (double)crval2;
          celestial.ref[2] = (double)lonpole;
          celestial.ref[3] = (double)latpole;

          // Buffer PGPLOT output.
          cpgbbuf();
          cpgeras();
          cpgsci(2);

          // Write parameter summary.
          sprintf(text, "(CRVAL1, CRVAL2, LONPOLE): (%+3.3d, %+2.2d, %+3.3d)",
            crval1, crval2, lonpole);
          cpgtext(-180.0f, 200.0f, text);

          // Skip invalid values of LONPOLE.
          if (celset(&celestial)) {
            sprintf(text, "INVALID VALUE OF LONPOLE (= %+3.3d)", lonpole);
            cpgtext(-90.0f, 0.0f, text);

            sprintf(text, "%s projection, (\\gf\\d0\\u,\\gh\\d0\\u) = "
               "(%+3.3d, %+2.2d)", native.prj.code, nint(celestial.phi0),
               nint(celestial.theta0));
            cpgtext(-180.0f, -200.0f, text);

            if (ctrl == 's' || ctrl == 'S') {
              cpgebuf();
              continue;
            }

            goto skip;
          }

          // Write parameters.
          sprintf(text, "%s projection, (\\gf\\d0\\u,\\gh\\d0\\u) = "
            "(%+3.3d, %+2.2d) - green circle with red centre",
            native.prj.code, nint(celestial.phi0),
            nint(celestial.theta0));
          cpgtext(-180.0f, -200.0f, text);

          sprintf(text, "(CRVAL1, CRVAL2): (%+3.3d, %+2.2d) - dashed grid"
            " lines", nint(celestial.ref[0]), nint(celestial.ref[1]));
          cpgtext(-180.0f, -213.0f, text);

          sprintf(text, "(LONPOLE, LATPOLE): (%+3.3d, %+3.3d) -> "
            "(%+3.3d, %+2.2d) - open green circle", lonpole, latpole,
            nint(celestial.ref[2]), nint(celestial.ref[3]));
          cpgtext(-180.0f, -226.0f, text);

          sprintf(text, "(\\ga\\dp\\u, \\gd\\dp\\u): (%+3.3d, %+2.2d)",
            nint(celestial.euler[0]), nint(90.0-celestial.euler[1]));
          cpgtext(-180.0f, -239.0f, text);

          if (celestial.latpreq == 0) {
            sprintf(text, "(LATPOLE not required.)");
          } else if (celestial.latpreq == 1) {
            sprintf(text, "(LATPOLE disambiguates.)");
          } else if (celestial.latpreq == 2) {
            sprintf(text, "(LATPOLE IS DEFINITIVE.)");
          }
          cpgtext(-40.0f, -239.0f, text);


          // Draw the native graticule in the background (dark green).
          cpgsci(8);

          // Draw native meridians of longitude.
          for (j = 0, ilat = -90; ilat <= 90; ilat++, j++) {
            lat[j] = (double)ilat;
          }

          phi_p = nint(celestial.ref[2]);
          for (ilng = -180; ilng <= 180; ilng += 15) {
            lng[0] = (double)ilng;
            if (ilng == -180) lng[0] = -179.99;
            if (ilng ==  180) lng[0] =  179.99;

            // Meridian containing the celestial pole (thick green).
            if (ilng == phi_p) {
              cpgslw(5);
              cpgsci(9);
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
            cpgslw(1);
            cpgsci(8);
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

          // Tag the longitude of the celestial pole.
          cpgslw(5);
          cpgsci(9);
          phi[0]   = celestial.ref[2];
          theta[0] = -90.0;
          theta[1] = -80.0;
          prjs2x(&(native.prj), 1, 2, 1, 1, phi, theta, x, y, stat);
          xr[0] = -x[0];
          yr[0] =  y[0];
          xr[1] = -x[0] + (x[1] - x[0]);
          yr[1] =  y[0] - (y[1] - y[0]);
          cpgline(2, xr, yr);


          // Draw a colour-coded celestial coordinate graticule.
          ci = 1;

          // Draw celestial meridians of longitude.
          for (j = 0, ilat = -90; ilat <= 90; ilat++, j++) {
            lat[j] = (double)ilat;
          }

          for (ilng = -180; ilng <= 180; ilng += 15) {
            lng[0] = (double)ilng;

            // Cycle through colours with the prime meridian in yellow.
            if (++ci > 7) ci = 2;
            cpgsci(ilng?ci:1);

            // Dash the reference longitude and make it thicker.
            if ((ilng-crval1)%360 == 0) {
              cpgsls(2);
              cpgslw(5);
            }

            cels2x(&celestial, 1, 181, 1, 1, lng, lat, phi, theta, x, y,
                   stat);

            k = 0;
            for (j = 0; j < 181; j++) {
              if (stat[j]) {
                if (k > 1) cpgline(k, xr, yr);
                k = 0;
                continue;
              }

              // Test for discontinuities.
              if (j > 0) {
                if (fabs(phi[j]-phi[j-1]) > 15.0) {
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

            // Cycle through colours with the prime meridian in yellow.
            if (++ci > 7) ci = 2;
            cpgsci(ilat?ci:1);

            // Dash the reference latitude and make it thicker.
            if (ilat == crval2) {
              cpgsls(2);
              cpgslw(5);
            }

            cels2x(&celestial, 361, 1, 1, 1, lng, lat, phi, theta, x, y,
                   stat);

            k = 0;
            for (j = 0; j < 361; j++) {
              if (stat[j]) {
                if (k > 1) cpgline(k, xr, yr);
                k = 0;
                continue;
              }

              // Test for discontinuities.
              if (j > 0) {
                if (fabs(phi[j]-phi[j-1]) > 15.0) {
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

          // Mark the fiducial point (green with red centre).
          phi[0]   = celestial.phi0;
          theta[0] = celestial.theta0;
          prjs2x(&(native.prj), 1, 1, 1, 1, phi, theta, x, y, stat);
          xr[0] = -x[0];
          yr[0] =  y[0];

          cpgslw(5);
          cpgsci(9);
          cpgpt1(xr[0], yr[0], 24);
          cpgpt1(xr[0], yr[0], 23);
          cpgsci(10);
          cpgpt1(xr[0], yr[0], 17);

          // Mark the celestial pole.
          phi[0]   = celestial.ref[2];
          theta[0] = celestial.ref[3];
          prjs2x(&(native.prj), 1, 1, 1, 1, phi, theta, x, y, stat);
          xr[0] = -x[0];
          yr[0] =  y[0];

          cpgslw(5);
          cpgsci(9);
          cpgpt1(xr[0], yr[0], 24);
          cpgpt1(xr[0], yr[0], 23);
          cpgsci(0);
          cpgpt1(xr[0], yr[0], 17);

          // Mark zero celestial longitude expected for "change of origin"
          // case with a thick yellow circle.
          if (celestial.euler[1] == 0.0 || celestial.euler[1] == 180.0) {
            if (celestial.theta0 == 90.0) {
              alpha_p = celestial.ref[0];
            } else if (fabs(celestial.ref[1]) == 90.0) {
              alpha_p = celestial.ref[0];
            } else if (celestial.euler[1] == 0.0) {
              alpha_p = celestial.ref[0] + celestial.ref[2] -
                        celestial.phi0 - 180.0;
            } else {
              alpha_p = celestial.ref[0] - celestial.ref[2] +
                        celestial.phi0;
            }

            if (celestial.euler[1] == 0.0) {
              phi[0] = celestial.euler[2] - alpha_p + 180.0;
            } else {
              phi[0] = celestial.euler[2] + alpha_p;
            }

            phi[0] = fmod(phi[0], 360.0);
            if (phi[0] < -180.0) {
              phi[0] += 360.0;
            } else if (phi[0] > 180.0) {
              phi[0] -= 360.0;
            }

            theta[0] = -45.0;
            prjs2x(&(native.prj), 1, 1, 1, 1, phi, theta, x, y, stat);
            xr[0] = -x[0];
            yr[0] =  y[0];

            cpgslw(5);
            cpgsci(1);
            cpgpt1(xr[0], yr[0], 24);
          }

          cpgslw(1);

          // Flush PGPLOT buffer.
        skip:
          cpgebuf();
          if ((ctrl >= 'A' && ctrl <= 'Z') ||
             ((ctrl == 'c' || ctrl == 'b' || ctrl == 'j') && !first)) {
            // Keep going.

          } else {
            printf("Next, skip, break, continue, exit [%c]: ", ctrl);
            if (fgets(answer, 16, stdin) == 0) answer[0] = 0;

            if (strchr("bBcCeEiIjJnNpPqQsS", (int)answer[0]) != 0) {
              ctrl = answer[0];
            }
          }

          if (ctrl == 'i' || ctrl == 'I') {
            lonpole_i = 1;
          } else {
            lonpole_i = 15;
          }

          if (ctrl == 'P') {
            // There's no point in skipping all projections.
            ctrl = 'p';
            break;
          }

          if (ctrl == 'p') break;
          if (ctrl == 'b' || ctrl == 'B') break;
          if (ctrl == 'j' || ctrl == 'J') break;
          if (ctrl == 'e' || ctrl == 'E') goto end;
          if (ctrl == 'q' || ctrl == 'Q') goto end;

          first = 0;
        }

        if (ctrl == 'p') break;
        if (ctrl == 'j' || ctrl == 'J') break;
      }

      if (ctrl == 'p') break;
      if (ctrl == 'j' || ctrl == 'J') {
        // Save CRVAL1 and LONPOLE for next time.
        crval1_j  = crval1;
        lonpole_j = lonpole;

        // Slow down CRVAL2.
        crval2_i  = 1;
      } else {
        crval2_i  = 45;
      }
    }
  }

end:
  cpgask(0);
  cpgend();

  return 0;
}
