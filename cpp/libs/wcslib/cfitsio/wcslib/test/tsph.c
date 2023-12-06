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
  $Id: tsph.c,v 7.3.1.2 2020/08/17 11:20:48 mcalabre Exp mcalabre $
*=============================================================================
*
* tsph tests the spherical coordinate transformation routines for closure.
*
*---------------------------------------------------------------------------*/

#include <math.h>
#include <stdlib.h>
#include <stdio.h>

#include <sph.h>
#include <wcstrig.h>


int main()

{
  int   j, k, lat, lng, nFail = 0;
  double cel1[181][2], cel2[181][2], coslat, dlat, dlatmx, dlng, dlngmx,
         eul[5], lng1[361], lng2[361], lat1, lat2[361], ntv[181][2], phi[361],
         theta[361], zeta;
  const double tol = 1.0e-12;


  printf(
  "Testing closure of WCSLIB coordinate transformation routines (tsph.c)\n"
  "---------------------------------------------------------------------\n");

  printf ("Reporting tolerance:%8.1e degrees of arc.\n", tol);

  dlngmx = 0.0;
  dlatmx = 0.0;

  for (k = 0; k < 3; k++) {
    // Set reference angles.
    eul[0] =  90.0;
    eul[2] = -90.0;

    if (k < 2) {
      // Special-case transformations.
      eul[1] = (k==0) ? 0.0 : 180.0;
    } else {
      eul[1] =  30.0;
    }

    printf("\n%s\n%s%10.4f%10.4f%10.4f\n",
      "Celestial longitude and latitude of the native pole, and native",
      "longitude of the celestial pole (degrees):", eul[0], eul[1], eul[2]);

    eul[3] = cosd(eul[1]);
    eul[4] = sind(eul[1]);

    // Test points at constant latitude.
    for (lat = 90; lat >= -90; lat--) {
      lat1 = (double)lat;
      coslat = cosd(lat1);

      for (j = 0, lng = -180; lng <= 180; lng++, j++) {
        lng1[j] = (double)lng;
      }

      sphs2x(eul, 361, 1, 1, 1, lng1, &lat1, phi, theta);
      sphx2s(eul, 361, 0, 1, 1, phi, theta, lng2, lat2);

      // Exact results are expected for special-case transformations.
      if (k == 0) {
        // Identity transformation.
        for (j = 0; j <= 360; j++) {
          if (phi[j] != lng1[j] || theta[j] != lat1) {
            nFail++;
            printf("   Error: lng1 =%20.15f  lat1 =%20.15f\n",
                   lng1[j], lat1);
            printf("           phi =%20.15f theta =%20.15f\n",
                   phi[j], theta[j]);
          }
        }

      } else if (k == 1) {
        // Antipodal transformation.
        for (j = 0; j <= 360; j++) {
          if (phi[j] != -lng1[j] || theta[j] != -lat1) {
            nFail++;
            printf("   Error: lng1 =%20.15f  lat1 =%20.15f\n",
                   lng1[j], lat1);
            printf("           phi =%20.15f theta =%20.15f\n",
                   phi[j], theta[j]);
          }
        }
      }

      // Do another round trip, just for good measure.
      sphs2x(eul, 361, 0, 1, 1, lng2, lat2, phi, theta);
      sphx2s(eul, 361, 0, 1, 1, phi, theta, lng2, lat2);

      // Check closure.
      for (j = 0; j <= 360; j++) {
        dlng = fabs(lng2[j] - lng1[j]);
        if (dlng > 180.0) dlng = fabs(dlng-360.0);
        dlng *= coslat;
        dlat = fabs(lat2[j] - lat1);

        if (dlng > dlngmx) dlngmx = dlng;
        if (dlat > dlatmx) dlatmx = dlat;

        if (dlng > tol || dlat > tol) {
          nFail++;
          printf("Unclosed: lng1 =%20.15f  lat1 =%20.15f\n", lng1[j], lat1);
          printf("           phi =%20.15f theta =%20.15f\n", phi[j], theta[j]);
          printf("          lng2 =%20.15f  lat2 =%20.15f\n", lng2[j], lat2[j]);
         }
      }
    }

    // Test vector strides using points in spirals from south to north.
    for (lng = 0; lng <= 360; lng++) {
      for (j = 0, lat = -90; lat <= 90; j++, lat++) {
        cel1[j][0] = (double)((lng+j)%360 - 180);
        cel1[j][1] = (double)lat;
      }

      sphs2x(eul, 181, 0, 2, 2, &(cel1[0][0]), &(cel1[0][1]),
             &(ntv[0][0]), &(ntv[0][1]));
      sphx2s(eul, 181, 0, 2, 2, &(ntv[0][0]), &(ntv[0][1]),
             &(cel2[0][0]), &(cel2[0][1]));

      // Exact results are expected for special-case transformations.
      if (k == 0) {
        // Identity transformation.
        for (j = 0; j <= 180; j++) {
          if (ntv[j][0] != cel1[j][0] || ntv[j][1] != cel1[j][1]) {
            nFail++;
            printf("   Error: lng1 =%20.15f  lat1 =%20.15f\n",
                   cel1[j][0], cel1[j][1]);
            printf("           phi =%20.15f theta =%20.15f\n",
                   ntv[j][0], ntv[j][1]);
          }
        }

      } else if (k == 1) {
        // Antipodal transformation.
        for (j = 0; j <= 180; j++) {
          if (ntv[j][0] != -cel1[j][0] || ntv[j][1] != -cel1[j][1]) {
            nFail++;
            printf("   Error: lng1 =%20.15f  lat1 =%20.15f\n",
                   cel1[j][0], cel1[j][1]);
            printf("           phi =%20.15f theta =%20.15f\n",
                   ntv[j][0], ntv[j][1]);
          }
        }
      }

      // Check closure.
      for (j = 0; j <= 180; j++) {
        dlng = fabs(cel2[j][0] - cel1[j][0]);
        if (dlng > 180.0) dlng = fabs(dlng - 360.0);
        dlng *= cosd(cel1[j][1]);
        dlat = fabs(cel2[j][1] - cel1[j][1]);

        if (dlng > dlngmx) dlngmx = dlng;
        if (dlat > dlatmx) dlatmx = dlat;

        if (dlng > tol || dlat > tol) {
          nFail++;
          printf("Unclosed: lng1 =%20.15f  lat1 =%20.15f\n",
            cel1[j][0], cel1[j][1]);
          printf("           phi =%20.15f theta =%20.15f\n",
            ntv[j][0], ntv[j][1]);
          printf("          lng2 =%20.15f  lat2 =%20.15f\n",
            cel2[j][0], cel2[j][1]);
         }
      }
    }
  }


  // Test closure at points close to the pole.
  for (j = -1; j <= 1; j += 2) {
    zeta = 1.0;
    lng1[0] = -180.0;

    for (lat = 0; lat < 12; lat++) {
      lat1 = (double)j*(90.0 - zeta);

      sphs2x(eul, 1, 1, 1, 1, lng1, &lat1, phi, theta);
      sphx2s(eul, 1, 1, 1, 1, phi, theta, lng2, lat2);

      dlng = fabs(lng2[0] - lng1[0]);
      if (dlng > 180.0) dlng = fabs(dlng - 360.0);
      dlng *= coslat;
      dlat = fabs(lat2[0] - lat1);

      if (dlng > dlngmx) dlngmx = dlng;
      if (dlat > dlatmx) dlatmx = dlat;

      if (dlng > tol || dlat > tol) {
        nFail++;
        printf("Unclosed: lng1 =%20.15f  lat1 =%20.15f\n", lng1[0], lat1);
        printf("           phi =%20.15f theta =%20.15f\n", phi[0], theta[0]);
        printf("          lng2 =%20.15f  lat2 =%20.15f\n", lng2[0], lat2[0]);
      }

      zeta /= 10.0;
      lng1[0] += 30.0;
    }
  }

  printf("\nsphs2x/sphx2s: Maximum closure residual = %.1e (lng), %.1e (lat) "
    "deg.\n", dlngmx, dlatmx);


  if (nFail) {
    printf("\nFAIL: %d closure residuals exceed reporting tolerance.\n",
      nFail);
  } else {
    printf("\nPASS: All closure residuals are within reporting tolerance.\n");
  }

  return nFail;
}
