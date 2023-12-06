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
  $Id: twcsmix.c,v 7.3.1.2 2020/08/17 11:20:48 mcalabre Exp mcalabre $
*=============================================================================
*
* twcsmix tests wcsmix() for closure on the 1 degree celestial graticule for
* a number of selected projections.  Points with good solutions are marked
* with a white dot on a graphical display of the projection while bad
* solutions are flagged with a red circle.
*
*---------------------------------------------------------------------------*/

#include <cpgplot.h>
#include <math.h>
#include <stdio.h>
#include <string.h>

#include <sph.h>
#include <wcs.h>


void mixex(double, double, double, double);
void id(struct wcsprm *, int *, double, double, double, double);
void grdplt(struct wcsprm *, double, double, double, double);
void parser(struct wcsprm *);

// Set to 1 to skip wcsmix(), primarily for debugging purposes.
const int skip_wcsmix = 0;

// Reporting tolerance for mixex().
const double tol = 1.0e-9;


// In real life these would be encoded as FITS header keyrecords.
const int NAXIS = 4;
const double CRPIX[4] =  {  513.0,  0.0,  0.0,  0.0};
const double PC[4][4] = {{    1.1,  0.0,  0.0,  0.0},
                         {    0.0,  1.0,  0.0,  0.1},
                         {    0.0,  0.0,  1.0,  0.0},
                         {    0.0,  0.2,  0.0,  1.0}};
const double CDELT[4] =  {-9.635265432e-6, 1.0, 1.0, -1.0};

				// The "xxx" is reset in main().
char CTYPE[4][9] = {"WAVE-F2W", "XLAT-xxx", "TIME    ", "XLON-xxx"};

				// Will be reset in mixex().
double CRVAL[4] = {0.214982042, -30.0, -2e3, 150.0};
double LONPOLE  = 150.0;
double LATPOLE  = 999.0;
double RESTFRQ  =   1.42040575e9;
double RESTWAV  =   0.0;

int NPV;
struct pvcard PV[10];		// Projection parameters are set in main().

int main()

{
  char   text[80];
  register int status;


  printf("Testing WCSLIB wcsmix() routine (twcsmix.c)\n"
         "-------------------------------------------\n");

  // List status return messages.
  printf("\nList of wcs status return values:\n");
  for (status = 1; status <= 13; status++) {
    printf("%4d: %s.\n", status, wcs_errmsg[status]);
  }


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
  cpgscr(9, 1.00f, 0.75f, 0.00f);


  //------------------------------------------------------------
  // Set the PVi_m keyvalues for the longitude axis so that
  // the fiducial native coordinates are at the native pole,
  // i.e. (phi0,theta0) = (0,90), but without any fiducial
  // offset.  We do this as a test, and also so that all
  // projections will be exercised with the same obliquity
  // parameters.
  //------------------------------------------------------------
  PV[0].i = 4;			// Longitude is on axis 4.
  PV[0].m = 1;			// Parameter number 1.
  PV[0].value =  0.0;		// Fiducial native longitude.

  PV[1].i = 4;			// Longitude is on axis 4.
  PV[1].m = 2;			// Parameter number 2.
  PV[1].value = 90.0;		// Fiducial native latitude.

  // Set the PVi_m keyvalues for the latitude axis.
  PV[2].i = 2;			// Latitude is on axis 2.
  PV[2].m = 1;			// Parameter number 1.
  PV[2].value = 0.0;		// PVi_1 (set below).

  PV[3].i = 2;			// Latitude is on axis 2.
  PV[3].m = 2;			// Parameter number 2.
  PV[3].value = 0.0;		// PVi_2 (set below).

  // ARC: zenithal/azimuthal equidistant.
  memcpy(&CTYPE[1][5], "ARC", 3);
  memcpy(&CTYPE[3][5], "ARC", 3);
  NPV = 2;
  mixex(-190.0, 190.0, -190.0, 190.0);

  // ZEA: zenithal/azimuthal equal area.
  memcpy(&CTYPE[1][5], "ZEA", 3);
  memcpy(&CTYPE[3][5], "ZEA", 3);
  NPV = 2;
  mixex(-120.0, 120.0, -120.0, 120.0);

  // CYP: cylindrical perspective.
  memcpy(&CTYPE[1][5], "CYP", 3);
  memcpy(&CTYPE[3][5], "CYP", 3);
  NPV = 4;
  PV[2].value = 3.0;
  PV[3].value = 0.8;
  mixex(-170.0, 170.0, -170.0, 170.0);

  // CEA: cylindrical equal area.
  memcpy(&CTYPE[1][5], "CEA", 3);
  memcpy(&CTYPE[3][5], "CEA", 3);
  NPV = 3;
  PV[2].value = 0.75;
  mixex(-200.0, 200.0, -200.0, 200.0);

  // CAR: plate carree.
  memcpy(&CTYPE[1][5], "CAR", 3);
  memcpy(&CTYPE[3][5], "CAR", 3);
  NPV = 2;
  mixex(-210.0, 210.0, -210.0, 210.0);

  // SFL: Sanson-Flamsteed.
  memcpy(&CTYPE[1][5], "SFL", 3);
  memcpy(&CTYPE[3][5], "SFL", 3);
  NPV = 2;
  mixex(-190.0, 190.0, -190.0, 190.0);

  // PAR: parabolic.
  memcpy(&CTYPE[1][5], "PAR", 3);
  memcpy(&CTYPE[3][5], "PAR", 3);
  NPV = 2;
  mixex(-190.0, 190.0, -190.0, 190.0);

  // MOL: Mollweide's projection.
  memcpy(&CTYPE[1][5], "MOL", 3);
  memcpy(&CTYPE[3][5], "MOL", 3);
  NPV = 2;
  mixex(-170.0, 170.0, -170.0, 170.0);

  // AIT: Hammer-Aitoff.
  memcpy(&CTYPE[1][5], "AIT", 3);
  memcpy(&CTYPE[3][5], "AIT", 3);
  NPV = 2;
  mixex(-170.0, 170.0, -170.0, 170.0);

  // COE: conic equal area.
  memcpy(&CTYPE[1][5], "COE", 3);
  memcpy(&CTYPE[3][5], "COE", 3);
  NPV = 4;
  PV[2].value = 60.0;
  PV[3].value = 15.0;
  mixex(-140.0, 140.0, -120.0, 160.0);

  // COD: conic equidistant.
  memcpy(&CTYPE[1][5], "COD", 3);
  memcpy(&CTYPE[3][5], "COD", 3);
  NPV = 4;
  PV[2].value = 60.0;
  PV[3].value = 15.0;
  mixex(-200.0, 200.0, -180.0, 220.0);

  // BON: Bonne's projection.
  memcpy(&CTYPE[1][5], "BON", 3);
  memcpy(&CTYPE[3][5], "BON", 3);
  NPV = 3;
  PV[2].value = 30.0;
  mixex(-160.0, 160.0, -160.0, 160.0);

  // PCO: polyconic.
  memcpy(&CTYPE[1][5], "PCO", 3);
  memcpy(&CTYPE[3][5], "PCO", 3);
  NPV = 2;
  mixex(-190.0, 190.0, -190.0, 190.0);

  // TSC: tangential spherical cube.
  memcpy(&CTYPE[1][5], "TSC", 3);
  memcpy(&CTYPE[3][5], "TSC", 3);
  NPV = 2;
  mixex(-340.0, 80.0, -210.0, 210.0);

  // QSC: quadrilateralized spherical cube.
  memcpy(&CTYPE[1][5], "QSC", 3);
  memcpy(&CTYPE[3][5], "QSC", 3);
  NPV = 2;
  mixex(-340.0, 80.0, -210.0, 210.0);

  cpgend();

  return 0;
}


/*----------------------------------------------------------------------------
*   mixex() tests wcsmix().
*---------------------------------------------------------------------------*/

void mixex(imin, imax, jmin, jmax)

double imax, imin, jmax, jmin;

{
  int    doid, lat, lng, wcslat, wcslng, stat, status;
  float  ipt[1], jpt[1];
  double lng1, lat1, phi, theta;
  double latspan[2], lngspan[2];
  double img[4], pix1[4], pix2[4], pix3[4], world[4];
  double pixlng, pixlat, *worldlat, *worldlng;
  struct wcsprm wcs;
  struct prjprm *wcsprj = &(wcs.cel.prj);


  // This routine simulates the actions of a FITS header parser.
  wcs.flag = -1;
  parser(&wcs);


  // Draw the coordinate graticule.
  grdplt(&wcs, imin, imax, jmin, jmax);
  if (skip_wcsmix) return;


  printf("Testing %s; reporting tolerance %5.1g deg.\n", wcsprj->code, tol);

  // Cache frequently used values.
  wcslng = wcs.lng;
  wcslat = wcs.lat;
  worldlng = world + wcslng;
  worldlat = world + wcslat;

  world[0] = 0.0;
  world[1] = 0.0;
  world[2] = 0.0;
  world[3] = 0.0;

  world[wcs.spec] = 2.99792458e8 / RESTFRQ;

  for (lat = 90; lat >= -90; lat--) {
    lat1 = (double)lat;

    for (lng = -180; lng <= 180; lng+=15) {
      lng1 = (double)lng;

      *worldlng = lng1;
      *worldlat = lat1;
      if ((status = wcss2p(&wcs, 1, 4, world, &phi, &theta, img, pix1,
                           &stat))) {
        printf("%3s(s2x): lng1 =%20.15f  lat1 =%20.15f  ERROR %3d\n",
          wcsprj->code, lng1, lat1, status);
        continue;
      }

      pixlng = pix1[wcslng];
      pixlat = pix1[wcslat];

      ipt[0] = pixlng;
      jpt[0] = pixlat;
      cpgpt(1, ipt, jpt, -1);

      lngspan[0] = lng1 - 9.3;
      if (lngspan[0] < -180.0) lngspan[0] = -180.0;
      lngspan[1] = lng1 + 4.1;
      if (lngspan[1] >  180.0) lngspan[1] =  180.0;
      latspan[0] = lat1 - 3.7;
      if (latspan[0] <  -90.0) latspan[0] =  -90.0;
      latspan[1] = lat1 + 7.2;
      if (latspan[1] >   90.0) latspan[1] =   90.0;

      doid = 1;

      pix2[wcslng] = pixlng;
      if ((status = wcsmix(&wcs, wcslng, 1, latspan, 1.0, 0, world, &phi,
                           &theta, img, pix2))) {
        id(&wcs, &doid, lng1, lat1, pixlng, pixlat);
        printf("  A: wcsmix ERROR %d: %s.\n", status, wcs_errmsg[status]);

      } else if ((status = wcss2p(&wcs, 1, 0, world, &phi, &theta, img,
                                  pix3, &stat))) {
        id(&wcs, &doid, lng1, lat1, pixlng, pixlat);
        printf("  A: wcss2p ERROR %d: %s.\n", status, wcs_errmsg[status]);

      } else if (fabs(pix3[wcslng]-pixlng) > tol &&
                (fabs(*worldlat-lat1)  > tol ||
                 fabs(pix2[wcslat]-pixlat) > tol)) {
        id(&wcs, &doid, lng1, lat1, pixlng, pixlat);
        printf("  A: (lng2) =%20.15f   lat2  =%20.15f\n", *worldlng,
          *worldlat);
        printf("       phi  =%20.15f  theta  =%20.15f\n", phi, theta);
        printf("       (i2) =%20.15f     j2  =%20.15f\n", pix2[wcslng],
          pix2[wcslat]);
      }


      pix2[wcslat] = pixlat;
      if ((status = wcsmix(&wcs, wcslat, 1, latspan, 1.0, 0, world, &phi,
                           &theta, img, pix2))) {
        id(&wcs, &doid, lng1, lat1, pixlng, pixlat);
        printf("  B: wcsmix ERROR %d: %s.\n", status, wcs_errmsg[status]);

      } else if ((status = wcss2p(&wcs, 1, 0, world, &phi, &theta, img,
                                  pix3, &stat))) {
        id(&wcs, &doid, lng1, lat1, pixlng, pixlat);
        printf("  B: wcss2p ERROR %d: %s.\n", status, wcs_errmsg[status]);

      } else if (fabs(pix3[wcslat]-pixlat) > tol &&
                (fabs(*worldlat-lat1)  > tol ||
                 fabs(pix2[wcslng]-pixlng) > tol)) {
        id(&wcs, &doid, lng1, lat1, pixlng, pixlat);
        printf("  B: (lng2) =%20.15f   lat2  =%20.15f\n", *worldlng,
          *worldlat);
        printf("       phi  =%20.15f  theta  =%20.15f\n", phi, theta);
        printf("        i2  =%20.15f    (j2) =%20.15f\n", pix2[wcslng],
          pix2[wcslat]);
      }

      *worldlat = lat1;

      pix2[wcslng] = pixlng;
      if ((status = wcsmix(&wcs, wcslng, 2, lngspan, 1.0, 0, world, &phi,
                           &theta, img, pix2))) {
        id(&wcs, &doid, lng1, lat1, pixlng, pixlat);
        printf("  C: wcsmix ERROR %d: %s.\n", status, wcs_errmsg[status]);

      } else if ((status = wcss2p(&wcs, 1, 0, world, &phi, &theta, img,
                                  pix3, &stat))) {
        id(&wcs, &doid, lng1, lat1, pixlng, pixlat);
        printf("  C: wcss2p ERROR %d: %s.\n", status, wcs_errmsg[status]);

      } else if (fabs(pix3[wcslng]-pixlng) > tol &&
                (fabs(*worldlng-lng1)  > tol ||
                 fabs(pix2[wcslat]-pixlat) > tol)) {
        id(&wcs, &doid, lng1, lat1, pixlng, pixlat);
        printf("  C:  lng2  =%20.15f  (lat2) =%20.15f\n", *worldlng,
          *worldlat);
        printf("       phi  =%20.15f  theta  =%20.15f\n", phi, theta);
        printf("       (i2) =%20.15f     j2  =%20.15f\n", pix2[wcslng],
          pix2[wcslat]);
      }


      pix2[wcslat] = pixlat;
      if ((status = wcsmix(&wcs, wcslat, 2, lngspan, 1.0, 0, world, &phi,
                           &theta, img, pix2))) {
        id(&wcs, &doid, lng1, lat1, pixlng, pixlat);
        printf("  D: wcsmix ERROR %d: %s.\n", status, wcs_errmsg[status]);

      } else if ((status = wcss2p(&wcs, 1, 0, world, &phi, &theta, img,
                                  pix3, &stat))) {
        id(&wcs, &doid, lng1, lat1, pixlng, pixlat);
        printf("  D: wcss2p ERROR %d: %s.\n", status, wcs_errmsg[status]);

      } else if (fabs(pix3[wcslat]-pixlat) > tol &&
                (fabs(*worldlng-lng1)  > tol ||
                 fabs(pix2[wcslng]-pixlng) > tol)) {
        id(&wcs, &doid, lng1, lat1, pixlng, pixlat);
        printf("  D:  lng2  =%20.15f  (lat2) =%20.15f\n", *worldlng,
          *worldlat);
        printf("       phi  =%20.15f  theta  =%20.15f\n", phi, theta);
        printf("        i2  =%20.15f    (j2) =%20.15f\n", pix2[wcslng],
          pix2[wcslat]);
      }

    }
  }

  wcsfree(&wcs);

  return;
}

//----------------------------------------------------------------------------

void id(wcs, doid, lng1, lat1, pixlng, pixlat)

struct wcsprm *wcs;
int    *doid;
double lng1, lat1, pixlng, pixlat;

{
  float  ipt[1], jpt[1];
  double phi, theta;
  struct celprm *wcscel = &(wcs->cel);
  struct prjprm *wcsprj = &(wcscel->prj);

  if (*doid) {
    // Compute native coordinates.
    sphs2x(wcscel->euler, 1, 1, 1, 1, &lng1, &lat1, &phi, &theta);

    printf("\n%3s:  lng1  =%20.15f   lat1  =%20.15f\n", wcsprj->code, lng1,
      lat1);
    printf(  "       phi  =%20.15f  theta  =%20.15f\n", phi, theta);
    printf(  "        i1  =%20.15f     j1  =%20.15f\n", pixlng, pixlat);
    *doid = 0;

    cpgsci(9);
    ipt[0] = pixlng;
    jpt[0] = pixlat;
    cpgpt(1, ipt, jpt, 21);
    cpgsci(2);
  }

  return;
}

//----------------------------------------------------------------------------

void grdplt(wcs, imin, imax, jmin, jmax)

struct wcsprm *wcs;
double imax, imin, jmax, jmin;

{
#define NELEM 9

  char   text[80];
  int    ci, ilat, ilng, j, k, stat[361];
  float  fimax, fimin, fjmax, fjmin, ir[1024], jr[1024];
  double freq, img[361][NELEM], lat, lng, phi[361], pix[361][NELEM], step,
         theta[361], world[361][NELEM];
  double *pixlat, *pixlng, *worldlat, *worldlng;
  struct wcsprm native;
  struct celprm *wcscel = &(wcs->cel);
  struct prjprm *wcsprj = &(wcscel->prj);
  struct prjprm *ntvprj = &(native.cel.prj);


  // Initialize non-celestial world coordinates.
  freq = 1.42040595e9 - 180.0 * 62500.0;
  for (j = 0; j < 361; j++) {
    world[j][0] = 0.0;
    world[j][1] = 0.0;
    world[j][2] = 0.0;
    world[j][3] = 0.0;

    world[j][wcs->spec] = 2.99792458e8 / freq;
    freq += 62500.0;
  }


  // Define PGPLOT viewport.
  fimax = (float)imax;
  fimin = (float)imin;
  fjmax = (float)jmax;
  fjmin = (float)jmin;
  cpgenv(fimin, fimax, fjmin, fjmax, 1, -2);

  // Draw face boundaries of the quad-cube projections.
  if (wcsprj->category == QUADCUBE) {
    cpgsci(8);

    // Draw the map boundary.
    for (j = 0; j < 9; j++) {
      img[j][0] = 0.0;
      img[j][1] = 0.0;
      img[j][2] = 0.0;
      img[j][3] = 0.0;
    }

    img[0][wcs->lng] = -wcsprj->w[0];
    img[0][wcs->lat] =  wcsprj->w[0];
    img[1][wcs->lng] = -wcsprj->w[0];
    img[1][wcs->lat] =  wcsprj->w[0]*3.0;
    img[2][wcs->lng] =  wcsprj->w[0];
    img[2][wcs->lat] =  wcsprj->w[0]*3.0;
    img[3][wcs->lng] =  wcsprj->w[0];
    img[3][wcs->lat] = -wcsprj->w[0]*3.0;
    img[4][wcs->lng] = -wcsprj->w[0];
    img[4][wcs->lat] = -wcsprj->w[0]*3.0;
    img[5][wcs->lng] = -wcsprj->w[0];
    img[5][wcs->lat] =  wcsprj->w[0];
    img[6][wcs->lng] =  wcsprj->w[0]*7.0;
    img[6][wcs->lat] =  wcsprj->w[0];
    img[7][wcs->lng] =  wcsprj->w[0]*7.0;
    img[7][wcs->lat] = -wcsprj->w[0];
    img[8][wcs->lng] = -wcsprj->w[0];
    img[8][wcs->lat] = -wcsprj->w[0];

    linx2p(&(wcs->lin), 9, NELEM, img[0], pix[0]);

    for (j = 0; j < 9; j++) {
      ir[j] = pix[j][wcs->lng];
      jr[j] = pix[j][wcs->lat];
    }

    cpgline(9, ir, jr);
  }

  if (wcsprj->category == POLYCONIC) {
    step = 10.0;
  } else {
    step = 15.0;
  }


  // Draw the native coordinate graticule faintly in the background.
  native.flag = -1;
  (void)wcscopy(1, wcs, &native);
  native.crval[wcs->lng] =  0.0;
  native.crval[wcs->lat] = 90.0;
  native.lonpole = 180.0;
  (void)wcssetprm(&native);

  cpgsci(8);

  // Draw native meridians of longitude.
  for (ilng = -180; ilng <= 180; ilng += 15) {
    lng = (double)ilng;
    if (ilng == -180) lng = -179.99;
    if (ilng ==  180) lng =  179.99;

    worldlng = world[0] + native.lng;
    worldlat = world[0] + native.lat;
    for (ilat = -90; ilat <= 90; ilat++) {
      *worldlng = lng;
      *worldlat = (double)ilat;

      worldlng += NELEM;
      worldlat += NELEM;
    }

    if (wcss2p(&native, 181, NELEM, world[0], phi, theta, img[0], pix[0],
               stat)) {
      continue;
    }

    k = 0;
    pixlng = pix[0] + native.lng;
    pixlat = pix[0] + native.lat;
    for (ilat = -90; ilat <= 90; ilat++) {
      if (ntvprj->category == QUADCUBE && k > 0) {
        if (fabs(*pixlng - ir[k-1]) > 2.0 ||
          fabs(*pixlat - jr[k-1]) > 5.0) {
          if (k > 1) cpgline(k, ir, jr);
          k = 0;
        }
      }

      ir[k] = *pixlng;
      jr[k] = *pixlat;
      k++;

      pixlng += NELEM;
      pixlat += NELEM;
    }

    cpgline(k, ir, jr);
  }

  // Draw native parallels of latitude.
  for (ilat = -90; ilat <= 90; ilat += 15) {
    lat = (double)ilat;

    worldlng = world[0] + native.lng;
    worldlat = world[0] + native.lat;
    for (ilng = -180; ilng <= 180; ilng++) {
      lng = (double)ilng;
      if (ilng == -180) lng = -179.99;
      if (ilng ==  180) lng =  179.99;

      *worldlng = lng;
      *worldlat = lat;

      worldlng += NELEM;
      worldlat += NELEM;
    }

    if (wcss2p(&native, 361, NELEM, world[0], phi, theta, img[0], pix[0],
               stat)) {
      continue;
    }

    k = 0;
    pixlng = pix[0] + native.lng;
    pixlat = pix[0] + native.lat;
    for (ilng = -180; ilng <= 180; ilng++) {
      if (ntvprj->category == QUADCUBE && k > 0) {
        if (fabs(*pixlng - ir[k-1]) > 2.0 ||
          fabs(*pixlat - jr[k-1]) > 5.0) {
          if (k > 1) cpgline(k, ir, jr);
          k = 0;
        }
      }

      ir[k] = *pixlng;
      jr[k] = *pixlat;
      k++;

      pixlng += NELEM;
      pixlat += NELEM;
    }

    cpgline(k, ir, jr);
  }

  wcsfree(&native);


  // Draw a colour-coded celestial coordinate graticule.
  ci = 1;

  // Draw celestial meridians of longitude.
  for (ilng = -180; ilng <= 180; ilng += 15) {
    lng = (double)ilng;

    if (++ci > 7) ci = 2;
    cpgsci(ilng?ci:1);

    worldlng = world[0] + wcs->lng;
    worldlat = world[0] + wcs->lat;
    for (ilat = -90; ilat <= 90; ilat++) {
      lat = (double)ilat;

      *worldlng = lng;
      *worldlat = lat;

      worldlng += NELEM;
      worldlat += NELEM;
    }

    if (wcss2p(wcs, 181, NELEM, world[0], phi, theta, img[0], pix[0],
               stat)) {
      continue;
    }

    k = 0;
    pixlng = pix[0] + wcs->lng;
    pixlat = pix[0] + wcs->lat;
    for (ilat = -90; ilat <= 90; ilat++) {
      // Test for discontinuities.
      if (k > 0) {
        if (fabs(*pixlng - ir[k-1]) > step ||
            fabs(*pixlat - jr[k-1]) > step) {
          if (k > 1) cpgline(k, ir, jr);
          k = 0;
        }
      }

      ir[k] = *pixlng;
      jr[k] = *pixlat;
      k++;

      pixlng += NELEM;
      pixlat += NELEM;
    }

    cpgline(k, ir, jr);
  }

  // Draw celestial parallels of latitude.
  ci = 1;
  for (ilat = -90; ilat <= 90; ilat += 15) {
    lat = (double)ilat;

    if (++ci > 7) ci = 2;
    cpgsci(ilat?ci:1);

    worldlng = world[0] + wcs->lng;
    worldlat = world[0] + wcs->lat;
    for (ilng = -180; ilng <= 180; ilng++) {
      lng = (double)ilng;

      *worldlng = lng;
      *worldlat = lat;

      worldlng += NELEM;
      worldlat += NELEM;
    }

    if (wcss2p(wcs, 361, NELEM, world[0], phi, theta, img[0], pix[0],
               stat)) {
      continue;
    }

    k = 0;
    pixlng = pix[0] + wcs->lng;
    pixlat = pix[0] + wcs->lat;
    for (ilng = -180; ilng <= 180; ilng++) {
      // Test for discontinuities.
      if (k > 0) {
        if (fabs(*pixlng - ir[k-1]) > step ||
            fabs(*pixlat - jr[k-1]) > step) {
          if (k > 1) cpgline(k, ir, jr);
          k = 0;
        }
      }

      ir[k] = *pixlng;
      jr[k] = *pixlat;
      k++;

      pixlng += NELEM;
      pixlat += NELEM;
    }

    cpgline(k, ir, jr);
  }


  // Write a descriptive title.
  cpgsci(1);
  sprintf(text, "%s projection - 15 degree graticule", wcsprj->code);
  printf("\n\n%s\n", text);
  fjmin = jmin - 10.0;
  cpgtext(fimin, fjmin, text);

  sprintf(text, "centered on celestial coordinates (%6.2f,%6.2f)",
    wcscel->ref[0], wcscel->ref[1]);
  printf("%s\n", text);
  fjmin = jmin - 20.0;
  cpgtext(fimin, fjmin, text);

  sprintf(text, "with celestial pole at native coordinates (%7.2f,%7.2f)",
    wcscel->ref[2], wcscel->ref[3]);
  printf("%s\n", text);
  fjmin = jmin - 30.0;
  cpgtext(fimin, fjmin, text);

  cpgsci(2);


  return;
}

//----------------------------------------------------------------------------

void parser(wcs)

struct wcsprm *wcs;

{
  int i, j, status;
  double *pcij;

  // In practice a parser would read the FITS header until it encountered
  // the NAXIS keyword which must occur near the start, before any of the
  // WCS keywords.  It would then use wcsini() to allocate memory for
  // arrays in the wcsprm struct and set default values.  In this
  // simulation the header keyvalues are set as global variables.
  wcsini(1, NAXIS, wcs);


  // Now the parser scans the FITS header, identifying WCS keywords and
  // loading their values into the appropriate elements of the wcsprm
  // struct.

  for (j = 0; j < NAXIS; j++) {
    wcs->crpix[j] = CRPIX[j];
  }

  pcij = wcs->pc;
  for (i = 0; i < NAXIS; i++) {
    for (j = 0; j < NAXIS; j++) {
      *(pcij++) = PC[i][j];
    }
  }

  for (i = 0; i < NAXIS; i++) {
    wcs->cdelt[i] = CDELT[i];
  }

  for (i = 0; i < NAXIS; i++) {
    strcpy(wcs->ctype[i], &CTYPE[i][0]);
  }

  for (i = 0; i < NAXIS; i++) {
    wcs->crval[i] = CRVAL[i];
  }

  wcs->lonpole = LONPOLE;
  wcs->latpole = LATPOLE;

  wcs->restfrq = RESTFRQ;
  wcs->restwav = RESTWAV;

  wcs->npv = NPV;
  for (i = 0; i < NPV; i++) {
    wcs->pv[i] = PV[i];
  }

  // Extract information from the FITS header.
  if ((status = wcssetprm(wcs))) {
    printf("wcsset ERROR%3d\n", status);
  }

  return;
}
