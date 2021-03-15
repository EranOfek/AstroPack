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
  $Id: ttab1.c,v 7.3.1.2 2020/08/17 11:20:48 mcalabre Exp mcalabre $
*=============================================================================
*
* ttab1 tests the -TAB routines for closure.
*
*---------------------------------------------------------------------------*/

#include <math.h>
#include <stdio.h>

#include <tab.h>

// Reporting tolerance.
const double tol = 1.0e-8;

int main()

{
  char   nl;
  int    i, j, K[2], K1, K2, k, k1, k2, M, m, map[2], n, nFail = 0,
         stat0[128], stat1[128], status, status0, status1;
  double crpix, crval[2], epsilon, *index[1], psi_0, psi_1, resid, residmax,
         s[16], world[11][11][2], xt0[16], xt1[16], x0[11][11][2],
         x1[11][11][2], z;
  struct tabprm tab;

  printf(
    "Testing closure of WCSLIB tabular coordinate routines (ttab1.c)\n"
    "---------------------------------------------------------------\n");

  // List status return messages.
  printf("\nList of tab status return values:\n");
  for (status = 1; status <= 5; status++) {
    printf("%4d: %s.\n", status, tab_errmsg[status]);
  }

  printf("\nReporting tolerance %5.1G.\n", tol);


  // First a 1-dimensional table without index.
  printf("\nOne-dimensional test without index:\n");
  M = 1;
  K[0] = 10;

  tab.flag  = -1;
  tab.index = index;
  if ((status = tabini(1, M, K, &tab))) {
    printf("tabini ERROR %d: %s.\n", status, tab_errmsg[status]);
    return 1;
  }

  tab.M = M;
  tab.K[0] = K[0];
  tab.map[0] = 0;
  tab.crval[0] = 0.0;

  tab.index[0] = 0x0;

  tab.coord[0] = 101.0;
  tab.coord[1] = 102.0;
  tab.coord[2] = 104.0;
  tab.coord[3] = 107.0;
  tab.coord[4] = 111.0;
  tab.coord[5] = 116.0;
  tab.coord[6] = 122.0;
  tab.coord[7] = 129.0;
  tab.coord[8] = 137.0;
  tab.coord[9] = 146.0;

  xt0[0]  =  1.0;
  xt0[1]  =  2.0;
  xt0[2]  =  3.0;
  xt0[3]  =  4.0;
  xt0[4]  =  5.0;
  xt0[5]  =  6.0;
  xt0[6]  =  7.0;
  xt0[7]  =  8.0;
  xt0[8]  =  9.0;
  xt0[9]  = 10.0;

  xt0[10] =  0.5;
  xt0[11] =  1.1;
  xt0[12] =  2.5;
  xt0[13] =  4.7;
  xt0[14] =  8.125;
  xt0[15] = 10.5;

  status0 = tabx2s(&tab, 16, 1, (double *)xt0, (double *)s, stat0);
  status1 = tabs2x(&tab, 16, 1, (double *)s, (double *)xt1, stat1);

  printf("    x   ->     s    ->   x\n");
  for (i = 0; i < 16; i++) {
    printf("%8.5f%12.5f%9.5f", xt0[i], s[i], xt1[i]);
    if (stat0[i] || stat1[i]) {
      printf("  ERROR\n");
    } else {
      printf("\n");
    }
    if (i == 9) printf("\n");
  }

  if (status0) {
    printf("\ntabx2s ERROR %d: %s.\n", status0, tab_errmsg[status0]);

    for (i = 0; i < 16; i++) {
      if (stat0[i]) {
        printf("   tabx2s: x = %6.1f, stat = %d\n", xt0[i], stat0[i]);
      }
    }
  }

  if (status1) {
    printf("\ntabs2x ERROR %d: %s.\n", status1, tab_errmsg[status1]);

    for (i = 0; i < 16; i++) {
      if (stat1[i]) {
        printf("   tabs2x: s = %6.1f, stat = %d\n", s[i], stat1[i]);
      }
    }
  }

  // Test closure.
  nl = 1;
  residmax = 0.0;
  for (i = 0; i < 16; i++) {
    if (stat0[i] || stat1[i]) continue;

    resid = fabs(xt1[i] - xt0[i]);
    if (resid > residmax) residmax = resid;

    if (resid > tol) {
      nFail++;
      if (nl) printf("\n");
      printf("   Closure error:\n");
      printf("      x = %20.15f\n", xt0[i]);
      printf("   -> s = %20.15f\n", s[i]);
      printf("   -> x = %20.15f\n", xt1[i]);
      nl = 0;
    }
  }

  // Check error handling.
  printf("\n");
  printf("------------------------------------"
         "------------------------------------\n"
         "Test of error handling in tabx2s() and tabs2x().\n");
  xt0[0] =  0.0;
  xt0[1] = 11.0;
  status0 = tabx2s(&tab, 2, 1, (double *)xt0, (double *)s, stat0);

  if (status0) {
    printf("\ntabx2s ERROR %d: %s.\n", status0, tab_errmsg[status0]);

    for (i = 0; i < 2; i++) {
      if (stat0[i]) {
        printf("   tabx2s: x = %6.1f, stat = %d\n", xt0[i], stat0[i]);
      }
    }
  }

  s[0] =   0.0;
  s[1] = 200.0;
  status1 = tabs2x(&tab, 2, 1, (double *)s, (double *)xt1, stat1);

  if (status1) {
    printf("\ntabs2x ERROR %d: %s.\n", status1, tab_errmsg[status1]);

    for (i = 0; i < 2; i++) {
      if (stat1[i]) {
        printf("   tabs2x: s = %6.1f, stat = %d\n", s[i], stat1[i]);
      }
    }
  }

  printf("\nReports of four invalid coordinates are expected.\n"
         "------------------------------------"
         "------------------------------------\n");

  tabfree(&tab);
  tab.index = 0x0;


  // Now the 1-dimensional table from Sect. 6.2.3 of Paper III.
  printf("\n\nOne-dimensional test with index:\n");
  M = 1;
  K[0] = 8;

  tab.flag  = -1;
  if ((status = tabini(1, M, K, &tab))) {
    printf("tabini ERROR %d: %s.\n", status, tab_errmsg[status]);
    return 1;
  }

  tab.M = M;
  tab.K[0] = K[0];
  tab.map[0] = 0;
  tab.crval[0] = 0.0;

  tab.index[0][0] = 0.0;
  tab.index[0][1] = 1.0;
  tab.index[0][2] = 1.0;
  tab.index[0][3] = 2.0;
  tab.index[0][4] = 2.0;
  tab.index[0][5] = 3.0;
  tab.index[0][6] = 3.0;
  tab.index[0][7] = 4.0;

  tab.coord[0] = 1997.84512;
  tab.coord[1] = 1997.84631;
  tab.coord[2] = 1993.28451;
  tab.coord[3] = 1993.28456;
  tab.coord[4] = 2001.59234;
  tab.coord[5] = 2001.59239;
  tab.coord[6] = 2002.18265;
  tab.coord[7] = 2002.18301;

  epsilon = 1e-3;
  crpix   = 0.5;
  xt0[0]  = 0.5 + epsilon - crpix;
  xt0[1]  = 1.0           - crpix;
  xt0[2]  = 1.5 - epsilon - crpix;
  xt0[3]  = 1.5 + epsilon - crpix;
  xt0[4]  = 2.0           - crpix;
  xt0[5]  = 2.5 - epsilon - crpix;
  xt0[6]  = 2.5 + epsilon - crpix;
  xt0[7]  = 3.0           - crpix;
  xt0[8]  = 3.5 - epsilon - crpix;
  xt0[9]  = 3.5 + epsilon - crpix;
  xt0[10] = 4.0           - crpix;
  xt0[11] = 4.5 - epsilon - crpix;

  status0 = tabx2s(&tab, 12, 1, (double *)xt0, (double *)s, stat0);
  status1 = tabs2x(&tab, 12, 1, (double *)s, (double *)xt1, stat1);

  printf("    x   ->   time   ->   x\n");
  for (i = 0; i < 12; i++) {
    printf("%8.5f%12.5f%9.5f", xt0[i], s[i], xt1[i]);
    if (stat0[i] || stat1[i]) {
      printf("  ERROR\n");
    } else {
      printf("\n");
    }
  }

  if (status0) {
    printf("\ntabx2s ERROR %d: %s.\n", status0, tab_errmsg[status0]);

    for (i = 0; i < 12; i++) {
      if (stat0[i]) {
        printf("   tabx2s: x = %6.1f, stat = %d\n", xt0[i], stat0[i]);
      }
    }
  }

  if (status1) {
    printf("\ntabs2x ERROR %d: %s.\n", status1, tab_errmsg[status1]);

    for (i = 0; i < 12; i++) {
      if (stat1[i]) {
        printf("   tabs2x: s = %6.1f, stat = %d\n", s[i], stat1[i]);
      }
    }
  }

  // Test closure.
  nl = 1;
  residmax = 0.0;
  for (i = 0; i < 12; i++) {
    if (stat0[i] || stat1[i]) continue;

    resid = fabs(xt1[i] - xt0[i]);
    if (resid > residmax) residmax = resid;

    if (resid > tol) {
      nFail++;
      if (nl) printf("\n");
      printf("   Closure error:\n");
      printf("      x = %20.15f\n", xt0[i]);
      printf("   -> s = %20.15f\n", s[i]);
      printf("   -> x = %20.15f\n", xt1[i]);
      nl = 0;
    }
  }

  tabfree(&tab);


  // Now a 2-dimensional table.
  printf("\n\nTwo-dimensional test with index:\n");
  M = 2;
  K[0] = K1 = 32;
  K[1] = K2 = 16;
  map[0] = 0;
  map[1] = 1;
  crval[0] =  4.0;
  crval[1] = -1.0;

  tab.flag = -1;
  if ((status = tabini(1, M, K, &tab))) {
    printf("tabini ERROR %d: %s.\n", status, tab_errmsg[status]);
    return 1;
  }

  // Set up the tabprm struct.
  tab.M = M;
  for (m = 0; m < tab.M; m++) {
    tab.K[m] = K[m];
    tab.map[m] = map[m];
    tab.crval[m] = crval[m];

    // Construct a trivial 0-relative index.
    for (k = 0; k < tab.K[m]; k++) {
      tab.index[m][k] = (double)k;
    }
  }

  // Construct a coordinate table.
  n = 0;
  z = 1.0 / (double)((K1-1) * (K2-1));
  for (k2 = 0; k2 < K2; k2++) {
    for (k1 = 0; k1 < K1; k1++) {
      tab.coord[n++] =  3.0*k1*k2*z;
      tab.coord[n++] = -1.0*(K1-k1-1)*k2*z + 0.01*k1;
    }
  }

  // Construct an array of intermediate world coordinates to transform.
  for (i = 0; i < 11; i++) {
    for (j = 0; j < 11; j++) {
      // Compute psi_m within bounds...
      psi_0 = i*(K1-1)/10.0;
      psi_1 = j*(K2-1)/10.0;

      // ...then compute x from it.
      x0[i][j][0] = psi_0 - crval[0];
      x0[i][j][1] = psi_1 - crval[1];
    }
  }

  // Transform them to and fro.
  status0 = tabx2s(&tab, 121, 2, (double *)x0, (double *)world, stat0);
  status1 = tabs2x(&tab, 121, 2, (double *)world, (double *)x1, stat1);

  // Print the results.
  printf("     x     ->     s       ->     x  \n");
  n = 0;
  for (i = 0; i < 11; i++) {
    for (j = 0; j < 11; j++, n++) {
      // Print every sixth one only.
      if (n%6) continue;

      printf("(%4.1f,%4.1f)  (%4.2f,%6.3f)  (%4.1f,%4.1f)",
        x0[i][j][0], x0[i][j][1], world[i][j][0], world[i][j][1],
        x1[i][j][0], x1[i][j][1]);
      if (stat0[n] || stat1[n]) {
        printf("  ERROR\n");
      } else {
        printf("\n");
      }
    }
  }

  if (status0) {
    printf("\ntabx2s ERROR %d: %s.\n", status0, tab_errmsg[status0]);

    n = 0;
    for (i = 0; i < 11; i++) {
      for (j = 0; j < 11; j++, n++) {
        if (stat0[n]) {
          printf("   tabx2s: x = (%6.1f,%6.1f), stat = %d\n", x0[i][j][0],
            x0[i][j][1], stat0[n]);
        }
      }
    }
  }

  if (status1) {
    printf("\ntabs2x ERROR %d: %s.\n", status1, tab_errmsg[status1]);

    n = 0;
    for (i = 0; i < 11; i++) {
      for (j = 0; j < 11; j++, n++) {
        if (stat1[n]) {
          printf("   tabs2x: s = (%6.1f,%6.1f), stat = %d\n",
            world[i][j][0], world[i][j][1], stat1[n]);
        }
      }
    }
  }

  // Check for closure.
  n  = 0;
  nl = 1;
  residmax = 0.0;
  for (i = 0; i < 11; i++) {
    for (j = 0; j < 11; j++, n++) {
      if (stat0[n] || stat1[n]) continue;

      for (m = 0; m < M; m++) {
        resid = fabs(x1[i][j][m] - x0[i][j][m]);
        if (resid > residmax) residmax = resid;

        if (resid > tol) {
          nFail++;
          if (nl) printf("\n");
          printf("   Closure error:\n");
          printf("      x = (%20.15f,%20.15f)\n", x0[i][j][0], x0[i][j][1]);
          printf("   -> w = (%20.15f,%20.15f)\n", world[i][j][0],
            world[i][j][1]);
          printf("   -> x = (%20.15f,%20.15f)\n", x1[i][j][0], x1[i][j][1]);
          nl = 0;

          break;
        }
      }
    }
  }

  printf("\ntabx2s/tabs2x: Maximum closure residual = %.1e\n", residmax);

  // Check error handling.
  printf("\n");
  printf("------------------------------------"
         "------------------------------------\n"
         "Test of error handling in tabx2s() and tabs2x().\n");

  x0[0][0][0] = -5.0;
  x0[0][0][1] =  1.0;
  x0[0][1][0] =  0.0;
  x0[0][1][1] =  0.0;
  x0[0][2][0] = 31.0;
  x0[0][2][1] =  1.0;
  x0[0][3][0] =  0.0;
  x0[0][3][1] = 17.0;
  status0 = tabx2s(&tab, 4, 2, (double *)x0, (double *)world, stat0);

  if (status0) {
    printf("\ntabx2s ERROR %d: %s.\n", status0, tab_errmsg[status0]);

    for (j = 0; j < 4; j++) {
      if (stat0[j]) {
        printf("   tabx2s: x = (%6.1f,%6.1f), stat = %d\n", x0[0][j][0],
          x0[0][j][1], stat0[j]);
      }
    }
  }

  world[0][0][0] = -1.0;
  world[0][0][1] =  0.0;
  world[0][1][0] =  0.0;
  world[0][1][1] = -2.0;
  world[0][2][0] =  4.0;
  world[0][2][1] =  0.0;
  world[0][3][0] =  0.0;
  world[0][3][1] =  1.0;
  status1 = tabs2x(&tab, 4, 2, (double *)world, (double *)x1, stat1);

  if (status1) {
    printf("\ntabs2x ERROR %d: %s.\n", status1, tab_errmsg[status1]);

    for (j = 0; j < 4; j++) {
      if (stat1[j]) {
        printf("   tabs2x: s = (%6.1f,%6.1f), stat = %d\n",
          world[0][j][0], world[0][j][1], stat1[j]);
      }
    }
  }

  printf("\nReports of eight invalid coordinates are expected.\n"
         "------------------------------------"
         "------------------------------------\n");

  tabfree(&tab);


  if (nFail) {
    printf("\nFAIL: %d closure residuals exceed reporting tolerance.\n",
      nFail);
  } else {
    printf("\nPASS: All closure residuals are within reporting tolerance.\n");
  }

  return nFail;
}
