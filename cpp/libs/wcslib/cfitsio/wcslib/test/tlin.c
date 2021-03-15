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
  $Id: tlin.c,v 7.3.1.2 2020/08/17 11:20:48 mcalabre Exp mcalabre $
*=============================================================================
*
*  tlin tests the linear transformation routines supplied with WCSLIB.
*
*---------------------------------------------------------------------------*/

#include <math.h>
#include <stdlib.h>
#include <stdio.h>

#include <lin.h>

int NAXIS  = 5;
int NCOORD = 2;
int NELEM  = 9;

double CRPIX[5] =  {256.0, 256.0,  64.0, 128.0,   1.0};
double PC[5][5] = {{  1.0,   0.5,   0.0,   0.0,   0.0},
                   {  0.5,   1.0,   0.0,   0.0,   0.0},
                   {  0.0,   0.0,   1.0,   0.0,   0.0},
                   {  0.0,   0.0,   0.0,   1.0,   0.0},
                   {  0.0,   0.0,   0.0,   0.0,   1.0}};
double CDELT[5] =  {  1.2,   2.3,   3.4,   4.5,   5.6};

double pix0[2][9] = {{303.0, 265.0, 112.4, 144.5,  28.2, 0.0, 0.0, 0.0, 0.0},
                     { 19.0,  57.0,   2.0,  15.0,  42.0, 0.0, 0.0, 0.0, 0.0}};

const double tol = 1.0e-13;


int main()

{
  int i, j, k, nFail, status;
  double img[2][9], *pcij, pix[2][9], resid, residmax;
  struct linprm lin;


  printf("Testing WCSLIB linear transformation routines (tlin.c)\n"
         "------------------------------------------------------\n");

  // List status return messages.
  printf("\nList of lin status return values:\n");
  for (status = 1; status <= 6; status++) {
    printf("%4d: %s.\n", status, lin_errmsg[status]);
  }


  lin.flag = -1;
  linini(1, NAXIS, &lin);

  pcij = lin.pc;
  for (i = 0; i < lin.naxis; i++) {
    lin.crpix[i] = CRPIX[i];

    for (j = 0; j < lin.naxis; j++) {
      *(pcij++) = PC[i][j];
    }

    lin.cdelt[i] = CDELT[i];
  }

  for (k = 0; k < NCOORD; k++) {
    printf("\nPIX %d:", k+1);
    for (j = 0; j < NAXIS; j++) {
      printf("%14.8f", pix0[k][j]);
    }
  }
  printf("\n");

  if ((status = linp2x(&lin, NCOORD, NELEM, pix0[0], img[0]))) {
    printf("linp2x ERROR %d\n", status);
    return 1;
  }

  for (k = 0; k < NCOORD; k++) {
    printf("\nIMG %d:", k+1);
    for (j = 0; j < NAXIS; j++) {
      printf("%14.8f", img[k][j]);
    }
  }
  printf("\n");

  if ((status = linx2p(&lin, NCOORD, NELEM, img[0], pix[0]))) {
    printf("linx2p ERROR %d\n", status);
    return 1;
  }

  for (k = 0; k < NCOORD; k++) {
    printf("\nPIX %d:", k+1);
    for (j = 0; j < NAXIS; j++) {
      printf("%14.8f", pix[k][j]);
    }
  }
  printf("\n");

  // Check closure.
  nFail = 0;
  residmax = 0.0;
  for (k = 0; k < NCOORD; k++) {
    for (j = 0; j < NAXIS; j++) {
      resid = fabs(pix[k][j] - pix0[k][j]);
      if (residmax < resid) residmax = resid;
      if (resid > tol) nFail++;
    }
  }

  printf("\nlinp2x/linx2p: Maximum closure residual = %.1e pixel.\n",
    residmax);

  linfree(&lin);


  if (nFail) {
    printf("\nFAIL: %d closure residuals exceed reporting tolerance.\n",
      nFail);
  } else {
    printf("\nPASS: All closure residuals are within reporting tolerance.\n");
  }

  return nFail;
}
