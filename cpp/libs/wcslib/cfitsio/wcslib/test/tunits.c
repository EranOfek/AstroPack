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
  $Id: tunits.c,v 7.3.1.2 2020/08/17 11:20:48 mcalabre Exp mcalabre $
*=============================================================================
*
* tunits tests wcsulex(), wcsutrn(), and wcsunits() the FITS units
* specification parser, translator and converter.
*
*---------------------------------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <wcsunits.h>

int main()

{
  char   have[80], want[80];
  int    func, i, interactive, status;
  double offset, power, scale, units[WCSUNITS_NTYPE];

  interactive = isatty(0);

  printf("Testing FITS unit specification parser (tunits.c)\n"
         "-------------------------------------------------\n");
  if (interactive) printf("\nTo test wcsulex(), enter <CR> when prompted "
                          "with \"Unit string (want):\".\n");

  while (1) {
    if (interactive) printf("\nUnit string (have): ");
    if (!fgets(have, 80, stdin)) break;
    have[strlen(have)-1] = '\0';
    if (!interactive) printf("\nUnit string (have): %s\n", have);

    if ((status = wcsutrn(7, have)) >= 0) {
      printf("       Translation: %s", have);
      if (status == 0) {
        printf("\n");
      } else {
        printf("   (WARNING: %s)\n", wcsunits_errmsg[status]);
      }
    }

    if (interactive) printf("Unit string (want): ");
    if (!fgets(want, 80, stdin)) break;
    want[strlen(want)-1] = '\0';

    if (*want) {
      if (!interactive) printf("Unit string (want): %s\n", want);

      if ((status = wcsutrn(7, want)) >= 0) {
        printf("       Translation: %s", want);
        if (status == 0) {
          printf("\n");
        } else {
          printf("   (WARNING: %s)\n", wcsunits_errmsg[status]);
        }
      }

      printf("Conversion: \"%s\" -> \"%s\"\n", have, want);

      if ((status = wcsunits(have, want, &scale, &offset, &power))) {
        printf("wcsunits ERROR %d: %s.\n", status,
               wcsunits_errmsg[status]);
        continue;
      }

      printf("            = %s", (power == 1.0) ? "" : "(");

      if (scale == 1.0) {
        printf("value");
      } else {
        printf("%.8g * value", scale);
      }

      if (offset != 0.0) {
        printf(" + %.8g", offset);
      }

      if (power == 1.0) {
        printf("\n");
      } else {
        printf(")^%.8g\n", power);
      }

    } else {
      // Parse the unit string.
      printf("   Parsing: \"%s\"\n", have);

      if ((status = wcsulex(have, &func, &scale, units))) {
        printf("wcsulex ERROR %d: %s.\n", status,
               wcsunits_errmsg[status]);
        continue;
      }

      printf("%15.8g *\n", scale);
      for (i = 0; i < WCSUNITS_NTYPE; i++) {
        if (units[i] != 0.0) {
          printf("%11.2f %s", units[i], wcsunits_types[i]);
          if (strlen(wcsunits_units[i])) {
            printf(" (%s)\n", wcsunits_units[i]);
          } else {
            printf("\n");
          }
        }
      }
    }
  }

  printf("\n");

  return 0;
}
