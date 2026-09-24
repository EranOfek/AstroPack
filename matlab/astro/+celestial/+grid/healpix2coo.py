import sys
import numpy as np
import healpy as hp

nside   = int(sys.argv[1])
nested  = bool(int(sys.argv[3]))
degrees = bool(int(sys.argv[4]))

# print("Nested: ",nested, "Degrees: ",degrees)

try:
    ipix = int(sys.argv[2])
    RA, Dec = hp.pixelfunc.pix2ang(nside, ipix, nest=nested, lonlat=degrees)

except ValueError:  # If the argument is not convertible to float, treat it as a filename
    filename = sys.argv[2]
    with open(filename, 'r') as file:
        ipix = np.genfromtxt(file, dtype=int)
    RA, Dec = hp.pixelfunc.pix2ang(nside, ipix, nest=nested, lonlat=degrees)

# print(RA, Dec)

filename = f"healpix_nside_{nside}_coo.txt"
np.savetxt(filename, np.column_stack((RA, Dec)),fmt='%.4f %.4f')
