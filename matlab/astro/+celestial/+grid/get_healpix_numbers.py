import sys
import numpy as np
import healpy as hp

nside = int(sys.argv[1])

try:
    RA  = float(sys.argv[2])
    Dec = float(sys.argv[3])

    pixel_numbers = hp.pixelfunc.ang2pix(nside, RA, Dec, nest=False, lonlat=True)

except ValueError:  # If the argument is not convertible to float, treat it as a filename
    filename = sys.argv[2]
    with open(filename, 'r') as file:
        data = np.genfromtxt(file, dtype=float)
        RA  = data[:, 0]
        Dec = data[:, 1]

    pixel_numbers = hp.pixelfunc.ang2pix(nside, RA, Dec, nest=False, lonlat=True)

# print("Nearest pixel numbers:", pixel_numbers)

filename = f"healpix_nside_{nside}_pixelnumbers.txt"
np.savetxt(filename, pixel_numbers,fmt='%d')
