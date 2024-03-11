import sys
import numpy as np
import healpy as hp

nside = int(sys.argv[1])
nested  = bool(int(sys.argv[4]))
degrees = bool(int(sys.argv[5]))

# print("Nested: ",nested, 'Degrees: ",degrees)

try:
    RA  = float(sys.argv[2])
    Dec = float(sys.argv[3])

    pixel_numbers = hp.pixelfunc.ang2pix(nside, RA, Dec, nest=nested, lonlat=degrees)

except ValueError:  # If the argument is not convertible to float, treat it as a filename
    filename = sys.argv[2]
    with open(filename, 'r') as file:
        data = np.genfromtxt(file, dtype=float)
        RA  = data[:, 0]
        Dec = data[:, 1]

    pixel_numbers = hp.pixelfunc.ang2pix(nside, RA, Dec, nest=nested, lonlat=degrees)

# print("Nearest pixel numbers:", pixel_numbers)

filename = f"healpix_nside_{nside}_pixelnumbers.txt"
np.savetxt(filename, np.array([pixel_numbers]),fmt='%d')
