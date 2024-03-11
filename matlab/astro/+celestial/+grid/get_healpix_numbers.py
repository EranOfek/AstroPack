import sys
import healpy as hp

nside = int(sys.argv[1])
RA    = float(sys.argv[2])
Dec   = float(sys.argv[3])

# print("Input:", nside, RA, Dec)

pixel_numbers = hp.pixelfunc.ang2pix(nside, RA, Dec, nest = False, lonlat = True)
print("Nearest pixel numbers:", pixel_numbers)

