import sys
import healpy as hp

pixel_numbers = hp.pixelfunc.ang2pix(sys.argv[1], sys.argv[2], sys.argv[3], nest = False, lonlat = True)
print("Nearest pixel numbers:", pixel_numbers)

