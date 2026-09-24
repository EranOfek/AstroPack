from astropy import units as u
from astropy_healpix import HEALPix
import argparse

def main(nside, ra, dec, radius):
    hp = HEALPix(nside=nside, order='nested')

    # Perform cone search
    pixels = hp.cone_search_lonlat(ra * u.deg, dec * u.deg, radius=radius * u.deg)
    
    # Print the list of pixels
    # print("HEALPix pixels within the radius:", pixels)
    # print(ra, dec, radius, nside)
    print(' '.join(map(str, pixels)))

if __name__ == "__main__":
    # Set up command line argument parsing
    parser = argparse.ArgumentParser(description="HEALPix cone search.")
    parser.add_argument("nside", type=int, help="HEALPix resolution parameter (Nside).")
    parser.add_argument("ra", type=float, help="Right Ascension (RA) of the point in degrees.")
    parser.add_argument("dec", type=float, help="Declination (Dec) of the point in degrees.")
    parser.add_argument("radius", type=float, help="Search radius in degrees.")

    # Parse the arguments
    args = parser.parse_args()

    # Call the main function with parsed arguments
    main(args.nside, args.ra, args.dec, args.radius)

