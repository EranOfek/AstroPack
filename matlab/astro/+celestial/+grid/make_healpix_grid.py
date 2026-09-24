import sys
import numpy as np
import healpy as hp

def main(nside):

# Get the coordinates of the centers of N equal areas:

# Number of areas = 41253 deg^2 / pixel area
# Number of pixels is 12 * nside^2 (should nside be always a power of 2?)
# nside-area: 4 -- 215 deg^2, 5 -- 137.5 deg^2, 8 -- 54 deg^2, 128 -- 0.2 deg^2, 1024 -- 0.03 deg^2

    N = 12 * nside ** 2
    lon, lat = get_equal_area_centers(nside, N)

    parea = round(hp.nside2pixarea(nside, degrees=True),3)

    # Print the coordinates
    filename = f"healpix_grid_nside_{nside}_npix_{N}_pixarea_{parea}_deg.txt"
    with open(filename, 'w') as outfile:
    	for i in range(N):
    		print(f"{lon[i]}, {lat[i]}",file=outfile)

def get_equal_area_centers(nside, N):
    """
    Get the sky coordinates of the centers of N areas of equal angular size.

    Returns:
    - lon: Array of longitudes (in degrees) of the area centers
    - lat: Array of latitudes (in degrees) of the area centers
    """

    # Total number of pixels in the sphere
    npix = hp.nside2npix(nside)

    # Calculate the area of each pixel in square degrees
    pixel_area = hp.nside2pixarea(nside, degrees=True)

    # Calculate the target area for each of the N areas
    target_area = 4 * np.pi / N  # Total area of the sphere divided by N

    # Calculate the number of pixels needed for each area
    pixels_per_area = target_area / pixel_area

    # Round up to the nearest integer
    pixels_per_area = np.ceil(pixels_per_area)

    # Ensure that the total number of pixels is not more than the total number of pixels in the sphere
    pixels = min(pixels_per_area * N, npix)

    # Get the pixel indices of the selected pixels
    pixels_indices = np.arange(int(pixels))

    # Get the sky coordinates of the pixel centers
    lon, lat = hp.pix2ang(nside, pixels_indices, nest=False, lonlat=True)

    print(f"npix = {npix}")
    print(f"pixel_area = {pixel_area} sq. deg.")
    print(f"target_area = {target_area} sq. deg.")
    print(f"pixels_per_area = {pixels_per_area}")

    return lon, lat

# run the main code:

if len(sys.argv) == 1:
    nside = 4
else:
    nside = int(sys.argv[1])
main(nside)
