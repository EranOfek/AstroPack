# https://docs.astropy.org/en/stable/io/fits/

from astropy.io import fits
fits_image_filename = fits.util.get_testdata_filepath('test0.fits')

hdul = fits.open(fits_image_filename)

hdul = fits.open(fits_image_filename)
print(hdul[0].header['DATE'])


# https://learn.astropy.org/FITS-images.html

from astropy.utils.data import download_file


image_file = download_file('http://data.astropy.org/tutorials/FITS-images/HorseHead.fits', cache=True )

hdu_list = fits.open(image_file)
hdu_list.info()

image_data = hdu_list[0].data

print(type(image_data))
print(image_data.shape)

