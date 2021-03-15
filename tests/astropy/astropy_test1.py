# https://docs.astropy.org/en/stable/wcs/wcsapi.html#basic-usage

fname = 'D:/Ultrasat/wcs/Images/ztf_20190709485764_000600_zg_c04_o_q4_sciimg.fits'

fname = './1.fits'

from astropy.wcs import WCS
from astropy.utils.data import get_pkg_data_filename
from astropy.io import fits
filename = get_pkg_data_filename(fname)
hdu = fits.open(filename)[0]
wcs = WCS(hdu.header)
print(wcs)
