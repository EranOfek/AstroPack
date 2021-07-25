import healpy as hp
import numpy as np
from configparser import ConfigParser
from astropy.table import Table


def get_coo_healpix_probability(ra, dec, skymap_path):
    """
    Returns the healpix probability of (RA, Dec).

    :param ra: RA in deg
    :param dec: Dec in deg
    :param skymap_path: path to skymap FITS file
    :return: healpix probability
    """
    # Read the HEALPix sky map:
    try:
        prob, dist_mu, dist_sigma, dist_norm = hp.read_map(skymap_path, field=None, verbose=False)
    except Exception:
        print('Failed to read sky map!')

    # Skymap parameters:
    npix = len(prob)
    nside = hp.npix2nside(npix)

    # Convert galaxy WCS (RA, DEC) to spherical coordinates (theta, phi):
    theta = 0.5 * np.pi - np.deg2rad(dec)
    phi = np.deg2rad(ra)

    # Convert galaxy coordinates to skymap pixels:
    galaxy_pix = hp.ang2pix(nside, theta, phi)

    p = prob[galaxy_pix]

    return p


def get_galaxy_healpix_probability(glade_id, skymap_path):
    """
    Returns the healpix probability of a Glade galaxy.

    :param glade_id: GladeID of the galaxy
    :param skymap_path: path to skymap FITS file
    :return: healpix probability
    """
    # Read the HEALPix sky map:
    try:
        prob, dist_mu, dist_sigma, dist_norm = hp.read_map(skymap_path, field=None, verbose=False)
    except Exception:
        print('Failed to read sky map!')

    # settings:
    config = ConfigParser(inline_comment_prefixes=';')
    config.read('config.ini')
    cat_file = config.get('CATALOG', 'PATH') + config.get('CATALOG', 'NAME') + '.npy'  # galaxy catalog file

    # Load the galaxy catalog (glade_id, RA, DEC, distance, Bmag):
    galaxy_cat = np.load(cat_file)
    galaxy_cat = Table(galaxy_cat, names=('ID', 'RA', 'Dec', 'Dist', 'Bmag'))
    # galaxy_cat = galaxy_cat[np.where(galaxy_cat['Dist'] > 0)]  # remove entries with a negative distance
    # galaxy_cat = galaxy_cat[np.where(~np.isnan(galaxy_cat['Bmag']))]  # remove entries with no Bmag

    # Galaxy ID:
    idx = np.where(galaxy_cat["ID"]==glade_id)[0][0]

    # Skymap parameters:
    npix = len(prob)
    nside = hp.npix2nside(npix)

    # Convert galaxy WCS (RA, DEC) to spherical coordinates (theta, phi):
    theta = 0.5 * np.pi - np.deg2rad(galaxy_cat['Dec'][idx])
    phi = np.deg2rad(galaxy_cat['RA'][idx])
    # d = np.array(galaxy_cat['Dist'][idx])

    # Convert galaxy coordinates to skymap pixels:
    galaxy_pix = hp.ang2pix(nside, theta, phi)

    p = prob[galaxy_pix]

    return p


def get_sky_area(skymap_path, credzone=0.5):
    """
    Returns the credzone sky area in degrees
    :param skymap_path: path to skymap file
    :param credzone: localization probability to consider credible, could also be a list
    :return: credzone area in deg^2
    """

    # Read the HEALPix sky map:
    try:
        prob, dist_mu, dist_sigma, dist_norm = hp.read_map(skymap_path, field=None, verbose=False)
    except Exception:
        print('Failed to read sky map!')

    # Skymap parameters:
    npix = len(prob)
    nside = hp.npix2nside(npix)

    sort_idx = np.flipud(np.argsort(prob, kind="stable"))
    sorted_credible_levels = np.cumsum(prob[sort_idx])
    credible_levels = np.empty_like(sorted_credible_levels)
    credible_levels[sort_idx] = sorted_credible_levels

    if np.isscalar(credzone):
        area = np.sum(credible_levels <= credzone) * hp.nside2pixarea(nside, degrees=True)
    else:
        area = [None]*len(credzone)
        for i in range(len(credzone)):
            area[i] = np.sum(credible_levels <= credzone[i]) * hp.nside2pixarea(nside, degrees=True)

    return area
