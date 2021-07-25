import healpy as hp
import numpy as np
import logging
from astropy import units as u
from astropy.coordinates import Angle
from wisegcn.email_alert import send_mail


def tile_region(skymap_path, credzone=0.9, tile_area=1, log=None):
    if log is None:
        log = logging.getLogger(__name__)

    # Read the HEALPix sky map:
    try:
        prob, dist_mu, dist_sigma, dist_norm = hp.read_map(skymap_path, field=None, verbose=False)
    except Exception as e:
        log.error('Failed to read sky map!')
        send_mail(subject="[GW@Wise] Failed to read LVC sky map",
                  text='''FITS file: {}
                              Exception: {}'''.format(skymap_path, e),
                  log=log)

    sort_idx = np.flipud(np.argsort(prob, kind="stable"))
    sorted_credible_levels = np.cumsum(prob[sort_idx])
    credible_levels = np.empty_like(sorted_credible_levels)
    credible_levels[sort_idx] = sorted_credible_levels

    # pixels inside the credible zone
    good_pix = sort_idx[0:np.sum(credible_levels <= credzone)]
    npix = len(prob)
    nside = hp.npix2nside(npix)
    theta, phi = hp.pix2ang(nside, good_pix)

    sky_area = 4 * 180 ** 2 / np.pi  # [deg^2]
    nside_obs = int(np.ceil(np.sqrt(sky_area / tile_area / 12)))
    obs_pix, unique_idx = np.unique(hp.ang2pix(nside_obs, theta, phi), return_inverse=True)
    probability = np.bincount(unique_idx, weights=prob[good_pix])

    # sort by descending probability
    sort_idx = np.flipud(np.argsort(probability, kind="stable"))
    probability = probability[sort_idx]
    obs_pix = obs_pix[sort_idx]

    theta, phi = hp.pix2ang(nside_obs, obs_pix)
    ra = Angle(np.rad2deg(phi)*u.deg)
    dec = Angle(np.rad2deg(0.5 * np.pi - theta)*u.deg)

    return ra, dec, probability
