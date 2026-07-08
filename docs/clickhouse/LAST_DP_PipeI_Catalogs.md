# LAST Data Products / pipeline I / Catalogs

The image catalog content and column descriptions.

## Catalogs

Each LAST image is associated with a source catalog. There are several types of image catalogs:

1. Individual / Epoch image catalogs
2. Visit image catalogs
3. Reference image catalogs
4. Subtraction image catalog

This page is relevant to (1), (2), and (3). For the subtraction image catalog see:
Each line in the catalog corresponds to a source.
Sources with `SN_2-SN_1<0` (e.g., hot pixels, cosmic rays) were removed from the catalog.

## File names

This content is available either as a FITS table associated with the image (including headers: see ),
or in the LAST products database.

## Column names

In each catalog, the following column names are available.

| Column | Units | Description |
| ------ | ----- | ----------- |
| `XPEAK` | pix | Integer-pixel X position of the source in the cropped image, measured from the local maximum in the matched-filter image. |
| `YPEAK` | pix | Integer-pixel Y position of the source in the cropped image, measured from the local maximum in the matched-filter image. |
| `X1` | pix | First-moment X position of the source. |
| `Y1` | pix | First-moment Y position of the source. |
| `X2` | pix$^2$ | Weighted second central moment, $\langle X^2 \rangle$, of the source. |
| `Y2` | pix$^2$ | Weighted second central moment, $\langle Y^2 \rangle$, of the source. |
| `XY` | pix$^2$ | Weighted second central cross moment, $\langle XY \rangle$, of the source. |
| `SN_1` | — | Signal-to-noise ratio assuming the source is a delta function. |
| `SN_2` | — | Signal-to-noise ratio assuming the source is an unresolved PSF, as measured in the image. |
| `SN_3` | — | Signal-to-noise ratio assuming the source has a slightly extended PSF, modeled with an exponential profile. |
| `BACK_IM` | $e^{-}$ | Background at the source position, measured from the background image. |
| `VAR_IM` | $(e^-)^2$ | Variance at the source position, measured from the variance image. |
| `BACK_ANNULUS` | $e^{-}$ | Local background around the source, estimated as the median pixel value in an annulus around the source. |
| `STD_ANNULUS` | $e^{-}$ | Standard deviation of the local background, measured from the pixels in an annulus around the source. |
| `FLUX_APER_1` | $e^{-}$ | Annulus-background-subtracted aperture flux of the source, measured in aperture 1, with radius 2 pix. |
| `FLUX_APER_2` | $e^{-}$ | Annulus-background-subtracted aperture flux of the source, measured in aperture 2, with radius 4 pix. |
| `FLUX_APER_3` | $e^{-}$ | Annulus-background-subtracted aperture flux of the source, measured in aperture 3, with radius 6 pix. |
| `FLUXERR_APER_1` | — | Relative error in the flux corresponding to `FLUX_APER_1`. Measured from $\sqrt{F + S^2}/F$, where $F$ is the source flux and $S$ is `STD_ANNULUS`. |
| `FLUXERR_APER_2` | — | Relative error in the flux corresponding to `FLUX_APER_2`. Measured from $\sqrt{F + S^2}/F$, where $F$ is the source flux and $S$ is `STD_ANNULUS`. |
| `FLUXERR_APER_3` | — | Relative error in the flux corresponding to `FLUX_APER_3`. Measured from $\sqrt{F + S^2}/F$, where $F$ is the source flux and $S$ is `STD_ANNULUS`. |
| `MAG_APER_1` | mag | Calibrated magnitude in aperture 1, with radius 2 pix. |
| `MAG_APER_2` | mag | Calibrated magnitude in aperture 2, with radius 4 pix. |
| `MAG_APER_3` | mag | Calibrated magnitude in aperture 3, with radius 6 pix. The calibration is likely derived using the transmission-fitting method and includes aperture correction. |
| `MAGERR_APER_1` | mag | Photometric uncertainty of `MAG_APER_1`, including source Poisson noise and background noise, but excluding calibration uncertainty. |
| `MAGERR_APER_2` | mag | Photometric uncertainty of `MAG_APER_2`, including source Poisson noise and background noise, but excluding calibration uncertainty. |
| `MAGERR_APER_3` | mag | Photometric uncertainty of `MAG_APER_3`, including source Poisson noise and background noise, but excluding calibration uncertainty. |
| `FLUX_XYPEAK` | $e^{-}$ | Flux-like matched-filter or detection statistic measured at the integer-pixel peak position `XPEAK`,`YPEAK`. Exact definition should be verified from the pipeline code. |
| `FLAGS` | — | 32-bit mask propagated from the mask image (all pixels within 3 pix radius from the source position). See bitmask description in: Image bit mask page. |
| `X` | pix | PSF-fit X position of the source. |
| `Y` | pix | PSF-fit Y position of the source. |
| `XFULL` | pix | PSF-fit X position of the source as measured in the full image (after overscan removal, as calculated using `ORIGSEC` header keyword). |
| `YFULL` | pix | PSF-fit Y position of the source as measured in the full image (after overscan removal, as calculated using `ORIGSEC` header keyword). |
| `FLUX_PSF` | $e^{-}$ | PSF-fit flux of the source. |
| `MAG_PSF` | mag | Calibrated magnitude corresponding to the PSF-fit flux. |
| `MAGERR_PSF` | mag | Photometric uncertainty of `MAG_PSF`, including PSF-fit flux uncertainty but excluding calibration uncertainty unless otherwise specified. |
| `PSF_CHI2DOF` | — | Reduced $\chi^2$ of the PSF fit, $\chi^2/\mathrm{dof}$. |
| `SN` | — | Signal-to-noise ratio of the PSF fit. |
| `MITER` | — | Number of the PSF-fitting iteration in which the source was found. |
| `RA` | deg | J2000.0 right ascension corresponding to the PSF-fit `X`,`Y` position of the source. |
| `Dec` | deg | J2000.0 declination corresponding to the PSF-fit `X`,`Y` position of the source. |
| `MergedCatMask` | — | A bit mask indicating possible association of the source with external catalogs. Different bits correspond to different catalogs (See: MergedCat bit mask table). |
| `DistMP` | arcsec | Distance of the source from the nearest known minor planet (NaN if no minor planet within 10''). |
| `AIRMASS` | — | Hardie airmass of the source. |
| `UPIX_PAR` | — | Healpix unique-identifier of source (partition: NSide=$2^3$). |
| `UPIX_LOW` | — | Healpix unique-identifier of source (low: NSide=$2^8$). |
| `UPIX_HIG` | — | Healpix unique-identifier of source (high: NSide=$2^{16}$). |
| `AB_ZP` | mag | AB-magnitude photometric zero point used to convert instrumental fluxes into AB magnitudes. |
| `FORCED` | logical | A flag indicating if the source is a forced photometry source (true) or not (false). |

## Comments

### Forced photometry

The forced photometry is done (only on the epoch images) on a list of pre-defined positions.
By default, this list contains all the known white dwarfs (GAIA), quasars, CV, and about 200,000 transients from the TNS.
In order to avoid astrometric drifts due to source non-detection, the PSF fitting is limited to two positional iteartions, and can move up to 0.2 pix from the initial astrometric position.
The initial astrometric position includes proper motion.
Forced photometry measurments are indicated by true in the `FORCED` column.

### Artifacts

The source catalog may include artifacts of many kinds. Here are some ways to check the source quality.

### Cosmic rays and bad pixels

Cosmic rays and bad pixels are removed from the source catalog. However, their positions is marked in the Mask image as `CR_DeltaHT`.

#### Bogus sources near bright sources

In some cases, bogus sources are found near bright sources. This is due to the fact that the PSF wings are not measured well and the PSF is tapered by a cosine bell. A simple way to detect such cases is by:

`STD_ANNULUS` ^2 / `VAR_IM` ^2 > 10^6
