# LAST Data Products / pipeline I / Headers

This page describes the FITS image headers of LAST data products.

## Header keywords

Here we provide descriptions of special header keywords.

`Group` corresponds to the general topic or theme that keywords belong to, and `Tables` indicate in which tables the keyword is present (R-Raw; E-Epoch, V-Visit; ...).

| Group | Tables | Key | Units | Description |
| ----- | ------ | --- | ----- | ----------- |
| FITS structure | REV | `SIMPLE` | bool | Indicates that the file conforms to the FITS standard. |
| FITS structure | REV | `BITPIX` | bit | Number of bits per data pixel. Negative values indicate floating-point data. |
| FITS structure | REV | `NAXIS` |  | Number of data axes in the image. |
| FITS structure | REV | `NAXIS1` | pixel | Length of image axis 1. |
| FITS structure | REV | `NAXIS2` | pixel | Length of image axis 2. |
| FITS structure | REV | `EXTEND` | bool | Indicates that the FITS file may contain extensions. |
| FITS structure | REV | `LONGSTRN` | string | Long-string convention used for FITS header values continued over multiple cards. |
| FITS checksum | EV | `CHECKSUM` | string | FITS header/data checksum keyword. |
| FITS checksum | EV | `DATASUM` | string | FITS data checksum keyword. |
| File info | REV | `FILENAME` | string | Name of the FITS file. |
| File info | REV | `CRDATE` | UTC | UTC date and time when the file was created. |
| Project info | REV | `PROJNAME` | string | LAST project/system name. |
| Project info | REV | `FULLPROJ` | string | Full LAST project/node/mount/camera identifier (e.g., 'LAST.01.08.03'). |
| Project info | REV | `NODENUMB` |  | LAST node number. |
| Project info | REV | `TIMEZONE` | h | Observatory/local time-zone offset. |
| Project info | REV | `MOUNTNUM` |  | Mount number. |
| Software | REV | `GITASTRP` | string | Git version or branch identifier of the astronomy processing/control software. |
| Software | REV | `GIT_CAMR` | string | Git version or branch identifier of the camera-control software. |
| Software | REV | `GITFOCUS` | string | Git version or branch identifier of the focus-control software. |
| Software | EV | `PIPEVER` | string | Pipeline version or Git identifier. |
| Observatory | REV | `OBSLON` | deg | Longitude of the observatory. |
| Observatory | REV | `OBSLAT` | deg | Latitude of the observatory. |
| Observatory | REV | `OBSALT` | m | Height of the observatory above sea level. |
| Environment | REV | `MNTTEMP` | deg C | Temperature measured by the mount sensors. |
| Time | REV | `JD_START` | JD | Julian day of the command starting the series of exposures (i.e., in a visit this corresponds to a time before the start of the first saved image). |
| Time | EV | `JD` | JD | Julian day at exposure start. |
| Time | EV | `MIDJD` | JD | Julian day of mid-exposure, or weighted mean observation time for coadds. |
| Time | V | `MINJD` | JD | Mid-exposure Julian day of the first coadded observation. |
| Time | V | `MAXJD` | JD | Mid-exposure Julian day of the last coadded observation. |
| Time | REV | `DATE-OBS` | UTC | UTC date and time at exposure start. |
| Time | EV | `TIME` | string | Exposure time string used in file and directory naming. |
| Time | REV | `LST` | deg | Local sidereal time at exposure start. |
| Pointing | REV | `RA` | deg | J2000 right ascension for image center. For raw image, this is the best guess including telescope offset, for processed data this is the solved RA, Dec of the image center. |
| Pointing | REV | `DEC` | deg | J2000 declination (like RA). |
| Pointing | REV | `EQUINOX` | yr | Equinox of the coordinate system. |
| Pointing | REV | `M_RA` | deg | Mount physical/apparent pointing right ascension. |
| Pointing | REV | `M_HA` | deg | Mount physical/apparent pointing hour angle. |
| Pointing | REV | `M_DEC` | deg | Mount physical/apparent pointing declination. |
| Pointing | REV | `M_AZ` | deg | Mount physical/apparent pointing azimuth. |
| Pointing | REV | `M_ALT` | deg | Mount physical/apparent pointing altitude. |
| Pointing | REV | `M_JRA` | deg | Mount center J2000 right ascension |
| Pointing | REV | `M_JDEC` | deg | Mount center J2000 declination. |
| Pointing | REV | `M_ARA` | deg | Mount apparent equinox-of-date right ascension. |
| Pointing | REV | `M_AHA` | deg | Mount apparent equinox-of-date hour angle. |
| Pointing | REV | `M_ADEC` | deg | Mount apparent equinox-of-date declination. |
| Pointing | REV | `M_ADRA` | deg | Mount apparent right ascension including distortion correction (pointing model). |
| Pointing | REV | `M_ADHA` | deg | Mount apparent hour angle including distortion correction (pointing model). |
| Pointing | REV | `M_ADDEC` | deg | Mount apparent declination including distortion correction (pointing model). |
| Pointing | REV | `AIRMASS` |  | Hardie airmass at the time of observation. |
| Tracking | REV | `TRK_RA` | arcsec s$^{-1}$ | Tracking speed in right ascension. |
| Tracking | REV | `TRK_DEC` | arcsec s$^{-1}$ | Tracking speed in declination. |
| Focus | REV | `FOCUS` | step | Current focuser position. |
| Focus | REV | `PRVFOCUS` | step | Previous focuser position. |
| Image info | REV | `IMTYPE` | string | Image type, e.g., science image. |
| Image info | REV | `OBJECT` | string | Target or object name. |
| Image info | REV | `FILTER` | string | Filter used for the exposure. |
| Image info | REV | `COUNTER` |  | Exposure or frame counter. |
| Image info | REV | `EXPTIME` | s | Exposure time. |
| Image info | V | `MEXPTIME` | s | Mean exposure time of images from which the coadd image was composed. |
| Image scaling | REV | `BZERO` |  | FITS data offset used for scaling stored pixel values. |
| Image scaling | REV | `BSCALE` |  | FITS data scale factor applied to stored pixel values. |
| Camera | REV | `EXPMODE` | string | Camera exposure mode. |
| Camera | REV | `GAIN` | $e^{-}$/ADU | Effective detector gain. It is typically 1, because the image is multiplied by the `ORIGGAIN`. However, the visit coadd, and reference coadd, are based on mean-coaddition. Therefore, altough their GAIN is set to 1, their actual gain is one over `NCOADD`. |
| Calibration | EV | `ORIGGAIN` | $e^{-}$/ADU | Original detector gain used before or during calibration. Exact units should be verified. |
| Camera | REV | `READNOI` | $e^{-}$ | Detector read noise. Exact units should be verified from the camera configuration. |
| Camera | REV | `DARKCUR` | $ADU$ $s^{-1}$ pix$^{-1}$ | Detector dark current. Exact units should be verified from the camera configuration. |
| Camera | REV | `SATURVAL` | ADU | Saturation value. |
| Camera | REV | `NONLIN` | ADU | Approximate non-linearity threshold. |
| Camera | REV | `BINX` | pixel | Detector binning factor along the X axis. |
| Camera | REV | `BINY` | pixel | Detector binning factor along the Y axis. |
| Camera | REV | `CAMNUM` |  | Camera number. |
| Camera | REV | `CAMNAME` | string | Camera name or camera identifier. |
| Camera | REV | `CAMTEMP` | deg C | Camera sensor temperature. |
| Camera | REV | `CAMCOOL` | percent | Camera cooling power. |
| Camera | REV | `CAMMODE` |  | Camera readout mode. |
| Camera | REV | `CAMGAIN` |  | Camera gain setting. |
| Camera | REV | `CAMOFFS` | ADU | Camera offset level. |
| Pipeline | EV | `LEVEL` | string | Data product processing level. |
| Pipeline | EV | `SUBLEVEL` | string | Processing sub-level of the data product. |
| Pipeline | EV | `VERSION` | string | Data product version. |
| Identification | EV | `ID_RAW` | string | Unique identifier of the raw image. Note that this doesn't included in the raw image header. |
| Identification | EV | `ID_DARK` | string | Unique identifier of the dark calibration image. |
| Identification | EV | `ID_FLAT` | string | Unique identifier of the flat-field calibration image. |
| Identification | EV | `ID_PROC` | string | Unique identifier of the processed image (for visit coadd, this is available but meaningless). |
| Identification | V | `ID_COADD` | string | Unique identifier of the coadded image. |
| Identification | V | `ID_PROCF` | string | ID of the first processed image in the visit coadd. |
| Identification | V | `ID_PROCL` | string | ID of the last processed image in the visit coadd. |
| Identification | EV | `CCDID` |  | CCD identifier. |
| Identification | EV | `SUBDIR` | string | Subdirectory name associated with the visit. |
| Identification | EV | `FIELDID` | string | Field or target identifier. |
| Identification | EV | `DIRDAY` | day | Day component of the data directory date. |
| Identification | EV | `DIRMON` | month | Month component of the data directory date. |
| Identification | EV | `DIRYEAR` | yr | Year component of the data directory date. |
| Image region | EV | `CROPID` |  | Crop identifier. |
| Image region | EV | `LIGHTSEC` | pixel | Section of the original image used as the light-sensitive image region. |
| Image region | EV | `OVERSCAN` | pixel | Section of the original image corresponding to the overscan region. |
| Image region | EV | `CCDSEC` | pixel | Pixel section of the CCD represented in this image. |
| Image region | EV | `ORIGSEC` | pixel | Pixel section in the original full-frame image corresponding to this cropped image. |
| Image region | EV | `ORIGUSEC` | pixel | Pixel section in the original full-frame image corresponding to the unique (i.e., no overlap) region of this cropped image. |
| Image region | EV | `UNIQSEC` | pixel | Pixel section of the unique (non overlapping) part of this cropped image (corresponding to `ORIGUSEC`). |
| Calibration | EV | `BIAS_IM` | string | Bias or dark calibration image used in processing. |
| Calibration | EV | `FLAT_IM` | string | Flat-field calibration image used in processing. |
| Background | EV | `MEANBCK` | $e^{-}$ | Mean background level. |
| Background | EV | `MEDBCK` | $e^{-}$ | Median background level. |
| Background | EV | `STDBCK` | $e^{-}$ | Standard deviation of the background level. |
| Background | EV | `MINBCK` | $e^{-}$ | Minimum estimated background level. |
| Background | EV | `MAXBCK` | $e^{-}$ | Maximum estimated background level. |
| Variance | EV | `MEANVAR` | $e^{-}$$^2$ | Mean variance estimate. |
| Variance | EV | `MEDVAR` | $e^{-}$$^2$ | Median variance estimate. |
| Sources | EV | `N_STARS` |  | Number of stars found and measured in the image. |
| Matched | EV | `M_CHI2D` |  | Median of the best-fit chi-square of all sources in the image. |
| Matched | EV | `RP_MRMS` | mag | Minimum (asymptotic) RMS of relative photometry over all epochs in visit. |
| Matched | EV | `RP_MMRMS` | mag | Magnitude of the minimum (asymptotic) RMS of relative photometry over all epochs in visit. |
| Astrometry | EV | `AST_NSRC` |  | Number of astrometric sources. |
| Astrometry | EV | `AST_ARMS` | arcsec | Astrometric asymptotic RMS. |
| Astrometry | EV | `AST_ERRM` | arcsec | Astrometric error on the mean. |
| WCS | EV | `CTYPE1` | string | WCS projection type for image axis 1. |
| WCS | EV | `CTYPE2` | string | WCS projection type for image axis 2. |
| WCS | EV | `WCSAXES` |  | Number of WCS axes. |
| WCS | EV | `RADESYS` | string | Astrometric reference system. |
| WCS | EV | `LONPOLE` | deg | Native longitude of the celestial pole. |
| WCS | EV | `LATPOLE` | deg | Native latitude of the celestial pole. |
| WCS | EV | `CUNIT1` | string | World-coordinate unit for axis 1. |
| WCS | EV | `CUNIT2` | string | World-coordinate unit for axis 2. |
| WCS | EV | `CRPIX1` | pixel | Reference pixel coordinate on image axis 1. |
| WCS | EV | `CRPIX2` | pixel | Reference pixel coordinate on image axis 2. |
| WCS | EV | `CRVAL1` | deg | World coordinate at the reference pixel for axis 1. |
| WCS | EV | `CRVAL2` | deg | World coordinate at the reference pixel for axis 2. |
| WCS | EV | `CD%d_%d` | deg pix$^{-1}$ | Linear WCS transformation matrix element. |
| WCS | EV | `PIXSCALE` | arcsec pix$^{-1}$ | Pixel scale. |
| WCS | EV | `ROTAT` | deg | Image rotation angle. |
| WCS distortion | EV | `PV%d_%d` |  | TPV astrometric distortion coefficient. The first index is the WCS axis and the second index is the TPV polynomial coefficient index. |
| Image corners | EV | `RA%d` | deg | Right ascension of image corner `%d`. |
| Image corners | EV | `DEC%d` | deg | Declination of image corner `%d`. |
| Image corners | EV | `RAU%d` | deg | Right ascension of unique image-region corner `%d`. |
| Image corners | EV | `DECU%d` | deg | Declination of unique image-region corner `%d`. |
| PSF info | EV | `FWHE` | arcsec | FWHE measured by the radial cumulative sum of the PSF flux crossing half the total flux, i.e., effective width of half the energy. For a Gaussian PSF this parameter is biased downward. |
| PSF info | EV | `FWHM` | arcsec | FWHM measured by the radial mean flux crossing half the peak flux, i.e., effective width of half the height. For a Gaussian PSF this parameter is biased upward. |
| PSF info | EV | `PSF_FITN` |  | Normalization of Gaussian fit, where the PSF is normalized to unity. |
| PSF info | EV | `PSF_FITA` | arcsec | $\sigma$-width of the semi-major axis of the best-fit Gaussian. |
| PSF info | EV | `PSF_FITB` | arcsec | $\sigma$-width of the semi-minor axis of the best-fit Gaussian. |
| PSF info | EV | `PSF_FITT` | deg | Rotation angle of the best-fit 2D Gaussian measured counterclockwise from the X-axis. |
| PSF info | EV | `PSF_NST` |  | Number of stars used for PSF estimation. |
| PSF info | EV | `PSF_NPK` |  | Number of detected peaks in the PSF. |
| PSF info | EV | `PSF_PKR` |  | Ratio between the highest peak in the PSF and the second highest peak. If `PSF_NPK` is 1, then this is NaN. |
| PSF info | EV | `PSF_DPK` | pixel | Distance between the two highest peaks in the PSF. If `PSF_NPK` is 1, then this is NaN. |
| PSF info | EV | `PSF_ERR` |  | Relative error in the weighted integrated PSF (i.e., $\sqrt(\sum(PSF*VariancePSF)/\sum(PSF))$ ). |
| PSF info | EV | `PSF_S2` |  | Sum of PSF squared |
| PSF info | EV | `PSF_AF_%d` |  | Fraction of PSF light within aperture. Measured in aperture radii of 3, 5, 6, 7 pix. |
| Pixels | EV | `UPIX_PAR` |  | Healpix unique identifier (partition: NSide=$2^3$). |
| Pixels | EV | `UPIX_LOW` |  | Healpix unique identifier (low: NSide=$2^8$). |
| Pixels | EV | `UPIX_HIG` |  | Healpix unique identifier (high: NSide=$2^{16}$). |
| Shape diagnostics | EV | `MED_X2` | pix$^2$ | Median second moment, over all stars, in the X direction. |
| Shape diagnostics | EV | `MED_Y2` | pix$^2$ | Median second moment, over all stars, in the Y direction. |
| Shape diagnostics | EV | `MED_XY` | pix$^2$ | Median cross second moment, over all stars. |
| Photometry | EV | `MAG_95Q` | mag | 95th-percentile magnitude of selected sources. |
| Legacy Photometry | EV | `PH_ZP` | mag | Photometric zero point, based on legacy fitting method. |
| Legacy Photometry | EV | `PH_COL1` | mag | First-order photometric color term (legacy) |
| Legacy Photometry | EV | `PH_MEDC` | mag | Median color of sources used for photometric calibration. |
| Legacy Photometry | EV | `PH_RMS` | mag | RMS scatter of the photometric calibration residuals. |
| Legacy Photometry | EV | `PH_NSRC` |  | Number of sources used for photometric calibration. |
| Legacy Photometry | EV | `PH_MAGSY` | string | Magnitude system used for photometric calibration. |
| Photometry | EV | `LIMMAG` | mag | Limiting magnitude. |
| Photometry | EV | `BACKMAG` | mag arcsec$^{-2}$ | Background surface brightness. |
| Photometry | EV | `PH_MAGT` | string | Magnitude type (e.g., `MAG_APER_3`) used for photometric calibration. |
| Photometry | EV | `PH_MAGTE` | string | Magnitude-error column or type used for photometric calibration. |
| Aperture correction | EV | `APCOR_A%d` | mag | Aperture-correction coefficient or fitted parameter `%d`. |
| Aperture correction | EV | `APCOR_PS` | mag | Aperture correction associated with the PSF magnitude or PSF-to-aperture offset. |
| Aperture correction | EV | `APCOR_N` |  | Number of sources used to estimate the aperture correction. |
| Coaddition | EV | `NCOADD` |  | Number of coadded images. |
| Coaddition | EV | `COADDOP` | string | Coaddition method. |
| Coaddition | EV | `AVNCOADD` |  | Mean number of last-step coadded images per pixel. |
| Coaddition | EV | `MINCOADD` |  | Minimum number of last-step coadded images per pixel. |
| Moving object | EV | `GM_RATEX` | pix s$^{-1}$ | Global fitted motion rate along the X/image-axis direction. Exact units should be verified. |
| Moving object | EV | `GM_STDX` | pix | Standard deviation or uncertainty of the fitted motion in the X/image-axis direction. Exact units should be verified. |
| Moving object | EV | `GM_RATEY` | pix s$^{-1}$ | Global fitted motion rate along the Y/image-axis direction. Exact units should be verified. |
| Moving object | EV | `GM_STDY` | pix | Standard deviation or uncertainty of the fitted motion in the Y/image-axis direction. Exact units should be verified. |
| Photometric transmission | EV | `PT_RMS` | mag | RMS residual of the photometric-transmission fit. |
| Photometric transmission | EV | `PT_ARMS` | mag | Asymptotic RMS of the photometric-transmission fit. |
| Photometric transmission | EV | `PT_CHI2` |  | Chi-square statistic of the photometric-transmission fit. |
| Photometric transmission | EV | `PT_DOF` |  | Degrees of freedom of the photometric-transmission fit. |
| Photometric transmission | EV | `PT_NCALI` |  | Number of calibrating sources used in the photometric-transmission fit. |
| Photometric transmission | EV | `PT_SUCC` | bool | Indicates whether the photometric-transmission fit succeeded. |
| Photometric transmission | EV | `PT_AREF` | string | Reference atmosphere or atmospheric model used by the photometric-transmission fit. |
| Photometric transmission | EV | `PT_SPEC` | string | Reference spectral/catalogue source used by the photometric-transmission fit. |
| Photometric transmission | EV | `PT_%d_N` | string | Name of photometric-transmission model component `%d`. |
| Photometric transmission | EV | `PT_%d_V%d` |  | Value of parameter `%d` for photometric-transmission model component `%d`. |
| Photometric transmission | EV | `PT_%d_F%d` | bool/int | Flag indicating if parameter `%d` of photometric-transmission model component `%d` was held fixed (false) or fitted (true). |
