# AstroPack (a.k.a. MAATv2)
Astronomy, Astrophysics, &amp; Image Processing Software Pacakge

Main Developers: <a href="https://www.weizmann.ac.il/physics/ofek/home">Eran Ofek</a>, Chen Tishler, Yossi Shvartzvald, Amir Sharon, Dan Elhanati, Noam Segev
(Weizmann Institute of Science)

## Installation:
1. git clone <this package>
2. git checkout dev1
3. From matlab: edit manuals.Install, and follow the instructions.

For detailed installtion instruction, see the <a href="https://github.com/EranOfek/AstroPack/wiki/Install">Install wiki page</a>.
  
## Credit:
Please cite:
  1. <a href="https://ui.adsabs.harvard.edu/abs/2014ascl.soft07005O/abstract">Ofek (2014)</a>;
  2. <a href="https://ui.adsabs.harvard.edu/abs/2018PASP..130g5002S/abstract">Soumagnac and Ofek (2018)</a>; );
  3. <a href="https://ui.adsabs.harvard.edu/abs/2019PASP..131e4504O/abstract">Ofek (2019)</a>;
  4. Ofek et al., in prep.

## Getting started and documntation

See <a href="https://github.com/EranOfek/AstroPack/wiki/Getting-Started">getting started wiki page</a>, or 
from matlab: edit manuals.GettingStarted, and the inspect the content of the manuals package.

Documentation is available in the github <a href="https://github.com/EranOfek/AstroPack/wiki">wiki page</a>, or in matlab, after installtion, in the manuals package.

## Content
A collection of several thousands functions for astronomy, astrophysics, and image processing applications.
See also <a href="https://github.com/EranOfek/AstroPack/wiki/AstroPack-function-list">List of all functions</a>, and <a href="https://github.com/EranOfek/AstroPack/wiki/Getting-Started">getting started</a>.
For detailed help see the <a href="https://github.com/EranOfek/AstroPack/wiki">AstroPack/MAATv2 wiki page</a>
The following topics are covered:

### Astronomical Image Processing

AstroPack/MAATv2 contains tools for image analysis of astronomical images. The tools includes most of the required functions for image analysis from the <a href="https://github.com/EranOfek/AstroPack/wiki/Dark-and-Flat-calibration">basic calibration of images</a>, source finding, <a href="https://github.com/EranOfek/AstroPack/wiki/Photometry">photometry</a>, astrometry, catalog matching, photometric calibration, light curves, image coaddition, reference image management, image subtraction, and database storage.
The main tools includes a data container class for astronomical images (<a href="https://github.com/EranOfek/AstroPack/wiki/AstroImage">AstroImage</a>). It supports arrays of classes, such that each element of the object contains one image with all its metadata (e.g., <a href="https://github.com/EranOfek/AstroPack/wiki/AstroHeader">header</a>, <a href="https://github.com/EranOfek/AstroPack/wiki/AstroCatalog">catalog</a>, <a href="https://github.com/EranOfek/AstroPack/wiki/AstroPSF">PSF</a>, <a href="https://github.com/EranOfek/AstroPack/wiki/AstroWCS">WCS</a>, background image, variance image, and <a href="https://github.com/EranOfek/AstroPack/wiki/MaskImage">mask image</a>.
The classes provides easy to use arithmatic and logical manipulation of images. In addition low-level and high-level tools are available in several associated packages (e.g., <a href="https://github.com/EranOfek/AstroPack/wiki/imProc">imProc</a>).

### Celestial coordinates

A large number of tools for coordinates calculations, conversion, and manipulation is available (e.g., in the <a href="https://github.com/EranOfek/AstroPack/wiki/celestial">celestial</a> package.
  
### Celestial mechanics

The <a href="https://github.com/EranOfek/AstroPack/wiki/celestial">celestial</a> package also contains large number of tools for solving the Kepler equation, generating low precision and high precision epehmeris, querying the JPL horizons website, <a href="https://github.com/EranOfek/AstroPack/wiki/celestial.map">plotting star maps</a>, and more.

### Virtual Observatory excess
### Large catalogs and catsHTM
### Statistics
### Telescopes and Optics
### Spectra and filters
### Time series analysis
### Cosmology
### Gravitational lensing
### General utilities
