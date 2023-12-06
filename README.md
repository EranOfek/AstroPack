# AstroPack (a.k.a. MAATv2)

Astronomy, Astrophysics, &amp; Image Processing Software Pacakge

Main Developers: <a href="https://www.weizmann.ac.il/physics/ofek/home">Eran Ofek</a>, Sasha Krassilchtchikov, Chen Tishler, Yossi Shvartzvald, Amir Sharon, Dan Elhanati, Noam Segev
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

The following topics are covered:
### Astronomical Image Processing
### Celestial coordinates
### Celestial mechanics
### Virtual Observatory excess
### Large catalogs and catsHTM
### Statistics
### Telescopes and Optics
### Spectra and filters
### Time series analysis
### Cosmology
### Gravitational lensing
### General utilities


See also <a href="https://github.com/EranOfek/AstroPack/wiki/AstroPack-function-list">List of all functions</a>, and <a href="https://github.com/EranOfek/AstroPack/wiki/Getting-Started">getting started</a>.
For detailed help see the <a href="https://github.com/EranOfek/AstroPack/wiki">AstroPack/MAATv2 wiki page</a>
The following topics are covered:

### Astronomical Image Processing

AstroPack/MAATv2 contains tools for image analysis of astronomical images. The tools includes most of the required functions for image analysis from the <a href="https://github.com/EranOfek/AstroPack/wiki/Dark-and-Flat-calibration">basic calibration of images</a>, source finding, <a href="https://github.com/EranOfek/AstroPack/wiki/Photometry">photometry</a>, astrometry, catalog matching, photometric calibration, light curves, image coaddition, reference image management, image subtraction, and database storage.
The main tools includes a data container class for astronomical images (<a href="https://github.com/EranOfek/AstroPack/wiki/AstroImage">AstroImage</a>). It supports arrays of classes, such that each element of the object contains one image with all its metadata (e.g., <a href="https://github.com/EranOfek/AstroPack/wiki/AstroHeader">header</a>, <a href="https://github.com/EranOfek/AstroPack/wiki/AstroCatalog">catalog</a>, <a href="https://github.com/EranOfek/AstroPack/wiki/AstroPSF">PSF</a>, <a href="https://github.com/EranOfek/AstroPack/wiki/AstroWCS">WCS</a>, background image, variance image, and <a href="https://github.com/EranOfek/AstroPack/wiki/MaskImage">mask image</a>.
The classes provides easy to use arithmatic and logical manipulation of images. In addition low-level and high-level tools are available in several associated packages (e.g., <a href="https://github.com/EranOfek/AstroPack/wiki/imProc">imProc</a>).

### Celestial coordinates

A large number of tools for coordinates calculations, conversion, and manipulation is available (e.g., in the <a href="https://github.com/EranOfek/AstroPack/wiki/celestial">celestial</a> package).
  
### Celestial mechanics

The <a href="https://github.com/EranOfek/AstroPack/wiki/celestial">celestial</a> package also contains large number of tools for solving the Kepler equation, generating low precision and high precision epehmeris, querying the JPL horizons website, <a href="https://github.com/EranOfek/AstroPack/wiki/celestial.map">plotting star maps</a>, and more.

### Large catalogs and catsHTM

AstroPack/MAATv2 supports astronomical catalog access via three channels:
  1. The <a href="https://github.com/EranOfek/AstroPack/wiki/catsHTM">catsHTM</a> class allows for coordinate-based fast access of big astronomical catalags. Currently, over 20 catalogs are available in the catsHTM format. catsHTM is described in <a href="https://ui.adsabs.harvard.edu/abs/2018PASP..130g5002S/abstract">Soumagnac & Ofek 2018</a>.
  2. The <a href="https://github.com/EranOfek/AstroPack/wiki/cats">cats</a> package that can be use to store and access small catalogs (i.e., up to tens of milions of entries).
  3. Web access of catalogs - e.g., VizieR, SDSS, PS1, GALAEX.
  
### Virtual Observatory access
  
  The <a href="">VO</a> package include tools for accessing several astronomical databases and for formatting catalogs.
  
### Telescopes and Optics

  The <a href="">telescope</a> package includes tools for signal-to-noise calculations and speckle images simulations.
  
### Spectra and filters

Tools to analyze astronomical spectra, database of astronomical spectra, and a database of astronomical telescope filters.

### Time series analysis

Tools for time series analysis including periodicity searches, and time delay estimation (e.g., <a href="https://ui.adsabs.harvard.edu/abs/2021MNRAS.506..864S/abstract">Springer & Ofek 2021a</a>, <a href="https://ui.adsabs.harvard.edu/abs/2021MNRAS.508.3166S/abstract">Springer & Ofek 2021b</a>).
  
### Cosmology

Tools for calculating comsological distances and related properties.

### Gravitational lensing

Basic tools for gravitational lensing.

### Statistics

Tools for statistical analysis.

### General utilities

Tools for I/O, WWW access, array and other classes manipulation, and more.

## Changelog

[Here](https://github.com/EranOfek/AstroPack/wiki/New-functions-and-changes-log)
