# Package: imProc.astrometry


### imProc.astrometry.addCoordinates2catalog

Add or update RA/Dec coordinates in catalogs in AstroImage/Astrocatalog


### imProc.astrometry.assessAstrometricQuality

Collect information regarding quality of astrometric solution and return a sucess flag.


### imProc.astrometry.astrometryCheck

Compare the astrometry of a catalog with a reference astrometric catalog. Return statistics regarding the matched sources, rms, rms as a function of position and mag.


### imProc.astrometry.astrometryCore

A core function for astrometry. Match pattern and fit transformation.


### imProc.astrometry.astrometryImage




### imProc.astrometry.astrometryRefine

Refine an astrometric solution of an AstroCatalog object This function may work on images which have either an approximate WCS (either in AstroHeader or AstroWCS), or a catalog with RA/Dec coordinates. The coordinates should be good to a few arcseconds.


### imProc.astrometry.astrometrySubImages

Solve astrometry for sub images of a single contigious image The solution is done by executing astrometryCore for a limited number of sub images, and astrometryRefine for all the rest, based on the solution from astrometryCore.


### imProc.astrometry.fitWCS

Perform the Tran2D.fitAstrometricTran and prepare the WCS info This is an auxilary function that performs the fitting stage between an astrometric catalog and an image catalog, and return the Tran2D object as well as the information needed for the WCS (e.g.,


### imProc.astrometry.unitTest

unitTest for +imProc.astrometry Example: imProc.astrometry.unitTest


