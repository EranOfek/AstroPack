;#autogen:ignore

# Overview

The AstroCatalog class is a container for storing and manipulating astronomical catalogs (i.e., tables with two dimensional coordinates like X, Y or RA, Dec). This class inherits from AstroTable.
The class includes the basic functionality. 
Additional help is available using:
help AstroCatalog
help AstroCatalog.AstroCatalog
% to see this file
open +manuals.AstroCatalog
AstroCatalog.help
while a list of properties and methods is available by typing AstroCatalog followed by <tab>, or using
methods(AstroCatalog)


Class properties
This class inherits from AstroTable that inherits from Component that inherits from Base.
Additional properties for this class are:
ColX  - X/RA/Lon column index or column name.
ColY - Y/Dec/Lat column index or column name.
CooType - Indicating the main coordinate type 'pix' | 'sphere'.
CooUnits - Indicating the spherical coordinate units - 'deg'|'rad'|'pix'.
In addition, the following are inherited from AstroTable:
Catalog - Containing the table data. This can be eithre a matrix, or a table.
ColNames - A cell array of column names.
ColUnits - A cell array of column units.
ColDesc - A cell array of column descriptions.
SortByCol - An array of column indices, or a cell array of column names by which the catalog is sorted.
IsSorted - A logical indicating if the catalog is sorted.
In addition the following hidden constant properties are available in Astrocatalog:
DefNamesX = {'X','X_IMAGE','XWIN_IMAGE','X1','X_PEAK','XPEAK'};
DefNamesY = {'Y','Y_IMAGE','YWIN_IMAGE','Y1','Y_PEAK','YPEAK'};
DefNamesRA = {'RA','ALPHA','ALPHAWIN_J2000','ALPHA_J2000','RA_J2000','RAJ2000','RightAsc'};
DefNamesDec = {'Dec','DELTA','DELTAWIN_J2000','DELTA_J2000','DEC_J2000','DEJ2000','Declination'};
DefNamesPMRA     = {'PMRA'};
DefNamesPMDec     = {'PMDec'};
 DefNamesRV         = {'RV'};
 DefNamesPlx       = {'Plx'};
DefNamesMag     = {'Mag','PSF_MAG','MAG_PSF','Mag_BP','Mag_G','Mag_RP'};
Constructor
The AstroCatalog constructor can be used to generate empty objects, or load tables into an AstroTable object. A few examples:
% create a single element AstroTable with an empty Catalog.
AC = AstroCatalog
% Create a 2x2 AstroTable with an empty Catalog
AC = AstroCatalog([2 2])
% Created a 1x2 AstroTable. In each catalog store a rand(10,2) matrix
AC = AstroCatalog({rand(10,2),rand(10,2)})
% THe same as the previous line + set the Column names to 'a' and 'b'
AC = AstroCatalog({rand(10,2),rand(10,2)},'ColNames',{'X','Y'});
% store tables
AC=AstroCatalog({array2table(rand(10,2))});
% Create an AstCat object (an old version of AstroTable)
A = AstCat;
A(1).Cat=rand(10,2);
A(2).Cat=rand(10,2); A(1).ColCell={'RA','Dec'};
A(1).ColUnits={'rad','rad'};
% read the AstCat object and convert it to AstroTable
AC = AstroCatalog(A);
% read from a FITS table
AC=AstroCatalog('asu.fit','HDU',2);

Setters and getters
The setters and getters will set the IsSorted to false when a Catalog is changed. Furthermore, when the ColNames is modified, if the Catalog property contains a table, it will updated accordingly.
In Addition getter for CooUnits - if empty, attempt to obtain from catalog. Note this is not cleared when the catalog is changed.
Static methods
see AstroTable.
unitTest - unitTest for the class
Examples
% test the class
AstroTable.unitTest
Methods
For many more methods see AstroTable. Note that AstroTable contains most of the useful functions for this class.
General methods
getCooTypeAuto - Attempt to get CooType and RA/Dec X/Y column names automatically from Catalog.
convertCooUnits - Convert all coordinates Units for multiple element object
Examples

% populate the CooType and CooUnits properties:
[CooType, NameX, NameY, IndInCellX, IndInCellY] = getCooTypeAuto(AC(1));
% convert the units in the coordinate columns from 'rad' to 'deg'
AC=AstroCatalog({'asu.fit','asu.fit'},'HDU',2)
AC.convertCooUnits('deg')
Obj.convertCooUnits('deg')


Methods for retrieving coordinate columns

getCoo - Get coordinates columns from a single element AstroCatalog object.
getLonLat - Get Lon/Lat columns from AstroCatalog.
getRADecPM - Get RA/Dec/PM/Plx/RV from astrometric catalog.
getXY - Get X/Y columns from AstroCatalog.
Examples
% Get RA/Dec columns
AC=AstroCatalog({'asu.fit'},'HDU',2);
% get RA, Dec, or X, Y (according to CooType) in deg in seperate variables
[RA, Dec] = AC.getCoo('deg');
% get RA, Dec in rad in a single two column array
[RADec]   = AC.getCoo('rad');

AC=AstroCatalog({'asu.fit'},'HDU',2);
% get RA, Dec
[Lon,Lat] = getLonLat(AC);
[Lon,Lat] = getLonLat(AC,'rad');

% get proper motion information:
C=catsHTM.cone_search('GAIADR2',1,1,100,'OutType','astrocatalog');
[RA, Dec, PM_RA, PM_Dec, Plx, RV] = getRADecPM(C)

% get X, Y coordinates
AC=AstroCatalog({rand(100,2)},'ColNames',{'XWIN_IMAGE','YWIN_IMAGE'});
[X,Y] = getXY(AC);


Special coordinates related functions

boundingCircle - Fit a bounding circle position and radius to a catalog
sphere_dist - Calculate the spherical distance and PA between Lon,Lat in Astrocatalog and a Lon, Lat in array.
cropXY - crop AstroCatalog object by X/Y coordinates.
Examples

% crop AstroCatalog by X,Y coordinates
AC = AstroCatalog({rand(100,3).*100}, 'ColNames',{'XWIN','YWIN','Flux'});
Result = cropXY(AC, [1 50 1 50])
Result = cropXY(AC, [81 100 41 70],'AddX',{'Flux'})
Result = cropXY(AC, [81 100 41 70; 1 50 1 50]); % multiple crops of a single catalog

% Calculate spherical distance between Lon,Lat and celestial coordinates in
% AstroCatalog
AC=AstroCatalog({'asu.fit'},'HDU',2);
[Dist, PA] = sphere_dist(AC,1,1);


