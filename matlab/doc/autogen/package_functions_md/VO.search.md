# Package: VO.search


### VO.search.astcat_search

Search an astronomical catalog formatted in HDF5/HTM/zones format Example: VO.search.astcat_search('FIRST',1,1,0.01)


    
    Search an astronomical catalog formatted in HDF5/HTM/zones format  
    Example: VO.search.astcat_search('FIRST',1,1,0.01)  
      
      
### VO.search.cat_cone

Cone search a local or online catalog. Package: VO.search Description: A uniform interface function for catalogs cone search. The catalog can be either a local catalog in the +cats package, or an online (web) catalog for which inteface


    
    Cone search a local or online catalog.  
    Package: VO.search  
    Description: A uniform interface function for catalogs cone search.  
    The catalog can be either a local catalog in the +cats  
    package, or an online (web) catalog for which inteface  
    function exist, or a local catalog in HDF5/HTM format.  
    Input  : - catalog name, or function handle.  
    The following options are available:  
    (1) To access catalogs in the +cats directory use e.g.,  
    @cats.X.ROSAT_faint.  
    (2) To access catalogs which have access program use  
    function handle.  
    (3) To access catalogs in catsHTM format, specify catalog  
    name.  
    - J2000.0 R.A. [radians, [H M S], or sexagesimal string].  
    - J2000.0 Dec. [radians, [sign D M S], or sexagesimal string].  
    - Search radius [arcsec].  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    'RadiusUnits'  - Search radius units. Default is 'arcsec'.  
    'OutType'      - 'mat' | 'astcat'. Default is 'mat'.  
    Output : - The catalog.  
    - Cell array of column names.  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Jan 2018  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [Cat,ColCell] = VO.search.cat_cone(@cats.X.ROSAT_faint,1,1,1)  
    [Cat,ColCell] = VO.search.cat_cone(@cats.X.ROSAT_faint,1,1,1,'OutType','astcat')  
    [Cat,ColCell] = VO.search.cat_cone('UCAC4',1,1,1)  
    [Cat,ColCell] = VO.search.cat_cone('UCAC4',1,1,1,'OutType','astcat')  
    [Cat,ColCell] = VO.search.cat_cone('GAIADR1',1,1,1,'OutType','astcat')  
    Reliable: 2  
      
      
### VO.search.catalog_interface

An interface auxilary to the catalogs in the data directory Package: VO.search Description: An auxilary function to interface the local catalogs in the data directory as well as external catalogs. This function is responsible for operations like:


    
    An interface auxilary to the catalogs in the data directory  
    Package: VO.search  
    Description: An auxilary function to interface the local catalogs in the  
    data directory as well as external catalogs.  
    This function is responsible for operations like:  
    datacats.binaries.SB9  
    Input  : - Catalog file name, or function handle.  
    - Catalog path.  
    * Either ('q',QueryString) or (RA,Dec,Radius).  
    Output : - AstCat object  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Feb 2017  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example:  
    Cat=VO.search.catalog_interface('SNR.mat','/home/eran/matlab/data/+datacats/+SN')  
    Reliable: 2  
      
      
### VO.search.get_cat

Search selected astronomical catalogs Package: VO.search Description: Search selected astronomical catalogs


    
    Search selected astronomical catalogs  
    Package: VO.search  
    Description: Search selected astronomical catalogs  
    Input  : - Catalog name: 'sdss' | 'apass' | ...  
    - J2000.0 R.A. [default radians or sexagesimal].  
    - J2000.0 Dec. [default radians or sexagesimal].  
    - Radius [default in arcsec].  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    'CooUnits' - input coordinate units 'rad'|'deg'.  
    Default is 'rad'.  
    'RadUnits' - 'arcsec'|'arcmin'|'deg'|'rad'.  
    Default is 'arcsec'.  
    'Shape'    - 'circ'. Default is 'circ'.  
    Output : - An AstCat object with the catalog  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Jul 2017  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Cat=VO.search.get_cat('sdss',pi,0.1,3600);  
    Reliable: 2  
      
### VO.search.htmcat_names

Get names of all HDF5/HTM catalogs in the /data/catsHTM/ directory. Package: VO.search Description: Get names of all HDF5/HTM catalogs in the /data/catsHTM/ directory.


    
    Get names of all HDF5/HTM catalogs in the /data/catsHTM/ directory.  
    Package: VO.search  
    Description: Get names of all HDF5/HTM catalogs in the /data/catsHTM/  
    directory.  
    Input  : null  
    Output : - A cell array of catalog names.  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Jan 2018  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: CatName=VO.search.htmcat_names  
    Reliable: 2  
      
      
      
      
### VO.search.htmcat_search

Cone earch on local HDF5/HTM catalog (OBSOLETE). Package: VO.search Description: Perform a cone search around RA/Dec on a local catalog in HDF5 format sorted into HTM. OBSOLETE: use catsHTM.cone_search instead.


    
    Cone earch on local HDF5/HTM catalog (OBSOLETE).  
    Package: VO.search  
    Description: Perform a cone search around RA/Dec on a local catalog in  
    HDF5 format sorted into HTM.  
    OBSOLETE: use catsHTM.cone_search instead.  
    Input  : - Catalog name (e.g., 'GAfADR1').  
    see VO.search.htmcat_names for options.  
    - J2000.0 R.A. [radians, [H M S], or sexagesimal string].  
    - J2000.0 Dec. [radians, [sign D M S], or sexagesimal string].  
    - Search radius [arcsec].  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    Output : -  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Dec 2017  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Cat=VO.search.htmcat_search('UCAC4',1,1,10);  
    Cat=VO.search.htmcat_search('GAIADR1',1,1,10);  
    Cat=VO.search.htmcat_search('GALEX',1,1,10);  
    Reliable:  
      
      
### VO.search.match_cats

Match two spherical coordinates catalogs sorted by declination Package: VO.search Description: Given two spherical coordinate catalogs. - for each entry in the reference catalog (second input argument), search for all nearby sources in the catalog


    
    Match two spherical coordinates catalogs sorted by declination  
    Package: VO.search  
    Description: Given two spherical coordinate catalogs. - for each entry  
    in the reference catalog (second input  
    argument), search for all nearby sources in the catalog  
    (first input).  
    Input  : - A catalog sorted by declination.  
    - A reference catalog.  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    'Radius' - Search radius. This is either a scalar or a vector  
    which length is identical to that of the reference  
    catalog (second input). If a vector than each  
    source in the reference catalog may have a  
    different search radius.  
    Default is 2 (arcsec).  
    'RadiusUnits' - Search radius units.  
    See convert.angular for options.  
    Default is 'arcsec'.  
    'CalcPA' - A logical flag indicating if to calculate and  
    return also the position angle for each matched  
    source.  
    Default is false.  
    'ColRA'  - Index of logitude column in the first input  
    catalog. Default is 1.  
    'ColDec' - Index of latitude column in the first input  
    catalog. Default is 2.  
    'ColRefRA'-Index of logitude column in the second input  
    reference catalog. Default is 1.  
    'ColRefDec'-Index of latitude column in the second input  
    reference catalog. Default is 2.  
    'CooUnits' - Units of coordinates in first input catalog.  
    Default is 'rad'.  
    'CooRefUnits'-Units of coordinates in second input  
    reference catalog.  
    Default is 'rad'.  
    'CheckIsSorted'- A logical flag indicating if to check that  
    the first input catalog is sorted.  
    Default is true.  
    Output : - Structure that contains the following fields, each containing  
    a vector which length is identical to the size of the  
    reference catalog.  
    'Nfound' - Number of sources found in the catalog that within  
    the search radius from the source in the reference  
    catalog.  
    'MinDist'- Minimum distance (radians) of matched sources.  
    NaN if not found.  
    'MinPA'  - P.A. of matched source with minimum distance.  
    NaN if not found or 'CalcPA' is false.  
    - A structure array containing additional information on the  
    matched sources, an entry for each matched refernce source,  
    with the following fields:  
    'IndRef' - Index of source in reference catalog.  
    'IndCat' - List of indices in the catalog that are matched to  
    the 'IndRef' source in the reference catalog.  
    'Dist'   - Vecor of angular distances (radians) for each one  
    of the sources indicated in 'IndCat'.  
    'PA'     - Vector of position angles (radians).  
    'Num'    - Number of sources within search radius.  
    - Vector of indices to nearest source in Cat. NaN if not found.  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Sep 2017  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Cat=sortrows(rand(10000,2),2);  
    Ref=rand(10000,2);  
    Reliable:  
      
      
      
### VO.search.match_cats_pl

Match two planer coordinates catalogs sorted by Y Package: VO.search Description: Given two planer coordinate catalogs. - for each entry in the reference catalog (second input argument), search for all nearby sources in the catalog


    
    Match two planer coordinates catalogs sorted by Y  
    Package: VO.search  
    Description: Given two planer coordinate catalogs. - for each entry  
    in the reference catalog (second input  
    argument), search for all nearby sources in the catalog  
    (first input).  
    Input  : - A catalog sorted by Y.  
    - A reference catalog.  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    'Radius' - Search radius. This is either a scalar or a vector  
    which length is identical to that of the reference  
    catalog (second input). If a vector than each  
    source in the reference catalog may have a  
    different search radius.  
    Default is 2 (arcsec).  
    'CalcPA' - A logical flag indicating if to calculate and  
    return also the position angle for each matched  
    source.  
    Default is false.  
    'ColRA'  - Index of logitude column in the first input  
    catalog. Default is 1.  
    'ColDec' - Index of latitude column in the first input  
    catalog. Default is 2.  
    'ColRefRA'-Index of logitude column in the second input  
    reference catalog. Default is 1.  
    'ColRefDec'-Index of latitude column in the second input  
    reference catalog. Default is 2.  
    'CheckIsSorted'- A logical flag indicating if to check that  
    the first input catalog is sorted.  
    Default is true.  
    Output : - Structure that contains the following fields, each containing  
    a vector which length is identical to the size of the  
    reference catalog.  
    'Nfound' - Number of sources found in the catalog that within  
    the search radius from the source in the reference  
    catalog.  
    'MinDist'- Minimum distance (radians) of matched sources.  
    NaN if not found.  
    'MinPA'  - P.A. of matched source with minimum distance.  
    NaN if not found or 'CalcPA' is false.  
    - A structure array containing additional information on the  
    matched sources, an entry for each matched refernce source,  
    with the following fields:  
    'IndRef' - Index of source in reference catalog.  
    'IndCat' - List of indices in the catalog that are matched to  
    the 'IndRef' source in the reference catalog.  
    'Dist'   - Vecor of angular distances (radians) for each one  
    of the sources indicated in 'IndCat'.  
    'PA'     - Vector of position angles (radians).  
    'Num'    - Number of sources within search radius.  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Sep 2017  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Cat=sortrows(rand(1000,2),2); Ref=rand(1000,2);  
    Cat = Cat.*5000; Ref = Ref.*5000;  
    Cat(1:50,:) = Ref(1:50,:);  
    Cat = sortrows(Cat,2);  
    [Vec,Res]=VO.search.match_cats_pl(Cat,Ref)  
    Reliable:  
      
      
      
### VO.search.prep_data_dir

Prepare interface functions for the catalogs in the data directory Package: VO.search Description: Prepare interface functions for the catalogs in the data directory


    
    Prepare interface functions for the catalogs in the data directory  
    Package: VO.search  
    Description: Prepare interface functions for the catalogs in the data directory  
    Input  : * Arbitrary number of pairs if ...,keyword,value,...  
    The possible keywords are possible:  
    'Dir' - Directory in which the data catalogs resides.  
    The directory tree should be stored like a package.  
    each directory start with "+".  
    Default is '~/matlab/data/+datacats/';  
    'Exten' - Cell array of file extensions to map.  
    Default is {'mat'}.  
    Output : null  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Feb 2017  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: VO.search.prep_data_dir  
    Reliable: 2  
      
      
### VO.search.proper_motion_sdss_ps1

SHORT DESCRIPTION HERE Package: VO.search Description:


    
    SHORT DESCRIPTION HERE  
    Package: VO.search  
    Description:  
    Input  : -  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    Output : -  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Apr 2018  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [Out]=VO.search.proper_motion_sdss_ps1(RA,Dec);  
    Reliable:  
      
      
### VO.search.search_cat

search_cat function                                            Catalogue Description: Given a catalog with Long,Lat coordinates position, search for lines near a list of reference positions. This function can be used to search for a near(est) position


    
      
    search_cat function                                            Catalogue  
    Description: Given a catalog with Long,Lat coordinates position,  
    search for lines near a list of reference positions.  
    This function can be used to search for a near(est) position  
    in a catalog or to match two catalogs.  
    This function replaces cat_search.m and cat_match.m  
    Input  : - Input catalog to saerch.  
    The input catalog should contains at least two columns  
    containing the longitude (X) and latitude (Y) coordinates.  
    The catalog can be a matrix,  
    a structure (or SIM) containing an astronomical catalog (e.g.,  
    FIRST.mat), or a string containing the name of a mat file  
    containing a structure of an an astronomical catalog (e.g.,  
    'FIRST.mat').  
    The last two options are available only if the number of  
    input arguments is not 2 (e.g., set Y=[] if needed).  
    - A single column vector containing the reference catalog  
    longitude to search,  
    or a two column matrix [Long, Lat] to search.  
    If two columns then the units are either radians or deg  
    (according to the value of 'IsRefdeg', default is radians).  
    If single column and IsRefdeg=false then will use convertdms  
    to convert the longitude into radians.  
    Note that if there are only two input argument the program  
    works in a fast mode, in which all the initilaization is  
    skipped.  
    - Either an empty matrix, or a single column vector of  
    latitudes. If IsRefdeg=false then will use convertdms  
    to convert the longitude into radians.  
    * Arbitrary number of pairs or arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    'SearchRad' - Search radius (radians). Default is 2./(RAD*3600) (2").  
    'CooType' - {'sphere'|'plane'}. Default is 'sphere'.  
    Alternatively this can be a function handle name  
    which calculate the distance between two points  
    (e.g., @sphere_dist).  
    'IsCatdeg'- A flag indicating if the input catalog coordinates  
    are in radians (false) or deg (true).  
    Default is false.  
    'IsRefdeg'- A flag indicating if the input reference catalog  
    coordinates are in radians (false) or deg (true).  
    Default is false.  
    'IsCatSorted'- A flag indicating if the input catalog is  
    sorted {true|false}. Default is true.  
    'IsRad'   - If true, then will assume that input reference  
    (search) coordinates are already in radians,  
    and no conversion is needed. Default is false.  
    'ColX'    - Column index or name of the longitude in the input  
    catalog. Default is 1.  
    'ColY'    - Column index or name of the latitude in the input  
    catalog. Default is 2.  
    'SortCol' - By which column the input catalog is sorted.  
    Either 'X'|'Y' or column index. Default is 'Y'  
    (i.e., latitude).  
    'RefSearchCol' - Corresponding sorted column index in the  
    reference catalog. Note that the reference catalog  
    not need to be sorted.  
    Default is 2.  
    'SearchMethod' - Options are:  
    'find' - use find slow search.  
    'bin'  - one-d binary search on sorted column.  
    'binm' - simultaneous binary search.  
    'binms'- simultaneous binary search, with fast  
    implemntation of spherical ang. distance  
    and no position angle calculation.  
    This is valid only for CooType='sphere'.  
    In this case CooType can't be a funtion  
    handle. Default.  
    'binms1' - like 'binms', but return the index  
    and distance of the neasrest source only.  
    'binmdup' - like 'binms', but in this case the  
    search is only for non-identical  
    duplicate entries in the same list.  
    This option should be used when you like  
    to remove non-identical duplicate entries  
    from a list.  
    In this case CooType can't be a funtion  
    handle.  
    'FunBinS' - Binary search function handle.  
    Default is @bin_sear2.  
    Output : - Structure array with element per each entry in the reference  
    [X,Y] catalog. The structure containing the following  
    information:  
    .IndCat    - Vector of indices of matched sources in the  
    sorted input catalog.  
    .DistRAD   - Vector of angular distances [radians] per each  
    match.  
    .PA        - Vector of position angles [radians] per each  
    match.  
    .Nfound    - Scalar indicating the number of matches.  
    - A boolean vector indicating if an entry in the (sorted)  
    input catalog was matched (false) or not (true).  
    - The sorted input catalog. The indices in the output structure  
    reffers to this catalog.  
    Tested : Matlab R2014a  
    By : Eran O. Ofek                    Jan 2015  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: R=sortrows(rand(1000000,2),2);  
    [Res,CatUM,Cat]=search_cat(R(1:10000,1:2),R(1:10000,1:2));  
    [Res,CatUM,Cat]=search_cat(R,'02:10:10.0','+38:45:12','SearchRad',1./RAD);  
    [Res,CatUM,Cat]=search_cat('FIRST.mat','12:10:10.0','+38:45:12','SearchRad',1./RAD);  
    [Res,CatUM,Cat]=search_cat(R,10.0,+12.0,'SearchRad',0.1./RAD,'IsRefdeg',true);  
    match catalog  
    load FIRST.mat  
    [Res,CatUM,Cat]=search_cat(FIRST.Cat,FIRST.Cat(:,1:2));  
    Reliable: 2  
      
### VO.search.search_htmcat

Search a local HTM/HDF5 catalog Description: Perform a cone search in an local catalog formatted in the HTM (Hiraricical Triangular Mesh) grid and stired in HDF5 format. Such catalogs are constructed and described in the


    
    Search a local HTM/HDF5 catalog  
    Description: Perform a cone search in an local catalog formatted in  
    the HTM (Hiraricical Triangular Mesh) grid and stired in  
    HDF5 format.  
    Such catalogs are constructed and described in the  
    Catalog.prep.build_htm_catalog function.  
    Input  : -  
    Output : -  
      
      
### VO.search.search_mhtm_cat

Search master HTM catalog Package: Catalog.search Description: A Master HTM catalog contains a list of HTM regions and the HDF file name containing the HTM region. This function can search for objects near coordinates using such master HTM


    
    Search master HTM catalog  
    Package: Catalog.search  
    Description: A Master HTM catalog contains a list of HTM regions and the  
    HDF file name containing the HTM region. This function can  
    search for objects near coordinates using such master HTM  
    catalogs.  
    Input  : - Master HTM catalog name in HDF5 format.  
    - J2000.0 R.A.  
    - J2000.0 Dec.  
    - Search radius [arcsec default]. Default is 60".  
    * Arbitrary number of ...,key,val,.. arguments.  
    The following keywords are available:  
    'SearcRadUnits' - Search radius units. See convert.angular for  
    options. Default is 'arcsec'.  
    'BaseName'  - HTM files base name.  
    Default is 'GAIA_DR1_HTM06d.hdf5'.  
    'VarBaseName'- HDF5 variable name base name.  
    Default is 'HTM06d'.  
    'HTMinFile' - Number of HTM in file. Default is 100.  
    'ColCellFile' - Mat file containing ColCell of catalog column  
    names. Default is 'GAIA_DR1_HTMcolcell.mat'.  
    'SortBy'    - Sort catalog by column name or index.  
    If empty then do not sort.  
    Default is [].  
    'ColRA'     - RA column name in catalog. Default is 'RA'.  
    'ColDec'     - Dec column name in catalog. Default is 'Dec'.  
    Example: Cat=Catalog.search.search_mhtm_cat('GAIA_DR1_HTMindex.hdf5',1,1,3600);  
    Reliable: 2  
      
### VO.search.search_sortedY_multi

Search a single X/Y in a catalog sorted by Y (planar geometry) Package: VO.search Description: A low level function for a single cone search in a [X, Y] array.


    
    Search a single X/Y in a catalog sorted by Y (planar geometry)  
    Package: VO.search  
    Description: A low level function for a single cone search  
    in a [X, Y] array.  
    Input  : - An array of [X, Y] in radians, sorted by Y.  
    The program doesnot verify that the array is sorted.  
    - X to search. Vector or scalar.  
    - Y to search. Vector or scalar.  
    - Search radius.  
    If radius is negative then will add .Dist to the output index  
    structure.  
    - A vector of false with the length of the catalog,  
    after first iteration provide the output.  
    Output : - A strucure array in which the number of elements equal to the  
    number of searched coordinates, and with the following  
    fields:  
    'Ind' - Vector of indices of matched sources.  
    'Nmatch' - Number of matched sources.  
    'Dist' - Distance between sources. This is provided only if  
    the input search radius is negative.  
    - A logical vector of length equal to the number of searched  
    coordinates (Y) which flag the first unique source.  
    - A matrix of matched indices for the nearest match only:  
    [Index in Cat, Dist between nearest match, DeltaX, DeltaY].  
    where DeltaX/Y is Search coordinates - Cat coordinates.  
    Number of lines is as the number of searched coordinates.  
    By : Eran O. Ofek                    Feb 2017  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Cat=sortrows(rand(10000,2).*1024,2);  
    Ind=VO.search.search_sortedY_multi(Cat,0.5,0.5,10)  
    [Ind,~,MI]=VO.search.search_sortedY_multi(Cat+randn(10000,2).*0.1,Cat(:,1),Cat(:,2),1)  
    Reliable: 2  
      
      
### VO.search.search_sortedlat

Search a single long/lat in a catalog sorted by latitude Package: VO.search Description: A low level function for a single cone search in a [Long, Lat] array.


    
    Search a single long/lat in a catalog sorted by latitude  
    Package: VO.search  
    Description: A low level function for a single cone search  
    in a [Long, Lat] array.  
    Input  : - An array of [Long, Lat] in radians, sorted by Lat.  
    The program doesnot verify that the array is sorted.  
    - Longitude [radians] to search.  
    - Latitude [radians] to search.  
    - Radius [radians] to search.  
    Output : - Indices of the entries in the input [Lon, Lat] catalog which  
    are found within Radius of Long,Lat.  
    By : Eran O. Ofek                    Feb 2017  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Cat=sortrows(rand(10000,2),2);  
    Ind=VO.search.search_sortedlat(Cat,0.5,0.5,0.01)  
    Reliable: 2  
      
### VO.search.search_sortedlat_multi

Search a single long/lat in a catalog sorted by latitude Package: VO.search Description: A low level function for a single cone search in a [Long, Lat] array.


    
    Search a single long/lat in a catalog sorted by latitude  
    Package: VO.search  
    Description: A low level function for a single cone search  
    in a [Long, Lat] array.  
    Input  : - An array of [Long, Lat] in radians, sorted by Lat.  
    The program doesnot verify that the array is sorted.  
    - Longitude [radians] to search.  
    - Latitude [radians] to search.  
    - Radius [radians] to search.  
    If radius is negative then will add .Dist to the output index  
    structure.  
    - A vector of false with the length of the catalog,  
    after first iteration provide the output.  
    - A function handle for calculating distances Fun(X1,Y1,X2,Y2).  
    Default is @celestial.coo.sphere_dist_fast.  
    Output : - A strucure array in which the number of elements equal to the  
    number of searched coordinates (Lat), and with the following  
    fields:  
    'Ind' - Vector of indices of matched sources.  
    'Nmatch' - Number of matched sources.  
    'Dist' - Distance between sources. This is provided only if  
    the input search radius is negative.  
    - A logical vector of length equal to the number of searched  
    coordinates (Cat) which flag the first unique source.  
    - If Cat and Long/Lat have the same length and are the same  
    catalog, then this is a flag indicating if a source was  
    already found as another source.  
    By : Eran O. Ofek                    Feb 2017  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Cat=sortrows(rand(10000,2),2);  
    Ind=VO.search.search_sortedlat_multi(Cat,0.5,0.5,0.01)  
    Reliable: 2  
      
### VO.search.search_sortedlat_multiNearest

Search a single long/lat in a catalog sorted by latitude Package: VO.search Description: A low level function for a single cone search in a [Long, Lat] array.


    
    Search a single long/lat in a catalog sorted by latitude  
    Package: VO.search  
    Description: A low level function for a single cone search  
    in a [Long, Lat] array.  
    Input  : - An array of [Long, Lat] in radians, sorted by Lat.  
    The program doesnot verify that the array is sorted.  
    - Longitude [radians] to search.  
    - Latitude [radians] to search.  
    - Radius [radians] to search.  
    - A function handle for calculating distances Fun(X1,Y1,X2,Y2).  
    Default is @celestial.coo.sphere_dist_fast.  
    Output : - A three column matrix with, one line per line in Long,Lat.  
    Columns are [Index of nearest source, within search radius, in  
    Cat;  
    Distance; Total number of matches within radius].  
    - A vector of logical (length as Cat), which indicate the object  
    in Cat that were identified as the nearest object to a source in  
    Long, Lat.  
    - The same as the previous output, but for all the sources  
    within the search radius.  
    - A vector of the indices of the sources in the ref image. NaN  
    if the source is not apperas in the ref image.  
    By : Eran O. Ofek                    Feb 2017  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Cat=sortrows(rand(10000,2),2);  
    Ind=VO.search.search_sortedlat_multi(Cat,0.5,0.5,0.01)  
    Reliable: 2  
      
### VO.search.search_sortedlong

Search a single long/lat in a catalog sorted by longitude Package: VO.search Description: A low level function for a single cone search in a [Long, Lat] array.


    
    Search a single long/lat in a catalog sorted by longitude  
    Package: VO.search  
    Description: A low level function for a single cone search  
    in a [Long, Lat] array.  
    Input  : - An array of [Long, Lat] in radians, sorted by Long.  
    The program doesnot verify that the array is sorted.  
    - Longitude [radians] to search.  
    - Latitude [radians] to search.  
    - Radius [radians] to search.  
    Output : - Indices of the entries in the input [Lon, Lat] catalog which  
    are found within Radius of Long,Lat.  
    By : Eran O. Ofek                    Feb 2017  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Cat=sortrows(rand(10000,2),1);  
    Ind=VO.search.search_sortedlong(Cat,0.5,0.5,0.01)  
    Reliable: 2  
      
### VO.search.simbad_url

Generate a SIMBAD URL for coordinates Package: VO.search Description: Generate a SIMBAD URL for coordinates


    
    Generate a SIMBAD URL for coordinates  
    Package: VO.search  
    Description: Generate a SIMBAD URL for coordinates  
    Input  : - J2000 RA [rad]. See celestial.coo.convertdms for other options.  
    This is either a scalar or a vector of coordinates.  
    - J2000 Dec [rad]. See celestial.coo.convertdms for other options.  
    This is either a scalar or a vector of coordinates.  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    'SearchRadius' - Default is 2.  
    'SearchRadiusUnits' - Default is 'arcmin'.  
    'CooFrame' - Default is 'FK5'.  
    'CooEpoch' - Default is 2000.  
    'CooEquinox' - Default is 2000.  
    Output : - A structure arrary withe the following fields:  
    'URL' - The SIMBAD URL for this coordinates.  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Aug 2019  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [Data]=VO.search.simbad_url(1,1);  
    Reliable: 2  
      
      
      
