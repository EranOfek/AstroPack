# Package: VO.SkyMapper


### VO.SkyMapper.skymapper_cat_search

Cone search the SkyMapper online catalog. Package: VO.SkyMapper Description: Cone search the SkyMapper online catalog.


    
    Cone search the SkyMapper online catalog.  
    Package: VO.SkyMapper  
    Description: Cone search the SkyMapper online catalog.  
    Input  : - Vector of J2000.0 R.A. [rad] or see celestial.coo.convertdms.  
    - Vector of J2000.0 Dec. [rad] or see celestial.coo.convertdms.  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    'Radius'   - Search radius. Default is 30 [arcsec].  
    'RadiusUnits' - Search radius units. Default is 'arcsec'.  
    'Output'   - Catalog output format:  
    'astcat' - An AstCat object (matrix catalog).  
    'astcat_t' - An AstCat object (table catalog).  
    'struct' - A structure array (matrix catalog).  
    'struct_t' - A structure array (tanle catalog).  
    'BaseURL' - Base URL search. Default is:  
    'http://skymapper.anu.edu.au/sm-cone/query?'.  
    Output : - A structure or AstCat array in which each element is for  
    one search coordinate, and contains a table of entries within  
    the radius from the search coordinates.  
    - A cell array of links from which the data was retrieved.  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Dec 2017  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reference: http://skymapper.anu.edu.au/how-to-access/#adql  
    Example: [Out,Link]=VO.SkyMapper.skymapper_cat_search(1,-1)  
    Reliable: 2  
      
      
### VO.SkyMapper.skymapper_cutout_link

Generate a URL link to SkyMapper image cutouts. Package: VO.SkyMapper Description: Generate a URL link to SkyMapper image cutouts.


    
    Generate a URL link to SkyMapper image cutouts.  
    Package: VO.SkyMapper  
    Description: Generate a URL link to SkyMapper image cutouts.  
    Input  : - J2000.0 R.A. [rad] or see celestial.coo.convertdms.  
    - J2000.0 Dec. [rad] or see celestial.coo.convertdms.  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    'Size' - Cutout size [deg deg]. Default is [0.1 0.1].  
    'Format' - 'fits' | 'png'. Default is 'fits'.  
    'Bands'  - Defaukt is 'y,v,g,r,i,z'.  
    'Intersect - 'covers' | 'center' | 'overlaps'  
    Default s 'covers'.  
    'StartJD' - Default is [1 1 2000].  
    'EndJD'   - Default is [1 1 2100].  
    'BaseURL' - Default is  
    'http://skymappersiap.asvo.nci.org.au/dr1_cutout/query?'.  
    Output : -  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Dec 2017  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Link=VO.SkyMapper.skymapper_cutout_link(1,-1)  
    Reliable: 2  
      
      
