# Package: VO.GALEX


### VO.GALEX.coo2id

Convert RA/Dec to GALEX image identifiers Package: VO.GALEX Description: Convert RA/Dec to GALEX image IDs.


    
    Convert RA/Dec to GALEX image identifiers  
    Package: VO.GALEX  
    Description: Convert RA/Dec to GALEX image IDs.  
    Input  : - Vector of J2000.0 R.A. [rad, [H M S], or sexagesimal].  
    - Vector of J2000.0 Dec. [rad, [Sign D M S], or sexagesimal].  
    - Data release. Default is 'GR7'.  
    Output : - AstCat object containing the GALEX images that  
    cover the requested coordinates.  
    Each AstCat element correspond to a requested  
    coordinates.  
    - Cell array of matrices. Cell element per requested  
    coordinate. Each cell contains the GALEX image ID  
    matrix with the following columns:  
    [Vsn, Tilenum, Type, Ow, Prod, Img, Try].  
    - Cell array of index of images in the GALEX image  
    catalog and the galex image path catalog.  
    Example: [CatID,ID,Ind]=VO.GALEX.coo2id('01:10:10.1','+00:20:30')  
    [CatID,ID,Ind]=VO.GALEX.coo2id([1;2],[0;1])  
    Reliable: 2  
      
### VO.GALEX.fov

Get the GALEX field of view radius Package: VO.GALEX Description: Return GALEX field of view (FOV)


    
    Get the GALEX field of view radius  
    Package: VO.GALEX  
    Description: Return GALEX field of view (FOV)  
    Input  : null  
    Output : FOV radius in degrees.  
    Example: VO.GALEX.fov  
    Reliable: 2  
      
### VO.GALEX.image_server

Get the GALEX image server URL Package: VO.GALEX Description: Get GALEX image server URL


    
    Get the GALEX image server URL  
    Package: VO.GALEX  
    Description: Get GALEX image server URL  
    Input  : null  
    Output : GALEX image server URL  
    Example: Server=VO.GALEX.image_server  
      
### VO.GALEX.images_db_filename

Get the database of all the GALEX images file names Package: VO.GALEX Description: Get the GALEX images file name database


    
    Get the database of all the GALEX images file names  
    Package: VO.GALEX  
    Description: Get the GALEX images file name database  
    Input  : - Data release. Default is 'GR7'.  
    Output : - HDF5 file name containing the catalog of GALEX  
    images. Use AstCat.loadh2astcat to read into an  
    AstCat object.  
    - mat file containing the path for the NUV images.  
    - mat file containing the path for the FUV images.  
    Example: F=VO.GALEX.images_db_filename;  
    Reliable: 2  
      
      
### VO.GALEX.ind2path

Convert index of the GALEX images DB file to GALEX image path Package: VO.GALEX Description: Convert index in the GALEX images file to GALEX images path.


    
    Convert index of the GALEX images DB file to GALEX image path  
    Package: VO.GALEX  
    Description: Convert index in the GALEX images file to  
    GALEX images path.  
    Input  : - Index.  
    - Data relaese. Default is 'GR7'.  
    Output : - GALEX NUV images path.  
    - GALEX FUV images path.  
    Reliable: 2  
      
### VO.GALEX.pixscale

Get the GALEX science image pixel scale. Package: VO.GALEX Description: Return GALEX pixel scale.


    
    Get the GALEX science image pixel scale.  
    Package: VO.GALEX  
    Description: Return GALEX pixel scale.  
    Input  : null  
    Output : - Pixel scale [arcsec/pix].  
    Example: VO.GALEX.pixscale  
    Reliable: 1  
      
### VO.GALEX.run_galex_sql

Run a GALEX command line SQL quary (OBSOLETE - see VO.MAST) Package: VO.GALEX Description: Run a GALEX command line SQL quary. Running the java command line tool.


    
    Run a GALEX command line SQL quary (OBSOLETE - see VO.MAST)  
    Package: VO.GALEX  
    Description: Run a GALEX command line SQL quary. Running the java  
    command line tool.  
    Input  : - File name containing the SQL query, or alternatively,  
    a string containing the full query,  or alternatively,  
    this can be a 3 elements cell array, in which the  
    first cell contains : a cell array of attributes in the  
    SELECT clause.  
    second cell contains: a cell array of tables/views in the  
    FROM clause.  
    third cell contains : a string to appear in the WHERE clause.  
    - Output file name.  
    If empty (i.e., []), then create a tmp file, which will  
    be deleteded at the end of the process, default is [].  
    - Query type:  
    'file'   - First argument is file name containing query (default).  
    'string' - First argument is the full query string.  
    'cell'   - First argument is a cell array of attributes.  
    - Optional string to concatenate to query (e.g., this could be  
    a variable part of the query), default is empty string (i.e., '').  
    Output : - Optional catalog, containing the search result.  
    - Return code: 1 - objects found; 0- No objects have been found  
    - First line in resulted file.  
    Instellation: Follow the casjobs.jar instellation instructions in:  
    http://galex.stsci.edu/casjobs/casjobscl.aspx  
    install the casjobs.jar and CasJobs.config files in  
    .../matlab/fun/bin/CasJobs/GALEX/  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                    Jul 2005  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Needed : The python script sqlcl.py should be located in ./sqlcl/ directory  
    relative to the location of this script.  
    Notes  : Generic SDSS queries can be found in the ./sqlcl/sql/ directory.  
    Example: Scheme browser at: http://galex.stsci.edu/GR6/?page=dbinfo  
    Static file query:  
    Query='/scr2/eran/bin/sdssQA/CasJOB/sql/get_1004.sql';  
    [Cat,Msg,FirstLine]=VO.GALEX.run_galex_sql(Query,[],'file');  
    dynamic file query:  
    Query='/scr2/eran/matlab/sdss/bin/sql/get_obj_field.sql';  
    Con='run=3813 and rerun=41 and camcol=6 and field=60';  
    [Cat,Msg,FirstLine]=VO.GALEX.run_galex_sql(Query,[],'file',Con);  
    string query:  
    QueryStr = 'select top 2 ra,dec from star';  
    [Cat,Msg,FirstLine]=VO.GALEX.run_galex_sql(QueryStr,[],'string');  
    Cell query:  
    Q{1} = {'ra','dec','nuv_mag','nuv_magerr'};  
    Q{2} = {'photoobjall'};  
    Q{3} = '(ra between 0 and 0.1) and (dec between 0 and 0.1)';  
    [Cat,Msg]=VO.GALEX.run_galex_sql(Q,[],'cell');  
    Reliable:  
      
### VO.GALEX.searchGALEXimg

Search GALEX images by coordinates Package: VO.GALEX Description: Search GALEX images by coordinates in the cats.GALEX.GALEXimg local catalog.


    
    Search GALEX images by coordinates  
    Package: VO.GALEX  
    Description: Search GALEX images by coordinates in the  
    cats.GALEX.GALEXimg local catalog.  
    Input  : - J2000.0 R.A. [rad, [H M S], or sexagesimal  
    string], or and Index in the GALEX images file.  
    - J2000.0 Dec. [rad, [Sign D M S], or sexagesimal  
    string]. If first argument is ID then this need to  
    be an empty matrix. Default is empty.  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    'SearchRadius' - Default is 1.  
    'SearchRadiusUnits' - Default is 'deg';  
    'CooUnits' - {'deg' | 'rad'}, Default is 'rad'.  
    If 'rad' then will use celestial.coo.convertdms  
    to read coordinates.  
    Output : - A structure of links to the various products.  
    - An AstCat object containing the selected images.  
    Example: [Links,GALEX]=VO.GALEX.searchGALEXimg(RA,Dec)  
    Reliable: 2  
      
      
### VO.GALEX.src_number_count

The cumulative number of sources in the GALEX-NUV band at high Gal. lat. Package: VO.GALEX Description: A simplistic, order of magnitude estimate of the surfae area of sources in the GALEX-NUV band at high Galactic latitude based on interpolation of the Bianchi et al. (2007) data in


    
    The cumulative number of sources in the GALEX-NUV band at high Gal. lat.  
    Package: VO.GALEX  
    Description: A simplistic, order of magnitude estimate of the surfae area  
    of sources in the GALEX-NUV band at high Galactic latitude  
    based on interpolation of the Bianchi et al. (2007) data in  
    the 18 to 22 mag range.  
    Input  : - AB mag  
    - Band. Default is 'NUV'.  
    Output : - Surface number of stars brighter than the requested magnitude  
    per deg^2 at high Galactic latitude.  
    - Differential number od stars per deg^2 per mag bin.  
    By : Eran O. Ofek                    Oct 2016  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: N=VO.GALEX.src_number_count(21)  
    Reliable: 2  
      
      
### VO.GALEX.wget_corrim

Get GALEX corrected images from image server Package: VO.GALEX Description: Get GALEX corrected images


    
    Get GALEX corrected images from image server  
    Package: VO.GALEX  
    Description: Get GALEX corrected images  
    Input  : - J2000.0 R.A. [rad, [H M S], or sexagesimal  
    string], or and Index in the GALEX images file.  
    - J2000.0 Dec. [rad, [Sign D M S], or sexagesimal  
    string]. If first argument is ID then this need to  
    be an empty matrix. Default is empty.  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    'Band' - Bands of images to retrieve. Either a string  
    containing a single or multiple bands  
    (e.g., 'n' | 'nf'), or a cell array of strings  
    (e.g., {'n,'f'}).  
    'Save' - Save the images. If false then only get the image  
    links. If true then save the images in the current  
    directory. If a dir name then cd to the dir name and  
    retrieve the files.  
    'ImType'- A string or cell array of image types to retrieve.  
    'i'-int.  
    'c'-cnt.  
    'r'-rrhr(exp time).  
    Default is 'icr'.  
    'DR'    - Data release. Default is 'GR6'.  
    'MaxGet'- Maximum number of parallel wget (see www.pwget.m).  
    Default is 15.  
    'Extra' - Extra parameters to pass to wget (see www.pwget.m).  
    Default is '-nc'.  
    'Unzip' - Unzip the images. Default is true.  
    Output : - Structure array of links to GALEX images.  
    Link(ImageInd,BandIndex,ImTypeIndex)...  
    Example: [LinkSt]=VO.GALEX.wget_corrim(0,0.1);  
    Reliable: 2  
      
### VO.GALEX.zp

Get the GALEX photometric zeropoint. Package: VO.GALEX Description: Return GALEX zeropoint for conversion of counts/second to AB mag.


    
    Get the GALEX photometric zeropoint.  
    Package: VO.GALEX  
    Description: Return GALEX zeropoint for conversion of counts/second  
    to AB mag.  
    Input  : - Band: 'NUV'|'n' | 'FUV'|'f'. Default is 'NUV'.  
    Output : - Photometric zero point: AB mag which produce 1  
    count per second.  
    Example: VO.GALEX.zp  
    Reliable: 1  
      
