# Package: VO.SDSS


### VO.SDSS.coo2run

Convert J2000 equatorial coordinates to SDSS run/rerun/col/field ID. Package: VO.SDSS Description: Given celestial equatorial coordinates, find all SDSS Run/Rerun/Col/Field number ID that cover the coordinates.


    
    Convert J2000 equatorial coordinates to SDSS run/rerun/col/field ID.  
    Package: VO.SDSS  
    Description: Given celestial equatorial coordinates, find all SDSS  
    Run/Rerun/Col/Field number ID that cover the coordinates.  
    Input  : - J2000.0 RA in [rad] or [H M S] or sexagesimal string.  
    - J2000.0 Dec in [rad] or [Sign D M S] or sexagesimal string.  
    - SDSS data relaese, default is 'DR9'.  
    If empty (i.e., []), then use default.  
    - Assign and read {0 | 1} the list of fields into/from  
    the matlab workspace.  
    Default is false.  
    If you are running coo2run many times, use 1 for faster  
    execuation.  
    Output : - A cell array of lists of [Run, Rerun, Col, Field]  
    that covers the given coordinates. Ecah cell for each  
    coordinate.  
    - A cell array of modified JD of frame for [u g r i z] frames,  
    one line per run. Each cell per each coordinate.  
    - A cell array od distances (in radians) of the requested  
    RA, Dec from each side of the frame polygon.  
    Each cell per each coordinate.  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                    Mar 2007  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Package: sdss  
    Example: [Run,MJD,D]=VO.SDSS.coo2run([10 04 00],[1 41 0 0]);  
    min(D{1}.*RAD.*3600,[],2)   print the distance ["] of the  
    coordinate from nearest boundry.  
    Reliable: 2  
      
      
### VO.SDSS.image_server

Get SDSS image server URL Package: VO.SDSS Description: Get the SDSS image server URL


    
    Get SDSS image server URL  
    Package: VO.SDSS  
    Description: Get the SDSS image server URL  
    Input  : null  
    Output : SDSS image server URL  
    Example: VO.SDSS.image_server  
    Reilable: 2  
      
### VO.SDSS.navigator_link

Given J2000 equatorial coordinates get link to SDSS navigator image. Package: VO.SDSS Description: Get link to SDSS navigator image


    
    Given J2000 equatorial coordinates get link to SDSS navigator image.  
    Package: VO.SDSS  
    Description: Get link to SDSS navigator image  
    Input  : - J2000.0 R.A. [rad, [H M S], sexagesimal string]  
    - J2000.0 Dec. [rad, [Sign D M S], sexagesimal string]  
    Output : - Cell array of links.  
    Example: Link=VO.SDSS.navigator_link(pi,0.4)  
    Reliable: 2  
      
### VO.SDSS.pixscale

Get SDSS pixel scale Package: VO.SDSS Description: Return SDSS pixel scale.


    
    Get SDSS pixel scale  
    Package: VO.SDSS  
    Description: Return SDSS pixel scale.  
    Input  : null  
    Output : - Pixel scale [arcsec/pix].  
    Example: VO.SDSS.pixscale  
    Reliable: 2  
      
### VO.SDSS.run_sdss_sql

run_sdss_sql function                                               sdss Description: Run SQL query on SDSS database and retrieve the results into an array.


    
      
    run_sdss_sql function                                               sdss  
    Description: Run SQL query on SDSS database and retrieve the results  
    into an array.  
    Input  : - File name containing the SQL query, or alternatively,  
    a string containing the full query,  or alternatively,  
    this can be a 3 elements cell array, in which the  
    first cell contains : a cell array of attributes in the  
    SELECT clause.  
    second cell contains: a cell array of tables/views in the  
    FROM clause.  
    third cell contains : a string to appear in the WHERE clause.  
    * Arbitrary number of pairs or arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    'OutputFileName' - File name. If empty then no output file.  
    Default is empty.  
    'QueryType' - One of the following query type:  
    'file' - First argument is file name  
    containing the query.  
    Default (unless query is a cell).  
    'string' - First argument is the full query  
    string.  
    'cell'   - First argument is a cell array of  
    attributes.  
    'ConcatString' - Optional string to concatenate to query  
    (e.g., this could be a variable part of the query),  
    default is empty string (i.e., '').  
    'ServerURL' - Default is 'http://skyserver.sdss3.org/public/en/tools/search/x_sql.aspx'  
    Output : - Optional catalog, containing the search result.  
    - Return code: 1 - objects found; 0- No objects have been found  
    - First line in resulted file.  
    - Structure containing a list of all column indices.  
    - Cell array of column names.  
    See also: wget_sdss.m  
    Tested : Matlab R2014a  
    By : Eran O. Ofek                    Jul 2014  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example:  Static file query:  
    Query='/scr2/eran/bin/sdssQA/CasJOB/sql/get_1004.sql';  
    [Cat,Msg,FirstLine]=VO.SDSS.run_sdss_sql(Query);  
    dynamic file query:  
    Query='/scr2/eran/matlab/sdss/bin/sql/get_obj_field.sql';  
    Con='run=3813 and rerun=41 and camcol=6 and field=60';  
    [Cat,Msg,FirstLine]=VO.SDSS.run_sdss_sql(Query,'QueryType','file','ConcatString',Con);  
    string query:  
    QueryStr = 'select top 2 ra,dec from star';  
    [Cat,Msg,FirstLine]=VO.SDSS.run_sdss_sql(QueryStr,'QueryType','string');  
    Cell query:  
    Q{1} = {'ra','dec','psfMag_g'};  
    Q{2} = {'Star'};  
    Q{3} = '(ra between 0 and 1) and (dec between 0 and 1)';  
    [Cat,Msg,FirstLine]=VO.SDSS.run_sdss_sql(Q);  
    Reliable: 1  
      
### VO.SDSS.sdss_coo_radec

Convert the SDSS great circles coordinate system to J2000 RA and Dec. Package: VO.SDSS Description: Convert the SDSS great circles coordinate system to J2000 right ascension and declination.


    
    Convert the SDSS great circles coordinate system to J2000 RA and Dec.  
    Package: VO.SDSS  
    Description: Convert the SDSS great circles coordinate system to J2000  
    right ascension and declination.  
    Input  : - Matrix of X Coordinates (frame row).  
    Each line for each field, where lines can contain  
    a single coordinate or multiple coordinates.  
    If empty, then default is X position of the four corners  
    of the field [65:1425,1:2048].  
    - Matrix of Y Coordinates (frame col).  
    Each line for each field, where lines can contain  
    a single coordinate or multiple coordinates.  
    If empty, then default is Y position of the four corners  
    - Matrix of SDSS coordinates and coef.  
    (e.g., SDSS_DR5_Fields_* files)  
    - Filter of image in which the row and col are specified  
    {'g'|'r'|'i'}, default is 'g'.  
    - Color of object for which to calculate poition,  
    default is 0 (i.e., do not apply color correction).  
    color should be g-r for g-band, and r-i for r and i-bands.  
    UNSPORTED!  
    - If frame row and frame col are empty, then subtract/add  
    margin in pixels to the position of corners, default is 0.  
    - Columns info structure:  
    .A - columns in matrix containing alpha (Node),  
    default is 69.  
    .I - column in matrix containing inclination.  
    default is 68.  
    .GH- columns containing  
    [DROW0_G, DROW1_G, DROW2_G, DROW3_G, DCOL0_G, DCOL1_G, DCOL2_G, DCOL3_G],  
    default is [14:1:21].  
    .AF- columns containing [A_G, B_G, C_G, D_G, E_G, F_G],  
    default is [24:1:29].  
    Output : - RA of corners [rad].  
    Dec of corners [rad].  
    Tested : Matlab 7.0  
    By : Eran O. Ofek / Dovi Poznanski   Jan 2006  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    See also: TargetFields_SDSS_DR4.head  
    Reference: http://www.sdss.org/dr5/products/general/astrometry.html  
    Example:  Calculating the RA/Dec of corners of frames:  
    [RA,Dec]=VO.SDSS.sdss_coo_radec([],[],SDSS_DR5_Fields_Best(1:10,:));  
    Reliable: 2  
      
      
      
### VO.SDSS.wget_corrim

Get corrected SDSS image Package: VO.SDSS Description: Retrieve SDSS corrected images by Run, Rerun, CamCol, Field or RA/Dec.


    
    Get corrected SDSS image  
    Package: VO.SDSS  
    Description: Retrieve SDSS corrected images by Run, Rerun, CamCol, Field  
    or RA/Dec.  
    Retrieve FITS files.  
    Input  : - A 4 columns matrix of SDSS images IDs:  
    [Run, Rerun, CamCol, Field].  
    Or J2000.0 R.A. [rad, [H M S], sexagesimal  
    string].  
    - J2000.0 Dec. [rad, [Sign D M S], sexagesimal  
    string]. If empty, then assume first input  
    argument is [Run, Rerun, CamCol, Field] ID.  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    'Band' - Bands of images to retrieve. Either a string  
    containing a single or multiple bands  
    (e.g., 'g' | 'ugr'), or a cell array of strings  
    (e.g., {'g','r','i'}).  
    'Save' - Save the images. If false then only get the image  
    links. If true then save the images in the current  
    directory. If a dir name then cd to the dir name and  
    retrieve the files.  
    'MaxGet'- Maximum number of parallel wget (see www.pwget.m).  
    Default is 15.  
    'Extra' - Extra parameters to pass to wget (see www.pwget.m).  
    Default is '-nc'.  
    'coo2runPar' - Cell array of parameters to pass to  
    SDSS.coo2run.  
    'Unzip' - Unzip the images. Default is true.  
    Output : - A structure array of images and links.  
    Link(ImageInd,BandInd)...  
    License: GNU general public license version 3  
    Tested : Matlab R2015b  
    By : Eran O. Ofek                    Aug 2016  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [LinkSt]=VO.SDSS.wget_corrim([1000 301 1 27]);  
    Reliable: 2  
      
      
### VO.SDSS.wget_sdss

Query SDSS PhotoPrimary table around specific coordinate. Package: VO.SDSS Description: Query SDSS PhotoPrimary table around specific coordinate. See VO.SDSS.run_sdss_sql.m for a more general queries.


    
    Query SDSS PhotoPrimary table around specific coordinate.  
    Package: VO.SDSS  
    Description: Query SDSS PhotoPrimary table around specific coordinate.  
    See VO.SDSS.run_sdss_sql.m for a more general queries.  
    Input  : - J2000.0 RA (rad, sexagesimal string or [H M S]).  
    - J2000.0 Dec (rad, sexagesimal string or [Sign D M S]).  
    - Search radius (radians). Default is 1 deg.  
    - Search shape {'circ'|'box'}. Default is 'circ';  
    - Cell array of PhotoPrimary columns to retrieve.  
    Default is:  
    {'RA','Dec','type','modelMag_u','modelMagErr_u','modelMag_g',  
    'modelMagErr_g','modelMag_r','modelMagErr_r','modelMag_i',  
    'modelMagErr_i','modelMag_z','modelMagErr_z'};  
    If empty then use default.  
    - Units of output ra and dec {'rad','deg'}. Default is 'rad'.  
    Output : - Matrix of SDSS catalog.  
    - Cell array of column names.  
    - Structure array of column indices.  
    See also: run_sdss_sql.m  
    Tested : Matlab R2014a  
    By : Eran O. Ofek                    Feb 2015  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [Cat,ColCell,Col]=VO.SDSS.wget_sdss('12:00:00','+30:00:00',0.1./RAD);  
    Reliable: 2  
      
### VO.SDSS.wget_sdss_spec

wget SDSS FITS spectra and links Package: VO.SDSS Description: wget SDSS FITS spectra and links


    
    wget SDSS FITS spectra and links  
    Package: VO.SDSS  
    Description: wget SDSS FITS spectra and links  
    Input  : - A 3 column matrix of [Plate, MJD, Fiber] SDSS spectra ID  
    to retrieve.  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    'OutType' - Options: 'AstCat' | 'cellmat' | 'AstSpec'  
    Default is 'AstCat'.  
    'Format' - Default is 'fits'.  
    'Survey' - 'boss' | ['eboss'] | 'sdss'  
    'DR'     - Default is 'dr16'.  
    'SpecType' - 'lite' | 'full'. Default is 'lite'.  
    'Run2D'    - Reduction version. Default is 'v5_10_0'.  
    'SaveFile' - Save FITS file to local disk.  
    Default is true.  
    'UseLink'  - Which link to use for file retrieval.  
    'link' - direct link (faster).  
    'api' - API.  
    'WgetOption' - Retrieval method:  
    'pwget' - Use www.pwget. Default.  
    'websave' - Use websave.  
    'PwgetPar' - Additionl aparmeters to pass to www.pwget.  
    'NP'       - Number of parallel retrival in pwget.  
    Output : - A structure array of URL/file names, with the following  
    fields:  
    'SpecView'  - Link to spectrum viewer.  
    'SpecAPI'   - Link to spectra retireval API.  
    'SpecLink'  - Direct link to spectra retrival.  
    'FileName'  - File name.  
    License: GNU general public license version 3  
    Reference: https://dr16.sdss.org/optical/spectrum/view/data/access  
    By : Eran O. Ofek                    Sep 2019  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [URL,Spec,ColCell]=VO.SDSS.wget_sdss_spec([4055 55359 596])  
    Reliable: 2  
      
      
