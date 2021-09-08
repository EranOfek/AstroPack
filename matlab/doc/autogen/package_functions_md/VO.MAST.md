# Package: VO.MAST


### VO.MAST.mashup_query

SHORT DESCRIPTION HERE Package: VO.MAST Description:


    
    SHORT DESCRIPTION HERE  
    Package: VO.MAST  
    Description:  
    Input  : -  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    Output : -  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Jun 2017  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example:  
    Reliable:  
      
      
### VO.MAST.query_casjobs

Query MAST CasJobs service (requires casjobs.jar) Package: VO.MAST Description: Query the MAST CasJobs service (requires casjobs.jar) For casjobs.jar instellation see:


    
    Query MAST CasJobs service (requires casjobs.jar)  
    Package: VO.MAST  
    Description: Query the MAST CasJobs service (requires casjobs.jar)  
    For casjobs.jar instellation see:  
    http://mastweb.stsci.edu/mcasjobs/casjobscl.aspx  
    Input  : - Query string or 3 elements query clause of the:  
    select, from, where parts of the query.  
    (e.g., {{'ra','dec'},{'photoobj'},''}).  
    Default is 'select top 10 ra, dec from photoobj'  
    If string start with '@' then assume the query is in a file.  
    If empty use default.  
    * Arbitrary number of ...,key,val,... pairs.  
    The following keywords are available:  
    'Table' - Query table name. Default is 'GALEX_GR6Plus7'.  
    Other options include  
    'PanSTARRS_DR1','HSCv2','GAIA_DR1','GALAEX_GR6Plus7',...  
    'CasJobsPath' - The CasJobs java file path  
    The Java binary is available from:  
    http://mastweb.stsci.edu/ps1casjobs/casjobscl.aspx  
    Default is '~/matlab/bin/CasJobs/jar'.  
    The configuration file named 'CasJobs.config'  
    should be located in the same directory.  
    'FormatString' - Format string for query output.  
    If empty, will attempt to use 'f f...'.  
    Default is [].  
    'OutType' - Output type 'AstCat'|'mat'.  
    Default is 'AstCat'.  
    'SaveInTable' - Save catalog in table (rather than matrix).  
    Default is false.  
    'SortBy' - Sort output by column index or name.  
    Default is [].  
    'OnlyQuery' - A flag indicating if to return only the query.  
    If truen, then the query string will be returned  
    without execuation. Default is false.  
    'BoxCoo' - Optional [MinRA, MaxRA, MinDec, MaxDec] to query.  
    Default is empty [radians].  
    If not empty must also provide 'StrRA' and  
    'StrDec'.  
    'StrRA'  - RA string name. Default is empty.  
    'StrDec' - Dec string name. Default is empty.  
    'WaitRetry' - Wait before retry [s]. Default is 1.  
    'Boolean2num' - convert boolean strings to numbers.  
    Default is true.  
    'ShowResult' - Dipslay result string. Default is false.  
    'Verbose'- Verbose: true|false. Default is true.  
    Output : - Output catalog  
    - Cell array of column names  
    - Result status.  
    - Result string.  
    - Query string.  
    See also: query_casjobs_mydb  
    By : Eran O. Ofek                    Jan 2017  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [Out,ColCell,Status]=VO.MAST.query_casjobs;  
    Out = VO.MAST.query_casjobs('select top 10 ra, dec from photoobj');  
    Out = VO.MAST.query_casjobs('select ra, dec from photoobj','boxcoo',[0 0.1 0 0.1].*pi./180,'StrRA','RA','StrDec','Dec');  
    Out = VO.MAST.query_casjobs('select top 10 raMean, decMean from ObjectThin','Table','PanSTARRS_DR1');  
    Out = VO.MAST.query_casjobs('select raMean, decMean from ObjectThin WHERE raMean>0 and raMean<0.1 and decMean>0 and decMean<0.1','Table','PanSTARRS_DR1');  
    To get all table names  
    [~,~,~,Out] = VO.MAST.query_casjobs('SELECT table_name FROM information_schema.tables;')  
    Reliable: 2  
      
      
### VO.MAST.query_casjobs_mydb

Query MAST CasJobs service into MAST mydb (requires casjobs.jar) Package: VO.MAST Description: Query the MAST CasJobs service (requires casjobs.jar), but store the output in mydb.


    
    Query MAST CasJobs service into MAST mydb (requires casjobs.jar)  
    Package: VO.MAST  
    Description: Query the MAST CasJobs service (requires casjobs.jar),  
    but store the output in mydb.  
    Input  : - Query string or 3 elements query clause of the:  
    select, from, where parts of the query.  
    (e.g., {{'ra','dec'},{'photoobj'},''}).  
    Default is 'select top 10 ra, dec from photoobj'  
    If string start with '@' then assume the query is in a file.  
    If empty use default.  
    * Arbitrary number of ...,key,val,... pairs.  
    The following keywords are available:  
    'Table' - Query table name. Default is 'PanSTARRS_DR1'.  
    Other options include 'GALEX_GR6Plus7','HSCv2',...  
    'Into'  - String indicating into which DB name to save the  
    results. Default is 'test' which translate to.  
    'CasJobsPath' - The CasJobs java file path  
    The Java binary is available from:  
    http://mastweb.stsci.edu/ps1casjobs/casjobscl.aspx  
    Default is '~/matlab/bin/CasJobs/jar'.  
    The configuration file named 'CasJobs.config'  
    should be located in the same directory.  
    'Extract' - A flag indicating if to extract the results from  
    the MAST DB archive. Default is true.  
    'BaseURL' - MAST archive URL.  
    Default is  
    'http://mastweb.stsci.edu/CasOutPut/CSV/'.  
    'UserName' - MAST archive user name.  
    Default is 'EranOfek'.  
    'Drop'   - Drop the table from the MAST archive after  
    retrival. Default is true.  
    'Verbose'- Verbose: true|false. Default is true.  
    Output : - The output file name in which the query results are stored.  
    - The execuation status.  
    - The execuation result string.  
    Note that the actual result is stored in the MAST mydb  
    database.  
    See also: query_casjobs  
    By : Eran O. Ofek                    Jan 2017  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [F,St,Res]=VO.MAST.query_casjobs_mydb('select top 10 ra, dec from photoobj');  
    Reliable: 2  
      
      
      
### VO.MAST.query_casjobs_recur

Query MAST CasJobs service recursively for a box (requires casjobs.jar) Package: VO.MAST Description: Query the MAST CasJobs service recursively for a coordinates in a box (requires casjobs.jar). For non recursive query use


    
    Query MAST CasJobs service recursively for a box (requires casjobs.jar)  
    Package: VO.MAST  
    Description: Query the MAST CasJobs service recursively for a coordinates  
    in a box (requires casjobs.jar). For non recursive query use  
    VO.MAST.query_casjobs.  
    Input  : - Query string or 3 elements query clause of the:  
    select, from, where parts of the query.  
    (e.g., {{'ra','dec'},{'photoobj'},''}).  
    Default is 'select top 10 ra, dec from photoobj'  
    If string start with '@' then assume the query is in a file.  
    If empty use default.  
    * Arbitrary number of ...,key,val,... pairs.  
    The following keywords are available:  
    'Table' - Query table name. Default is 'GALEX_GR6Plus7'.  
    Other options include 'PanSTARRS_DR1','HSCv2',...  
    'CasJobsPath' - The CasJobs java file path  
    The Java binary is available from:  
    http://mastweb.stsci.edu/ps1casjobs/casjobscl.aspx  
    Default is '~/matlab/bin/CasJobs/jar'.  
    The configuration file named 'CasJobs.config'  
    should be located in the same directory.  
    'FormatString' - Format string for query output.  
    If empty, will attempt to use 'f f...'.  
    Default is [].  
    'OutType' - Output type 'AstCat'|'mat'.  
    Default is 'AstCat'.  
    'SortBy' - Sort output by column index or name.  
    Default is [].  
    'OnlyQuery' - A flag indicating if to return only the query.  
    If truen, then the query string will be returned  
    without execuation. Default is false.  
    'BoxCoo' - Optional [MinRA, MaxRA, MinDec, MaxDec] to query.  
    Default is empty [radians].  
    If not empty must also provide 'StrRA' and  
    'StrDec'.  
    'StrRA'  - RA string name. Default is empty.  
    'StrDec' - Dec string name. Default is empty.  
    'NumSubBox' - Recursive coordinate division. Default is 3.  
    'Verbose'- Verbose: true|false. Default is true.  
    Output : - Output catalog  
    - Cell array of column names  
    - Result status.  
    - Result string.  
    - Query string.  
    See also: query_casjobs_mydb  
    By : Eran O. Ofek                    Jan 2017  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Out = VO.MAST.query_casjobs_recur('select ra, dec from photoobj','boxcoo',[0 0.1 0 0.1].*pi./180,'StrRA','ra','StrDec','dec');  
    Out = VO.MAST.query_casjobs_recur('select raMean, decMean from ObjectThin','Table','PanSTARRS_DR1','boxcoo',[0 0.5 0 0.5].*pi./180,'StrRA','raMean','StrDec','decMean');  
    Reliable: 2  
      
      
### VO.MAST.wget_all_ps1_dr1

Prepare a local copy of the PS1-DR1 catalog Package: VO.MAST Example: VO.MAST.wget_all_ps1_dr1(false,0,300000)


    
    Prepare a local copy of the PS1-DR1 catalog  
    Package: VO.MAST  
    Example: VO.MAST.wget_all_ps1_dr1(false,0,300000)  
      
### VO.MAST.wget_hsc_sources

Query sources in the HST source catalog tables Package: VO.MAST Description: Query sources in the HST catalog tables. By default the query is done on a predefined columns in the SExtractor


    
    Query sources in the HST source catalog tables  
    Package: VO.MAST  
    Description: Query sources in the HST catalog tables. By default the  
    query is done on a predefined columns in the SExtractor  
    catalogs of the WFPC2, ACS, and WFPC3.  
    Input  : - J2000.0 RA [radians, sexagesimal string or h,m,s]  
    - J2000.0 Dec [radians, sexagesimal string or sign,d,m,s]  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    'HalfSize'  - Search box half size. Default is 10 arcsec.  
    'HalfSizeUnits' - Search half size units. Default is 'arcsec'.  
    'SearchTable'   - Tables in which to search for sources.  
    Default is: {'Catalog_WFPC2_SourceExtractor','Catalog_ACS_SourceExtractor','Catalog_WFC3_SourceExtractor'};  
    'Columns'       - Columns to retrieve. Default is:  
    {'ALPHA_J2000','DELTA_J2000','X_IMAGE','Y_IMAGE','MAG_APER1','MAG_APER2','MAGERR_APER1','MAGERR_APER2','MU_MAX','A_WORLD','B_WORLD','THETA_WORLD','ELONGATION','CLASS_STAR'};  
    'Table'         - Table to search. Default is 'HSCv1'.  
    Output : - An AstCat object with element per search table.  
    Each element contains the list of sources found in search  
    table.  
    - Cell array of query result string.  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Dec 2017  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [Out,QS]=VO.MAST.wget_hsc_sources(1,1)  
    Reliable: 2  
      
### VO.MAST.wget_ps1_api

Query the PS1 catalog via the web API Package: VO Description: Query the PS1 catalog via the web API See API details in: https://catalogs.mast.stsci.edu/docs/panstarrs.html


    
    Query the PS1 catalog via the web API  
    Package: VO  
    Description: Query the PS1 catalog via the web API  
    See API details in: https://catalogs.mast.stsci.edu/docs/panstarrs.html  
    and https://archive.stsci.edu/panstarrs/  
    Input  : - J2000.0 R.A. [deg].  
    - J2000.0 Dec. [deg].  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    'Radius' - Search radius [deg]. Default is 1./60.  
    'ColCell'- Cell array of column names to retieve.  
    'BaseURL'- Query base URL.  
    'DR'     - Data Release. Default is 'dr2'.  
    'Catalog'- PS1 catalog. Default is 'maen'.  
    'OutType'- Output type. Default is 'astcat'.  
    Output : - PS1 catalog.  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Mar 2019  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Out= VO.MAST.wget_ps1_api(1,1);  
    Reliable: 2  
      
      
      
      
