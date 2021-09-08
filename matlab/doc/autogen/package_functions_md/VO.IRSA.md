# Package: VO.IRSA


### VO.IRSA.irsa_db_url

Return the URL for IRSA database query. Package: VO.IRSA Description: Return the URL for IRSA database query.


    
    Return the URL for IRSA database query.  
    Package: VO.IRSA  
    Description: Return the URL for IRSA database query.  
    Input  : null  
    Output : - URL.  
    Tested : Matlab R2011b  
    By : Eran O. Ofek                    Sep 2016  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reference: http://irsa.ipac.caltech.edu/applications/Gator/GatorAid/irsa/catsearch.html  
    Example: Url=VO.IRSA.irsa_db_url  
    Reliable: 2  
      
      
### VO.IRSA.query_cat

Query IPAC/IRSA catalog. Package: VO.IRSA Description: Query IPAC/IRSA catalog.


    
    Query IPAC/IRSA catalog.  
    Package: VO.IRSA  
    Description: Query IPAC/IRSA catalog.  
    Input  : - Catalog name (e.g., 'wise_allwise_p3as_psd').  
    - Object name (e.g., 'M81') or coordinates in [H M S],  
    sexagesimal string or radians.  
    - Object declination [Sign D M S], sexagesimal or radians.  
    If empty then assume that the RA contains the object name.  
    - Search radius [arcsec]. Default is 60.  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    'Type'   - Search type 'circ'|'cone'|'box'. Default is 'cone'.  
    'Columns'- Cell array of column names to query.  
    If empty retrieve all columns. Default is empty.  
    Output : -  
    Tested : Matlab R2011b  
    By : Eran O. Ofek                    Sep 2016  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reference: http://irsa.ipac.caltech.edu/applications/Gator/GatorAid/irsa/catsearch.html  
    Example: Cat=VO.IRSA.query_cat('wise_allwise_p3as_psd','M81');  
    Reliable: 2  
      
      
### VO.IRSA.read_ipac_table

Read IPAC/IRSA table format Package: VO.IRSA Description: Read IPAC/IRSA table format from file or string.


    
    Read IPAC/IRSA table format  
    Package: VO.IRSA  
    Description: Read IPAC/IRSA table format from file or string.  
    Input  : - A file name containing an IPAC text table.  
    - File type: {'file','str'}. Default is 'file'.  
    - Convert date to JD {true|false}. Default is true.  
    Output : - A structure containing the following Fields:  
    'CatCell' - A cell array of the table. Each cell per column.  
    'Col' - Astructure of column indices.  
    'ColCell' - A cell array of column names.  
    'ColType' - A cell array of column types.  
    'ColUnit' - A cell array of column units.  
    Reference: http://irsa.ipac.caltech.edu/applications/DDGEN/Doc/ipac_tbl.html  
    Tested : Matlab R2011b  
    By : Eran O. Ofek                    May 2014  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [Table]=VO.IRSA.read_ipac_table('tt');  
    Reliable: 2  
      
### VO.IRSA.wget_all_catnames

Retrieve a list of all IPAC/IRSA public catalogs Package: VO.IRSA Description: Retrieve a list of all IPAC/IRSA public catalogs


    
    Retrieve a list of all IPAC/IRSA public catalogs  
    Package: VO.IRSA  
    Description: Retrieve a list of all IPAC/IRSA public catalogs  
    Input  : null  
    Output : - A cell array of all public catalog names.  
    - A cell array of catalog descriptions.  
    Tested : Matlab R2011b  
    By : Eran O. Ofek                    Sep 2016  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reference: http://irsa.ipac.caltech.edu/applications/Gator/GatorAid/irsa/catsearch.html  
    Example: [AllCat,Desc]=VO.IRSA.wget_all_catnames; [AllCat,Desc]  
    Reliable: 2  
      
      
### VO.IRSA.wget_cat_columns

Retrieve a list of all columns for an IPAC/IRSA public catalog Package: VO.IRSA Description: Given an IPAC/IRSA catalog name (see Catalog.IRSA.wget_all_catnames) retrieve all columns


    
    Retrieve a list of all columns for an IPAC/IRSA public catalog  
    Package: VO.IRSA  
    Description: Given an IPAC/IRSA catalog name  
    (see Catalog.IRSA.wget_all_catnames) retrieve all columns  
    in the catalog.  
    Input  : - Catalog name (e.g., 'wise_allwise_p3as_psd').  
    Use Catalog.IRSA.wget_all_catname to retrieve a list of  
    catalogs.  
    Output : - A structure array containing the column names and information.  
    Tested : Matlab R2011b  
    By : Eran O. Ofek                    Sep 2016  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reference: http://irsa.ipac.caltech.edu/applications/Gator/GatorAid/irsa/catsearch.html  
    Example: [Cols]=VO.IRSA.wget_cat_columns('wise_allwise_p3as_psd');  
    Reliable: 2  
      
      
### VO.IRSA.wget_irsa_coockie

Get IRSA cookie for a user and password Package: VO.IRSA Description: Get IRSA cookie for a user and password


    
    Get IRSA cookie for a user and password  
    Package: VO.IRSA  
    Description: Get IRSA cookie for a user and password  
    Input  : - User name. Default is  
    - Password. Default is  
    - Cookie name. Default is 'ptf.txt'.  
    Output : - String containing cookie name.  
    Example: CookieName=VO.IRSA.wget_irsa_coockie  
    Reliable:  
      
      
