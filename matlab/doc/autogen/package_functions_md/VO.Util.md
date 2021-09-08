# Package: VO.Util


### VO.Util.cat_band_dictionary

Return the band (filter) name in a given catalog. Package: VO.util Description: Given a catalog name (e.g., 'sdss') and a band name (e.g., 'r'), return the corresponding band in the catalog


    
    Return the band (filter) name in a given catalog.  
    Package: VO.util  
    Description: Given a catalog name (e.g., 'sdss') and a band name  
    (e.g., 'r'), return the corresponding band in the catalog  
    (e.g., 'modelMag_g') its error and color for photometric  
    calibration (e.g., 'modelMag_g - modelMag_r').  
    Input  : - A catalog name (e.g., 'sdss').  
    Available catalog names: 'sdss'  
    - A band name (e.g., 'r').  
    Output : - A string of corresponding band name in the catalog.  
    - A string of corresponding band error name in the catalog.  
    - A string of color formula for photometric calibration.  
    - A string of color error formula for photometric calibration.  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Jul 2017  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [CatBand,CatBandErr,CatColor,CatColorErr]=VO.util.cat_band_dictionary('sdss','g')  
    Reliable: 2  
      
      
### VO.Util.read_casjobs_table

Read SDSS CasJobs table into a matrix or table. Package: VO.util Description: Read SDSS CasJobs table into a matrix or table. If output is a matrix and some columns contains strings than


    
    Read SDSS CasJobs table into a matrix or table.  
    Package: VO.util  
    Description: Read SDSS CasJobs table into a matrix or table.  
    If output is a matrix and some columns contains strings than  
    the unique strings will be replaced by numbers and a  
    dictionary to translate numbers to strings is provided.  
    Input  : - File name containing the SDSS CasJobs table.  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    'OutType' - Options are:  
    'mat' - matrix output. Default.  
    'table' - table output.  
    'EmptyVal'- Empty val to replace with NaNs. Default is 'null'.  
    'RemDuplicate' - Remove duplicate columns. Default is true.  
    Output : - Matrix of table of data.  
    - Cell array of column names.  
    - Column dictionary.  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Jan 2018  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [Mat,ColCell,Dic]=VO.util.read_casjobs_table('sdss_dr14_spec.csv','OutType','table','Delimiter','|');  
    Reliable:  
      
      
      
      
### VO.Util.read_csv_with_header

Read SDSS CasJobs table into a matrix or table. Package: VO.util Description: Read SDSS CasJobs table into a matrix or table. If output is a matrix and some columns contains strings than


    
    Read SDSS CasJobs table into a matrix or table.  
    Package: VO.util  
    Description: Read SDSS CasJobs table into a matrix or table.  
    If output is a matrix and some columns contains strings than  
    the unique strings will be replaced by numbers and a  
    dictionary to translate numbers to strings is provided.  
    Input  : - File name containing the SDSS CasJobs table.  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    'StartKey' - A keyword that indicate the end of the header.  
    If empty (default), then no header.  
    'OutType' - Options are:  
    'mat' - matrix output. Default.  
    'table' - table output.  
    'EmptyVal'- Empty val to replace with NaNs. Default is 'null'.  
    'RemDuplicate' - Remove duplicate columns. Default is true.  
    Output : - Matrix of table of data.  
    - Cell array of column names.  
    - Column dictionary.  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Sep 2019  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [Mat,ColCell]=VO.util.read_csv_with_header('g13_maged_19me15_1m_20130301_20130331.csv','OutType','table','Delimiter',',','StartKey','data:');  
    Reliable:  
      
      
      
### VO.Util.read_votable

XML VO table reader Package: VO.util Description: Simple XML (VO table) reader.


    
    XML VO table reader  
    Package: VO.util  
    Description: Simple XML (VO table) reader.  
    Input  : - Either a cell containing a string containing of file name,  
    a file identifier, or a string containing the VO table string.  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    Output : -  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Sep 2019  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [Table]=VO.util.read_votable({'filename'});  
    [Table]=VO.util.read_votable(FID);  
    [Table]=VO.util.read_votable(String);  
    Reliable: 2  
      
      
    DefV. =  
    InPar = InArg.populate_keyval(DefV,varargin,mfilename);  
      
