# Package: www


### www.cgibin_parse_query_str

Break a URL parameters query string to parameter names and values. Package: www Description: Break a URL parameters query string to parameter names and values.


    
    Break a URL parameters query string to parameter names and values.  
    Package: www  
    Description: Break a URL parameters query string to parameter names  
    and values.  
    Input  : - URL query string. If empty, then get the query string from  
    the QUERY_STRING environment variable. Default is empty.  
    - decode strings {false|true}. Default is true.  
    - A boolean scalar or vector indicating if the value strings  
    in the structure  
    should be transformed to numerical values. Default is false;  
    Output : - Structure in which the field names represent parameter  
    names, and the field value (content) represent the parameter  
    value.  
    - Cell array in which each cell contains a cell array of the  
    {Par,Value} pair.  
    Tested : Matlab R2014a  
    By : Eran O. Ofek                    Dec 2014  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example:  
    [ST,CT]=www.cgibin_parse_query_str('q=parse+post+get+cgi-bin+matlav&oq=parse+post+get+cgi-bin+matlav');  
    Reliable: 2  
      
      
### www.find_urls

Read the URL content and extract all the links within the URL Package: www Description: Given a URL, read the URL content and extract all the links within the URL and return a cell array of all the links.


    
    Read the URL content and extract all the links within the URL  
    Package: www  
    Description: Given a URL, read the URL content and extract all the links  
    within the URL and return a cell array of all the links.  
    Optionaly, the program can filter URL using regular  
    expressions.  
    Input  : - A URL to read.  
    * Arbitrary number of pairs of input arguments ...,key,val,...  
    The following keywirds are available:  
    'strfind'  - select only links which contains a specific  
    specified string. Default is [].  
    If empty matrix then do not apply this search.  
    'match'    - Use a regular expression matching.  
    If empty matrix then do not apply this search.  
    'input'    - Input type of the first argument:  
    'url' - Assumes that the first input is a URL  
    (default).  
    'file'- Assumes that the first input is an html  
    file.  
    'base'     - Base URL (useful if read from file).  
    Default is empty matrix.  
    'Method'   - Function to use in order to read URL.  
    'webread'|'urlread'.  
    Default is 'webread'.  
    Output : - A cell array of links found within the URL.  
    - A flag indicating if link is a possible directory.  
    Directories are identified by the '/' sign in the end  
    of the link.  
    - A cell array of file names in the URL name (i.e., string after  
    last "/").  
    Tested : Matlab 2011b  
    By : Eran O. Ofek                    Feb 2013  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: List=www.find_urls('http://www.weizmann.ac.il/home/eofek/matlab/');  
    List=www.find_urls(URL,'strfind','.m');  
    List= www.find_urls(URL,'match','http.*?\.m');  
    List= www.find_urls(URL,'match','.*?\.fits');  
    Reliable: 2  
      
      
### www.find_urls_ftp

Find files in a FTP link Package: www Description: Make a list of files in an FTP link.


    
    Find files in a FTP link  
    Package: www  
    Description: Make a list of files in an FTP link.  
    Input  : - FTP URL.  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    'Match' - Regular expression for file matching. If empty, then  
    select all files. Default is [].  
    'Verbose' - Verbose. Default is true.  
    Output : - Cell array of links names.  
    - Cell array of file names.  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Jan 2018  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [List]=www.find_urls_ftp('ftp://archive.noao.edu/public/hlsp/nscdr1/instcal/','Match','fits$');  
    Reliable: 2  
      
      
### www.ftp_dir_list

Return files URLs from FTP containing a file listing. Package: www Description: Given an FTP URL that contains only a file listing, return a cell array of all the files URLs.


    
    Return files URLs from FTP containing a file listing.  
    Package: www  
    Description: Given an FTP URL that contains only a file listing,  
    return a cell array of all the files URLs.  
    Input  : - FTP url.  
    - A flag {true|false} indicating if to recursively search for  
    files in all sub sirectories.  
    If false then will return only files and directories  
    in the current directory.  
    Default is true.  
    Output : - Structure array of full URLs of files listed in FTP.  
    The structure contains the following fields:  
    .URL   - full URL  
    .isdir - is directory flag.  
    .name  - file name  
    .subdir- subdirectory relative to base  
    Tested : Matlab R2014a  
    By : Eran O. Ofek                    Jan 2015  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: FullURL=www.ftp_dir_list('ftp://legacy.gsfc.nasa.gov/chandra/data/science/ao01/cat1/1/primary/')  
    Reliable: 2  
      
### www.html_page

Create an HTML file Package: www Description: Create an HTML file. The file contains the necessery header and footer and a supplied content.


    
    Create an HTML file  
    Package: www  
    Description: Create an HTML file. The file contains the necessery  
    header and footer and a supplied content.  
    Input  : - Output file name.  
    - A string containing file name, or a cell array of strings.  
    The file/strings will be added between the HTML header  
    and footer. If a cell vector is given then each element  
    is regarded as new line (i.e., seperated by '<br>').  
    * Arbitrary pairs of arguments:...keyword,value,...  
    to enable the user to control the web page appereance,  
    where keyword can be one of the following:  
    'PageTitle'  - Page title string.  
    'BgColor'    - HTML background color, defaukt is '#ffffff'.  
    'TextColor'  - HTML text color, defaukt is '#000000'.  
    'LinkColor'  - HTML link color, default is '#0000ff'.  
    'VLinkColor' - HTML vlink color, default is '#ff0000'.  
    'BodyStatment'-HTML (extra) body statment, default is ''.  
    Output : null  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                    Apr 2007  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: www.html_page('Example.html',{'Hello world'},'PageTitle','example');  
    Reliable: 2  
      
      
### www.html_table

Create an HTML table Package: www Description: Given a matlab matrix or cell array create an html page with the matrix in an html table.


    
    Create an HTML table  
    Package: www  
    Description: Given a matlab matrix or cell array create an html  
    page with the matrix in an html table.  
    Input  : - Output file name.  
    * Arbitrary pairs of arguments:...keyword,value,...  
    where keyword can be one of the following:  
    'TableCell'  - Table Cell array.  
    'TableMat'   - Table matrix (dont supply not cell and mat).  
    'TableLink'  - Cell array of table links, default is empty.  
    'TableHead'  - Cell vector of table columns header.  
    'PreTable'   - String of pre table text.  
    'PostTable'  - String of pos ttable text.  
    'PageTitle'  - Page title string.  
    'TableBorder'- Table border number, default is 1.  
    'BgColor'    - HTML background color, defaukt is '#ffffff'.  
    'TextColor'  - HTML text color, defaukt is '#000000'.  
    'LinkColor'  - HTML link color, default is '#0000ff'.  
    'VLinkColor' - HTML vlink color, default is '#ff0000'.  
    'BodyStatment'-HTML (extra) body statment, default is ''.  
    'TableStatment'- Table (extra) statement, default is ''.  
    'HeadStatment' - Cell vector of (extra) statment within <th>.  
    'CellStatment' - Cell array of (extra) statment within <td> (cell content).  
    Output : null  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                    May 2006  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reliable: 2  
      
      
### www.mwget

A wrapper around the wget command Package: www Description: A wrapper around the wget command. Retrieve a URL using the wget command.


    
    A wrapper around the wget command  
    Package: www  
    Description: A wrapper around the wget command. Retrieve a URL using  
    the wget command.  
    OBSOLETE: Use www.pwget instead.  
    Input  : - A string containing a URL.  
    Alternativly this can be a cell array of URLs.  
    However, if this is a cell array than 'rftp' will be set to 0.  
    * Arbitary number of ...,keyword,value,... pairs.  
    Where allowed keywords are:  
    'user' - user  
    'pass' - password  
    'clob' - Clobber {'y'|'n'}. Default is 'n'.  
    No clobber means that if the file exist it will  
    be overwritten.  
    'par'  - string of additional parameters to pass to wget.  
    'out'  - Output file name. If the first inpur parameter  
    is a cell array this must be a cell array too.  
    Output : - Vector of flags [0 | 1] indicating if the program was  
    able to retrive the requested file (0) or failed (1).  
    Tested : Matlab 7.10  
    By : Eran O. Ofek                    May 2010  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    See also: www.pwget.m, www.rftpget.m  
    Example: [Res]=www.mwget('http://heasarc.gsfc.nasa.gov/FTP/swift/data/obs/2008_05/00037562001/xrt/event/sw00037562001xpcw2po_cl.evt.gz');  
    Reliable: 2  
      
### www.parse_html_table

Parse columns from an HTML table into matlab Package: www Description: Parse columns from an HTML table into matlab. The program can not parse tables with colspan parameter


    
    Parse columns from an HTML table into matlab  
    Package: www  
    Description: Parse columns from an HTML table into matlab.  
    The program can not parse tables with colspan parameter  
    different than 1.  
    Input  : - String containing URL name from which to parse the table.  
    Alternatively, this can be a file identifier that contains  
    the HTML table. In this case the user is responsible for  
    opening and closing the file. Or this can be a cell array  
    in which there is one cell that contains the html text.  
    - Number of table in HTML page. This is useful if more than one  
    table is found within HTML page. Default is 1.  
    If two element vector is given then assume the table is nested.  
    In this case the first number indicate the number of the  
    outer table and the second is the number of the nested table.  
    - Convert all data in table to double {'y','n'}, default is 'n'.  
    - Remove anchor links from the table {'y' | 'n'}, default is 'y'.  
    - Output type:  
    'cm' - cell matrix.  
    'cv' - cell vector of cells (default).  
    - Maximum number of retrieves. Default is 1.  
    If >1, then will try again in case of a failure to access  
    the URL.  
    output : - Cell table.  
    - Cell array containing table columns header.  
    Tested : Matlab 7.10  
    By : Eran O. Ofek                    Jun 2010  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example:  
    [Cell,Header]=www.parse_html_table('http://ogle.astrouw.edu.pl/ogle3/ews/2008/ews.html',1,'y','y','cm');  
    Reliable: 2  
      
### www.parse_html_table_old

Parse columns from an HTML table into matlab Package: www Description: Parse columns from an HTML table into matlab. The program can not parse tables with colspan parameter


    
    Parse columns from an HTML table into matlab  
    Package: www  
    Description: Parse columns from an HTML table into matlab.  
    The program can not parse tables with colspan parameter  
    different than 1.  
    Input  : - String containing URL name from which to parse the table.  
    Alternatively, this can be a file identifier that contains  
    the HTML table. In this case the user is responsible for  
    opening and closing the file. Or this can be a cell array  
    in which there is one cell that contains the html text.  
    - Number of table in HTML page. This is useful if more than one  
    table is found within HTML page. Default is 1.  
    If two element vector is given then assume the table is nested.  
    In this case the first number indicate the number of the  
    outer table and the second is the number of the nested table.  
    - Convert all data in table to double {'y','n'}, default is 'n'.  
    - Remove anchor links from the table {'y' | 'n'}, default is 'y'.  
    - Output type:  
    'cm' - cell matrix.  
    'cv' - cell vector of cells (default).  
    - Maximum number of retrieves. Default is 1.  
    If >1, then will try again in case of a failure to access  
    the URL.  
    output : - Cell table.  
    - Cell array containing table columns header.  
    Tested : Matlab 7.10  
    By : Eran O. Ofek                    Jun 2010  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example:  
    [Cell,Header]=www.parse_html_table_old('http://ogle.astrouw.edu.pl/ogle3/ews/2008/ews.html',1,'y','y','cm');  
    Reliable: 2  
      
### www.pwget

Parallel wget to retrieve multiple files simultanously Package: www Description: Parallel wget function designed to retrieve multiple files using parallel wget commands.


    
    Parallel wget to retrieve multiple files simultanously  
    Package: www  
    Description: Parallel wget function designed to retrieve multiple files  
    using parallel wget commands.  
    If fast communication is available, running several wget  
    commands in parallel allows almost linear increase in the  
    download speed.  
    After exceuting pwget.m it is difficult to kill it. In  
    order to stop the execuation while it is running you  
    have to create a file name 'kill_pwget' in the directory  
    in which pwget is running (e.g., "touch kill_pwget").  
    Input  : - Cell array of URL file links to download.  
    Alterantively a URL string.  
    - Additional string to pass to the wget command  
    e.g., '-q'. Default is empty string ''.  
    - Maxium wget commands to run in parallel.  
    Default is 5.  
    - An optional URL base to concatenate to the begining of each  
    link. This is useful if the Links cell array contains only  
    relative positions. Default is empty string ''.  
    If empty matrix then use default.  
    - Use function: 'wget'|'urlwrite'. Default is 'wget'.  
    Output : Original names of retrieved files.  
    Tested : Matlab 2012a  
    By : Eran O. Ofek                    Oct 2012  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Speed  : On my computer in the Weizmann network I get the following  
    results while trying to download 20 corrected SDSS fits images:  
    MaxGet=1  runs in 83 seconds  
    MaxGet=2  runs in 41 seconds  
    MaxGet=5  runs in 19 seconds  
    MaxGet=10 runs in 9 seconds  
    MaxGet=20 runs in 6 seconds  
    Example: tic;www.pwget(Links,'',10);toc  
    sometime wget will do nothing because the URL is untruste - in this case use:  
    www.pwget(Links,'no-check-certificate',10);  
    Reliable: 2  
      
### www.r_files_url

Recursively get links to all files in www directory list. Package: www Description: Recursively get links to all files in www directory list. Given a URL that contains a directory tree with files,


    
    Recursively get links to all files in www directory list.  
    Package: www  
    Description: Recursively get links to all files in www directory list.  
    Given a URL that contains a directory tree with files,  
    get the links to all file names in the directory tree.  
    Input  : - String containing URL.  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    see www.find_urls.m for options.  
    Output : - A cell array of links found within the URL.  
    - A flag indicating if link is a possible directory.  
    Directories are identified by the '/' sign in the end  
    of the link.  
    - A cell array of file names in the URL name (i.e., string after  
    last "/").  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Nov 2017  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example:  
    [LL,IsDir,FN]=www.r_files_url('https://ztfweb.ipac.caltech.edu/ztf/archive/sci/2017/1015/');  
    Reliable: 2  
      
      
### www.rftpget

Recursively retrieve the entire directory tree in an FTP site Package: www Description: A wrapper around the wget command designed to recursively retrieve the entire directory tree in an FTP site.


    
    Recursively retrieve the entire directory tree in an FTP site  
    Package: www  
    Description: A wrapper around the wget command designed to recursively  
    retrieve the entire directory tree in an FTP site.  
    See also: mwget.m, pwget.m, find_urls.m  
    Input  : - A string containing a URL.  
    Alternativly this can be a cell array of URLs.  
    However, if this is a cell array than 'rftp' will be set to 0.  
    * Arbitary number of ...,keyword,value,... pairs.  
    Where allowed keywords are:  
    'user' - user  
    'pass' - password  
    'par'  - string of additional parameters to pass to wget.  
    'out'  - Output file name  
    'rftp' - recursive wget of an FTP site (will get all URLs  
    for all files under a FTP directory.  
    keyword is 0 | 1, default is 0 (no).  
    Note that this option will get only files names,  
    but not the files themselfs.  
    'hidden' - how to deal with hidden files/directories in FTP  
    structure. If 0, then don't retrieve hidden files,  
    if 1 than retrieve hidden files. Default is 0.  
    'rmindex' - rm index.html files at the end if process.  
    0 - don't remove; 1 - remove (default).  
    Output : - In case the URL is an ftp directory return all the  
    directories listed in this URL.  
    - In case the URL is an ftp directory return all the  
    full URLs of the directories listed in this URL.  
    - In case the URL is an ftp directory return all the  
    files listed in this URL.  
    - In case the URL is an ftp directory return all the  
    full URLs of the files listed in this URL.  
    Tested : Matlab 7.10  
    By : Eran O. Ofek                    May 2010  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [DirsList,DirsListURL,FilesList,FilesListURL]=www.rftpget('ftp://legacy.gsfc.nasa.gov/chandra/data/science/ao01/cat1/1/','rftp',1);  
    Reliable: 2  
      
### www.url2url_key_val

SHORT DESCRIPTION HERE Package: www Description:


    
    SHORT DESCRIPTION HERE  
    Package: www  
    Description:  
    Input  : -  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    Output : -  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    May 2018  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example:  
    Reliable:  
      
      
      
### www.write_content_indexhtml

SHORT DESCRIPTION HERE Package: www Description:


    
    SHORT DESCRIPTION HERE  
    Package: www  
    Description:  
    Input  : -  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    Output : -  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Mar 2020  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example:  
    Reliable:  
      
      
      
    DefV. =  
    InPar = InArg.populate_keyval(DefV,varargin,mfilename);  
      
      
