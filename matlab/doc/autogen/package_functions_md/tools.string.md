# Package: tools.string


### tools.string.construct_fullpath

construct_fullpath function                                      General Description: Construct a full path string to a program name, given its name, path or relative path and base path.


    
      
    construct_fullpath function                                      General  
    Description: Construct a full path string to a program name, given  
    its name, path or relative path and base path.  
    Input  : - Program name.  
    - Program path. If startwith '.' then will be used as a relative  
    path (relative to the base path).  
    Otherwise, base path is ignored.  
    - Base path.  
    - Function name that resides in the base path. If given,  
    then will use which_dir.m to return base path.  
    Output : - Program full path and name.  
    Tested : Matlab R2014a  
    By : Eran O. Ofek                    Jan 2015  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: ProgFullPath=construct_fullpath('sex-2.5','../bin/SExtractor/src/','/home/eran/matlab/fun/ImPhot');  
    or:  
    ProgFullPath=construct_fullpath('sex-2.5','../bin/SExtractor/src/',[],'sextractor');  
    will return: '/home/eran/matlab/fun/ImPhot/../bin/SExtractor/src/sex-2.5  
    and if absolute path is required:  
    ProgFullPath=construct_fullpath('sex-2.5','/bin/SExtractor/src/');  
    Reliable: 2  
      
      
### tools.string.construct_keyval_string

construct_keyval_string function                                 General Description: Construct a ...,keyword, value,... string from pairs of parameters.


    
      
    construct_keyval_string function                                 General  
    Description: Construct a ...,keyword, value,... string from pairs of  
    parameters.  
    Input  : * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    The values will be transformed to strings and will be  
    appended after their keyword (e.g., 'c','4',...)  
    Alternatively, this can be a cell array of pairs, or a  
    single string.  
    If a single string then return the string as is.  
    Output : - String of key,val pairs.  
    A "-" will be added before eack keyword string.  
    Tested : Matlab R2014a  
    By : Eran O. Ofek                    Jan 2015  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: KVstr=construct_keyval_string('c 4 m first');  
    KVstr=construct_keyval_string({'c',4,'m','first'});  
    KVstr=construct_keyval_string('c',4,'m','first');  
    Reliable: 2  
      
      
### tools.string.find_strcmpi

find_strcmpi function                                            General Description: find(strcmpi(varargin{:})) function. I.e., like strcmpi.m but returning the indices of true.


    
      
    find_strcmpi function                                            General  
    Description: find(strcmpi(varargin{:})) function. I.e., like strcmpi.m  
    but returning the indices of true.  
    Input  : * See strcmpi.m for options.  
    Output : - Indices.  
    Tested : Matlab R2011b  
    By : Eran O. Ofek                    Mar 2014  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: find_strcmpi({'a','b'},'a');  
    Reliable: 1  
      
      
      
### tools.string.read_date

read_date function                                                     ephem Description: Given a string or cell array of strings containing dates and time in an arbitrary format, convert it to a matrix of dates


    
      
    read_date function                                                     ephem  
    Description: Given a string or cell array of strings containing dates and  
    time in an arbitrary format, convert it to a matrix of dates  
    and a vector of JDs.  
    Input  : - String or cell array of strings containing dates and time.  
    - Cell array containing three columns:  
    {start column, end column, format string}.  
    Example: {1,5,'d'; 7,15,'f'}.  
    Each one of the rows correspinds to one part of the date string.  
    - Description of each one (by order) of the parts of the date/time  
    string. This is a cell array in which each cell contain  
    a string corresponding to one of the parts of the string format.  
    Options are: 'Year', 'MonthName', 'MonthNameShort', 'Month', 'Day'  
    'Frac', 'Hour', 'Min', 'Sec'.  
    Example: {'Year','MonthNameShort','Day','Hour','Min'}.  
    Output : - Matrix of dates [D M Y FracDay]  
    - Vector of JD.  
    Example: [Date,JD]=read_date(Data{1},...  
    {1,4,'d'; 6,8,'s'; 10, 11, 'd'; 13, 14, 'd'; 16,17,'d'},...  
    {'Year','MonthNameShort','Day','Hour','Min'});  
    will read string with the format: '2000-Feb-01 00:00'.  
    Tested : Matlab 7.3  
    By : Eran O. Ofek                    August 2008  
    URL : http://wise-obs.tau.ac.il/~eran/matlab.html  
      
### tools.string.read_str_formatted

Read a string which is formatted by specidc column positions Package: Util.string


    
    Read a string which is formatted by specidc column positions  
    Package: Util.string  
      
    Description: Read text/data string with constant format (i.e., columns  
    at specfied locations). The start and end  
    column for each entry should be specified.  
    Input  : - Either a string containing a file content, or a cell array  
    in which each element contains a line from the file.  
    - Cell array containing three columns, specifing the file  
    structure:  
    {start column, end column, format string}.  
    Example: {1,5,'d'; 7,15,'f'}.  
    * Arbitrary number of pairs of arguments {...,key,val,...},  
    Where the following keyword are allowed:  
    'skip'   - number of lines to skip, default is 0.  
    'comment'- comment character, or a cell array of comments  
    characters. Default is ''.  
    Output : - Cell array containing data.  
    See also: read_formatted.m  
    Tested : Matlab R2011b  
    By : Eran O. Ofek                    May 2014  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Data=Util.string.read_str_formatted(String,{1,5,'d'; 7,15,'f'});  
    Reliable: 2  
      
      
      
### tools.string.spacedel

recursively delete all spaces from a string. Package: tools.string Description: Given a string, recursively delete all spaces.


    
    recursively delete all spaces from a string.  
    Package: tools.string  
    Description: Given a string, recursively delete all spaces.  
    Input  : - A string, or a cell array of strings.  
    Output : - A trimmed string.  
    Tested : Matlab 7.13  
    By : Eran O. Ofek                    Apr 2013  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    See also: spacetrim.m  
    Example: spacedel('a   a ');  
    spacedel({'a   a ';' bbbaj   a'});  
    Reliable: 2  
    -  
      
### tools.string.spacetrim

Recursively replace any occurance of two spaces with a single space. Package: tools.string Description: Given a string, recursively replace any occurance of two spaces with a single


    
    Recursively replace any occurance of two spaces with a single space.  
    Package: tools.string  
    Description: Given a string, recursively replace any occurance of two  
    spaces with a single  
    space, such that the final product is a string with a  
    single spaces between words.  
    Input  : - A string.  
    - A special charachter (default is ' ').  
    Output : - A trimmed string.  
    Tested : Matlab 7.13  
    By : Eran O. Ofek                    Aug 2012  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    See also: spacedel.m  
    Example: tools.string.spacetrim({'aa    a','ysys  a a'});  
    tools.string.spacetrim('aa    a');  
    Reliable: 2  
      
      
      
### tools.string.str2double_check

str2double_check function                                        General Description: Convert strings to doubles, but unlike str2double if the input is a number will return the number (instead of NaN).


    
      
    str2double_check function                                        General  
    Description: Convert strings to doubles, but unlike str2double if the  
    input is a number will return the number (instead of NaN).  
    Input  : - String or cell array of strings.  
    Output : - Strings converted to numbers.  
    Tested : Matlab R2014a  
    By : Eran O. Ofek                    Dec 2014  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Num=str2double_check('33');  
    Num=str2double_check({'33',44});  
    Reliable: 2  
      
      
### tools.string.str2num_nan

str2num_nan function                                             General Description: Convert string to number, and return NaN if not a number or empty.


    
      
    str2num_nan function                                             General  
    Description: Convert string to number, and return NaN if not a number  
    or empty.  
    Input  : - String or cell array of strings.  
    Output : - Number or matrix of numbers (if input is a cell array).  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                    Jun 2007  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Bug    : If input contains a number followed by a string (e.g., '12a'),  
    then the string part will be ignored.  
    Example: str2num_nan({[],1,'11.2';'a','b','15.1'})  
    str2num_nan('166.1')  
    str2num_nan('A')  
    Reliable: 2  
      
      
### tools.string.str_duplicate

Duplicate a string multiple times. package: Util.string Description: Duplicate a string multiple times.


    
    Duplicate a string multiple times.  
    package: Util.string  
    Description: Duplicate a string multiple times.  
    Input  : - The string to duplicate.  
    - Number of times to duplicate the string.  
    - Additional string to add at the end of the output string.  
    Default is ''.  
    Output : - Duplicated string.  
    Tested : Matlab R2011b  
    By : Eran O. Ofek                    Nov 2013  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: NewStr=str_duplicate('f,',5,'f\n');  
    Reliable: 2  
      
      
### tools.string.strcmp_cell

strcmp_cell function                                             General Description: Given two cell arrays of strings, check if each one of the strings in the first cell array exist in the second cell


    
      
    strcmp_cell function                                             General  
    Description: Given two cell arrays of strings, check if each one of the  
    strings in the first cell array exist in the second cell  
    array.  
    Input  : - A cell array of strings (or a single string).  
    Each one of the strings in this cell array will be compared  
    with all the strings in the second cell array.  
    - A cell array of strings.  
    Output : - A vector of flag of the length of first cell array which  
    indicate for each string in the first cell if it exist  
    in the second cell array.  
    License: GNU general public license version 3  
    Tested : Matlab R2014a  
    By : Eran O. Ofek                    May 2015  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example:  Flag=strcmp_cell({'A','B','C'},{'A','AA','B','E'})  
    Reliable: 2  
      
      
### tools.string.strdouble2double

- strdouble2double function                                           General Description: Convert string, souble or any data type to double. Unlike str2double.m, this script doesn't return NaN if


    
    -  
    strdouble2double function                                           General  
    Description: Convert string, souble or any data type to double.  
    Unlike str2double.m, this script doesn't return NaN if  
    the input is already a double.  
    Input  : - String or a number or a cell array of strings or numbers.  
    Output : - The input in double format - will convert cell to matrix.  
    Tested : Matlab 7.10  
    By : Eran O. Ofek                   October 2010  
    URL : http://wise-obs.tau.ac.il/~eran/matlab.html  
    -  
      
### tools.string.stringSearchFun

Return a function handle for a string search (i.e., strcmp, strcmpi, regexp, regexpi


    
    Return a function handle for a string search (i.e., strcmp, strcmpi, regexp, regexpi  
    Input  : - A logical indicating if to use regular expression (true) or  
    strcmp (false).  
    - A logical indicating if to use case sensetive search (true) or  
    not (false).  
    Output : - A function handle @(str1, str2)  
    Author : Eran Ofek (Apr 2021)  
    Example: SearchFun = tools.string.stringSearchFun(true, true)  
### tools.string.strlines2cell

strlines2cell function                                           General Description: Given a string with end of line characters, break the string into a cell array in which each cell contains


    
      
    strlines2cell function                                           General  
    Description: Given a string with end of line characters, break the  
    string into a cell array in which each cell contains  
    a line.  
    Input  : - A string.  
    Output : - A cell array of lines.  
    Tested : Matlab R2011b  
    By : Eran O. Ofek                    May 2014  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: CellLines=strlines2cell(String)  
    Reliable: 2  
      
      
