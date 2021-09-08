# Package: io.files


### io.files.catfile

Concatenate files into a single file. Package: Util.files Description: Concatenate files into a single file. Add a carriage return at the end of each Concatenated file.


    
    Concatenate files into a single file.  
    Package: Util.files  
    Description: Concatenate files into a single file. Add a carriage  
    return at the end of each Concatenated file.  
    Input  : - Output file name.  
    * Arbitrary number of strings containing files name.  
    Alternativel, a cell array containing files name.  
    Output : null  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                    Apr 2007  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reliable: 2  
      
      
### io.files.copy_files_from_dirtree

Copy or movde all files recursively in a directory tree. Package: Util.files Description: Given a location (or the present working directory), look for all files in the subdirectories and copy them to the main


    
    Copy or movde all files recursively in a directory tree.  
    Package: Util.files  
    Description: Given a location (or the present working directory), look  
    for all files in the subdirectories and copy them to the main  
    directory.  
    Input  : - Parent directory. Default is the present working directory.  
    - Directory to copy the data to. Default is the present working  
    directory.  
    - CopyType: 'move' - use move instead of copy.  
    default is 'copy'.  
    Output : null  
    Tested : Matlab 7.13  
    By : Eran O. Ofek                    Jun 2012  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reliable: 1  
      
      
### io.files.create_list

Create a file and a ell array containing a list of files. Package: io.files Description: Create a file and a cell array containing a list of files. The list is created from a cell array, or file name


    
    Create a file and a ell array containing a list of files.  
    Package: io.files  
    Description: Create a file and a cell array containing a list of files.  
    The list is created from a cell array, or file name  
    with wildcards.  
    Input  : - File listing can be one of the followings:  
    (1) A cell vector containing a file name in each cell.  
    (2) A string containing wild cards (i.e., '*' or '?'),  
    in this case, the program will produce a list of  
    all matched files in the current directory.  
    The program uses superdir, so expressions like  
    'lred00[75-82].fits' are legal.  
    (3) A string containing a file name (the file name not  
    necesserly exist).  
    (4) A string begining with '@' containing a file name  
    that contains a list of file (one per line).  
    (5) A matrix or a cell array of matrices.  
    In this case the output in ListCell will be a cell  
    array containing the matrices.  
    - Optional output file name, default is empty (i.e., []).  
    If empty, then create a temporary file name for output.  
    If NaN, than do not create file.  
    - Verify {'y'|'n'} if each one of the files listed in  
    the list exist in the current directory, default is 'n'.  
    If the file doesnot exist then donot print it to the  
    final output list.  
    Output : - File name containing the list of files (if verify set to 'y',  
    then this contains only existing files).  
    - A cell array containing the list of files (all files,  
    existing or not).  
    - A vector containing flags indicating if each file in  
    the original list is exist (1) or is missing (0).  
    If the veify option is set to 'n', then return an empty  
    vector.  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                    Apr 2007  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [F,C]=create_list({'A','B'});  
    [F,C,E]=create_list({'A','B'},[],'y');  
    [~,C]=create_list('lred00[75-82].fits',NaN);  
    [~,C]=create_list('@file',NaN);  
    [~,C]=create_list('A*.fits',NaN);  
    Reliable: 1  
      
### io.files.delete_cell

Delete a list of files listed in a cell array. Package: Util.files Description: Delete a list of files listed in a cell array.


    
    Delete a list of files listed in a cell array.  
    Package: Util.files  
    Description: Delete a list of files listed in a cell array.  
    Input  : - Cell array containing file names to delete.  
    Output : *  
    Tested : Matlab R2014a  
    By : Eran O. Ofek                    Jan 2015  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: delete({'try.conv','try.param'})  
    Reliable: 2  
      
      
### io.files.dir_cell

dir like command for a cell of file names. Package: Util.files Description: dir like command for a cell of file names.


    
    dir like command for a cell of file names.  
    Package: Util.files  
    Description: dir like command for a cell of file names.  
    Input  : - A cell array of file names.  
    Output : - Dir output.  
    License: GNU general public license version 3  
    Tested : Matlab R2015b  
    By : Eran O. Ofek                    Sep 2016  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: A=Util.files.dir_cell({'GI5_048014_GAMA12_13209-nd-rrhr.fits','GI5_048014_GAMA12_13209-nd-cnt.fits'})  
    Reliable: 2  
      
      
### io.files.file2str

Read the content of a file into a string or cell vector. Package: Util.IO Description: Read the content of a file into a string or cell vector (line per element).


    
    Read the content of a file into a string or cell vector.  
    Package: Util.IO  
    Description: Read the content of a file into a string or cell vector  
    (line per element).  
    Input  : - File name, cell vector of file names, or a vector of  
    file identifiers.  
    If file identifiers are given, the files will not be closed  
    at the end.  
    If cell vector is given, then each file in each cell will  
    be concatenated.  
    - Read type:  
    'str'  - Read the file including the line breaks, into a  
    single string (default).  
    'cell' - Read each line in the file (without the line  
    break), into a cell vector (line per element).  
    Output : - String or cell vector containing file content.  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                    Apr 2007  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reliable: 1  
      
### io.files.filelist

Generate a cell array array of files list from file name/regular expression Package: @imUtil.util


    
    Generate a cell array array of files list from file name/regular expression  
    Package: @imUtil.util  
    Input  : - A file name, a file name containing wild  
    cards or regular expression, a cell array of  
    file names, or a structure arrawy which is the  
    output of the dir command.  
    - A logical indicating if to use regular expression (true) or  
    wild cards (false). Default is false.  
    Output : - A cell array of file names.  
    Author : Eran Ofek (Apr 2020)  
    Example: List=io.files.filelist('\w*.fits',true);  
      
### io.files.files_arrived

Check if all files in a list arrived to disk (i.e., size not increasing). Package: io.files Description: Check if all files in a list arrived to disk. This is done by checking that the file size does not increase with time.


    
    Check if all files in a list arrived to disk (i.e., size not increasing).  
    Package: io.files  
    Description: Check if all files in a list arrived to disk. This is done  
    by checking that the file size does not increase with time.  
    Input  : - Cell array of files to check.  
    Output : null. Return when file sizes converged.  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    May 2017  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: io.files.files_arrived({'files_arrived'});  
    Reliable: 2  
      
      
### io.files.files_by_date

Select files by date Package: Util.files


    
    Select files by date  
    Package: Util.files  
    Input  : - Files string  
    - Minimum date in any format acceptable by  
    celestial.time.julday.  
    - Maximum date.  
    Output : - Selected files  
    By : Eran O. Ofek                    Feb 2017  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Dir=Util.files.files_by_date('*.hdf5',[1 1 2000],[2 2 2017]);  
    Reliable:  
      
### io.files.for_each_file

Execute a function on a list of files. Package: Util.files Description: Given a file name containing list of files, load each file into a matrix and execute a function with the loaded


    
    Execute a function on a list of files.  
    Package: Util.files  
    Description: Given a file name containing list of files, load each file  
    into a matrix and execute a function with the loaded  
    matrix as a paramter.  
    Input  : - File name containing a list of files to load.  
    Ignore lines begining with # or .  
    - Function to execute for each file that is being loaded.  
    - SaveType:  
    'c'   - Save the output from the executed function in a cell  
    array, default.  
    'm'   - Save the output from the executed function in a matrix,  
    each line per each execuation (assuming the output is  
    a raw vector.  
    * Additional aribitrary number of argument to pass to the  
    function to execute.  
    Output : - Cell array or matrix containing the output of executing  
    the function on each file.  
    - Cell array of all the individual file names in the input file.  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                    Dec 2005  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reliable: 2  
    -  
### io.files.fpf

Easy to use fprintf, with automatic formatting. Package: Util.IO Description: Easy to use fprintf, with automatic formatting. This function is similar to fprintf, but (i) open and close the file


    
    Easy to use fprintf, with automatic formatting.  
    Package: Util.IO  
    Description: Easy to use fprintf, with automatic formatting. This function  
    is similar to fprintf, but (i) open and close the file  
    automaticaly; (ii) in case that format string  
    is not given then the function try to select a format  
    string automaticaly based on the precision of each number.  
    Input  : - File name to write the data into, or file identifier  
    (in this case it is the user responsibility to open  
    and close the file).  
    - Data matrix.  
    - Format string (e.g., '8.3f 6.3f 5.3f\n').  
    In case format string is not given or empty, then the format  
    string is constructed by estimating a resnoable format.  
    - Control sequence [MaxDexForFloat,  
    MaxDexForInteger,  
    NumberOfBlanks,  
    Mentisa,  
    Digit];  
    where:  
    MaxDexForFloat     - calculating the log10 of the ratio  
    between the biggest and smallest  
    number (=order). If the 'order is  
    larger than MaxDexForFloat use  
    exponent number representation,  
    otherwise use float or integer.  
    Default is 4.  
    MaxDexForInteger   - If the column contains integers  
    and the 'order' is smaller than  
    MaxDexForInteger then use integer  
    representation, otherwise use  
    exponent.  
    Default is 6.  
    NumberOfBlanks     - Number of blanks between columns.  
    Default is 1.  
    Mentisa            - Default number of digits in mentisa.  
    Default is 8.  
    Digit              - Default number of digits after the  
    digit.  
    Default is 5.  
    Output : - The format string used.  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Mar 2004  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reliable: 2  
      
### io.files.fprintf_cell

An fprintf command for a cell vector. Package: Util.IO Description: An fprintf command for a cell vector.


    
    An fprintf command for a cell vector.  
    Package: Util.IO  
    Description: An fprintf command for a cell vector.  
    Input  : - File identifier or string containing file name to open and  
    close at the end.  
    If empty matrix (i.e., []), then return a string containing  
    the cell contents, but donot write a file.  
    - Print format (e.g., 's\n').  
    - Cell vector to print.  
    Output : - A string containing the cell contents.  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                    Apr 2007  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Util.IO.fprintf_cell('File.dat','s\n',{'first line','second line'});  
    Reliable: 2  
      
### io.files.gunzip

Execute gunzip on files, or a cell array of files Package: AstroUtil.files Description: Execute the gunzip system command (file uncompression) on files, or a list of files.


    
    Execute gunzip on files, or a cell array of files  
    Package: AstroUtil.files  
    Description: Execute the gunzip system command (file uncompression) on  
    files, or a list of files.  
    Input  : - String containing a file name, file name with wild cards or  
    ranges, a file (starting with @) containing a list of files,  
    or a cell array of file names. See create_list for options.  
    - Additional arguments to pass to the gunzip command.  
    Default is ''.  
    Output : - Cell array of compressed file names (i.e., '.gz' extension  
    removed).  
    By : Eran O. Ofek                    Oct 2016  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: AstroUtil.files.gzip('*.fits');  
    Reliable: 2  
      
      
### io.files.gzip

Execute gzip on files, or a cell array of files Package: AstroUtil.files Description: Execute the gzip system command (file compression) on files, or a list of files.


    
    Execute gzip on files, or a cell array of files  
    Package: AstroUtil.files  
    Description: Execute the gzip system command (file compression) on files,  
    or a list of files.  
    Input  : - String containing a file name, file name with wild cards or  
    ranges, a file (starting with @) containing a list of files,  
    or a cell array of file names. See create_list for options.  
    - Additional arguments to pass to the gzip command.  
    Default is ''.  
    Output : - Cell array of compressed file names (i.e., '.gz' extension  
    added).  
    By : Eran O. Ofek                    Oct 2016  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: AstroUtil.files.gzip('*.fits');  
    Reliable: 2  
      
      
### io.files.ini2struct

Author: Andriy Nych ( nych.andriy@gmail.com ) Version:        733341.4155741782200


    
      
    Author: Andriy Nych ( nych.andriy@gmail.com )  
    Version:        733341.4155741782200  
      
      
    INI = ini2struct(FileName)  
      
    This function parses INI file FileName and returns it as a structure with  
    section names and keys as fields.  
      
    Sections from INI file are returned as fields of INI structure.  
    Each fiels (section of INI file) in turn is structure.  
    It's fields are variables from the corresponding section of the INI file.  
      
    If INI file contains "oprhan" variables at the beginning, they will be  
    added as fields to INI structure.  
      
    Lines starting with ';' and '#' are ignored (comments).  
      
    See example below for more information.  
      
    Usually, INI files allow to put spaces and numbers in section names  
    without restrictions as long as section name is between '[' and ']'.  
    It makes people crazy to convert them to valid Matlab variables.  
    For this purpose Matlab provides GENVARNAME function, which does  
    "Construct a valid MATLAB variable name from a given candidate".  
    See 'help genvarname' for more information.  
      
    The INI2STRUCT function uses the GENVARNAME to convert strange INI  
    file string into valid Matlab field names.  
      
    [ test.ini ]~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
      
    SectionlessVar1=Oops  
    SectionlessVar2=I did it again ;o)  
    [Application]  
    Title = Cool program  
    LastDir = c:\Far\Far\Away  
    NumberOFSections = 2  
    [1st section]  
    param1 = val1  
    Param 2 = Val 2  
    [Section #2]  
    param1 = val1  
    Param 2 = Val 2  
      
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
      
    The function converts this INI file it to the following structure:  
      
    [ MatLab session (R2006b) ]~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
    >> INI = ini2struct('test.ini');  
    >> disp(INI)  
    sectionlessvar1: 'Oops'  
    sectionlessvar2: 'I did it again ;o)'  
    application: [1x1 struct]  
    x1stSection: [1x1 struct]  
    section0x232: [1x1 struct]  
      
    >> disp(INI.application)  
    title: 'Cool program'  
    lastdir: 'c:\Far\Far\Away'  
    numberofsections: '2'  
      
    >> disp(INI.x1stSection)  
    param1: 'val1'  
    param2: 'Val 2'  
      
    >> disp(INI.section0x232)  
    param1: 'val1'  
    param2: 'Val 2'  
      
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
      
    NOTE.  
    WhatToDoWithMyVeryCoolSectionAndVariableNamesInIniFileMyVeryCoolProgramWrites?  
    GENVARNAME also does the following:  
    "Any string that exceeds NAMELENGTHMAX is truncated". (doc genvarname)  
    Period.  
      
    =  
### io.files.inifile

INIFILE Creates, reads, or writes data from/to a standard ini (ascii) file. Such a file is organized into sections ([section name]), subsections(enclosed by {subsection name}), and keys (key=value).  Empty lines and lines with the first non-empty


    
    INIFILE Creates, reads, or writes data from/to a standard ini (ascii)  
    file. Such a file is organized into sections  
    ([section name]), subsections(enclosed by {subsection name}),  
    and keys (key=value).  Empty lines and lines with the first non-empty  
    character being ; (comment lines) are ignored.  
      
    Usage:  
    INIFILE(fileName,'new')  
    Rewrites an existing file - creates a new, empty file.  
      
    INIFILE(fileName,'write',keys,<style>)  
    Writes keys given as cell array of strings (see description of  
    the keys below). Optional style variable: 'tabbed' writes sections,  
    subsections and keys in a tabbed style to get more readable  
    file. The 'plain' style is the default style. This only affects  
    the keys that will be written/rewritten.  
      
    INIFILE(fileName,'deletekeys',keys)  
    Deletes keys and their values - if they exist.  
      
    [readsett,result] = INIFILE(fileName,'read',keys)  
    Reads the values of the keys where readsett is a cell array of  
    strings and/or numeric values of the keys. If any of the keys  
    is not found, the default value is returned (if given in the  
    5-th column of the keys parameter). result is a cell array of  
    strings - one for each key read; empty if OK, error/warning  
    string if error; in both cases an empty string is returned in  
    readsett{i} for the i-th key if error.  
      
    [keys,sections,subsections] = INIFILE(fName,'readall')  
    Reads entire file and returns all the sections, subsections  
    and keys found.  
      
      
    Notes on the keys cell array given as an input parameter:  
    Cell array of STRINGS; either 3, 4, or 5 columns.  
    Each row has the same number of columns. The columns are:  
    'section':      section name string (the root is considered if  
    empty)  
    'subsection':   subsection name string (the root is considered  
    if empty)  
    'key':          name of the field to write/read from (given as  
    a string).  
    value:          (optional) STRING or NUMERIC value (scalar or  
    matrix) to be written to the  
    ini file in the case of 'write' operation OR  
    conversion CHAR for read operation:  
    'i' for integer, 'd' for double, 's' or  
    '' or not given for string (default).  
    defaultValue:   (optional) string or numeric value (scalar or  
    matrix) that is returned when the key is not  
    found or an empty value is found  
    when reading ('read' operation).  
    If the defaultValue is not given and the key  
    is not found, an empty value is returned.  
    It MUST be in the format as given by the  
    value, e.g. if the value = 'i' it must be  
    given as an integer etc.  
      
      
    EXAMPLE:  
    Suppose we want a new ini file, test1.ini with 4 fields, including a  
    5x5 matrix (see below). We can write the 5 fields into the ini file  
    using:  
      
    x = rand(5);     matrix data  
    inifile('test1.ini','new');  
    writeKeys = {'measurement','person','name','Primoz Cermelj';...  
    'measurement','protocol','id',1;...  
    'application','','description.m1','some...';...  
    'application','','description.m2','some...';...  
    'data','','x',x};  
    inifile('test1.ini','write',writeKeys,'plain');  
      
    Later, you can read them out. Additionally, if any of them won't  
    exist, a default value will be returned (if the 5-th column is given  
    for all the rows as below).  
      
    readKeys = {'measurement','person','name','','John Doe';...  
    'measurement','protocol','id','i',0;...  
    'application','','description.m1','','none';...  
    'application','','description.m2','','none';...  
    'data','','x','d',zeros(5)};  
    readSett = inifile('test1.ini','read',readKeys);  
      
    Or, we can just read all the keys out  
    [keys,sections,subsections] = inifile(test1.ini,'readall');  
      
      
    NOTES: If the operation is 'write' and the file is empty or does not  
    exist, a new file is created. When writing and if any of the section  
    or subsection or key does not exist, it creates (adds) a new one.  
    Everything but value is NOT case sensitive. Given keys and values  
    will be trimmed (leading and trailing spaces will be removed).  
    Any duplicates (section, subsection, and keys) are ignored. Empty  
    section and/or subsection can be given as an empty string, '',  
    but NOT as an empty matrix, [].  
      
    Numeric matrices can be represented as strings in one of the two form:  
    '1 2 3;4 5 6' or '1,2,3;4,5,6' (an example).  
      
    Comment lines starts with ; as the first non-empty character but  
    comments can not exist as a tail to a standard, non-comment line as ;  
    is also used as a row delimiter for matrices.  
      
    This function was tested on the win32 platform only but it should  
    also work on Unix/Linux platforms. Since some short-circuit operators  
    are used, at least Matlab 6.5 (R13) is required.  
      
      
    First release on 29.01.2003  
    (c) Primoz Cermelj, Slovenia  
    Contact: primoz.cermelj@gmail.com  
    Download location: http://www.mathworks.com/matlabcentral/fileexchange/loadFile.do?objectId=2976&objectType=file  
      
    Version: 1.4.2  
    Last revision: 12.01.2007  
      
    Bug reports, questions, etc. can be sent to the e-mail given above.  
      
    ACKNOWLEDGEMENTS: Thanks to Diego De Rosa for a suggestion/fix how to  
    read the value when the key is found but empty.  
      
      
      
    INIFILE history  
      
      
    [v.1.4.2] 12.01.2007  
    - FIX: When in read mode and a certain key is found but the value is  
    empty, the default value will be used instead.  
      
    [v.1.4.1] 12.01.2006  
    - FIX: Some minor refinements (speed,...)  
      
    [v.1.4.0] 05.12.2006  
    - NEW: New 'readall' option added which reads all the sections,  
    subsections and keys out  
      
    [v.1.3.2 - v.1.3.5] 25.08.2004  
    - NEW: Speed improvement for large files - using fread and fwrite instead  
    of fscanf and fprintf, respectively  
    - NEW: Some minor changes  
    - NEW: Writing speed-up  
    - NEW: New-line chars are properly set for pc, unix, and mac  
      
    [v.1.3.1] 04.05.2004  
    - NEW: Comment lines are detected and thus ignored; comment lines are  
    lines with first non-empty character being ;  
    - NEW: Lines not belonging to any of the recognized types (key, section,  
    comment,...) raise an error.  
      
    [v.1.3.0] 21.04.2004  
    - NEW: 2D Numeric matrices can be read/written  
    - FIX: Bug related to read operation and default value has been removed  
      
    [v.1.2.0] 30.04.2004  
    - NEW: Automatic conversion capability (integers, doubles, and strings)  
    added for read and write operations  
      
    [v.1.1.0] 04.02.2004  
    - FIX: 'writetext' option removed (there was a bug previously)  
      
    [v.1.01b] 19.12.2003  
    - NEW: A new concept - multiple keys can now be read, written, or deleted  
    ALL AT ONCE which makes this function much faster. For example, to  
    write 1000 keys, using previous versions it took 157 seconds on a  
    1.5 GHz machine, with this new version it took only 0.9 seconds.  
    In general, the speed improvement is greater when a larger number of  
    read/written keys is considered (with respect to the older version).  
    - NEW: The format of the input parameters has changed. See above.  
      
    [v.0.97] 19.11.2003  
    - NEW: Additional m-function, strtrim, is no longer needed  
      
    [v.0.96] 16.10.2003  
    - FIX: Detects empty keys  
      
    [v.0.95] 04.07.2003  
    - NEW: 'deletekey' option/operation added  
    - FIX: A major file refinement to obtain a more compact utility ->  
    additional operations can "easily" be implemented  
      
    [v.0.91-0.94]  
    - FIX: Some minor refinements  
      
    [v.0.90] 29.01.2003  
    - NEW: First release of this tool  
      
      
      
### io.files.list_fun_in_package

Find all functions in a matlab package. Package: Util.files Description: Find all functions in a matlab package.


    
    Find all functions in a matlab package.  
    Package: Util.files  
    Description: Find all functions in a matlab package.  
    Input  : - Package name  
    E.g., '+AstroUtil/+spec' or 'AstroUtil.spec'.  
    Output : - A structure containing the directory path and file content.  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    May 2017  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: DirS=io.files.list_fun_in_package('AstroUtil.spec')  
    Reliable: 2  
      
      
### io.files.load2

Load a mat file into a variable Package: Util.IO Description: load a mat file containing a single variable to a variable name (rather than a structure, like load.m).


    
    Load a mat file into a variable  
    Package: Util.IO  
    Description: load a mat file containing a single variable to a variable  
    name (rather than a structure, like load.m).  
    If multiple variables are returned then will behave like  
    load.m  
    Input  : - Mat file name.  
    * Additional parameters to pass to the load.m function.  
    Output : - Variable name.  
    Tested : Matlab 7.13  
    By : Eran O. Ofek                    Jan 2011  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reliable: 2  
      
      
      
### io.files.load_check

Load, but check if variable exist in workspace. Package: Util.IO Description: Load a matlab variable or file from disk (similar to the load.m command). However, before the variable is loaded the


    
    Load, but check if variable exist in workspace.  
    Package: Util.IO  
    Description: Load a matlab variable or file from disk (similar to the  
    load.m command). However, before the variable is loaded the  
    function checks if the variable with name identical to the  
    file name is already exist in the matlab main workspace.  
    If it is exist it will copy  
    the variable from the workspace.  
    If variable does not exist it will load it in the usual way  
    and store it in the main workspace.  
    This is usefull when you want to load big variables  
    in multiple function calles.  
    Input  : - String of file name to load  
    - Store the variable in the main work space, if doesn't exist,  
    {true|false}. Default is true.  
    - Workspace {'base'|'caller'}, Default is 'base'.  
    Output : - Loaded variable.  
    Tested : Matlab 7.11  
    By : Eran O. Ofek                    May 2011  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Var=Util.IO.load_check('Tycho2.mat');  
    Reliable: 1  
      
      
### io.files.load_from_zip

Extract and load files from a zip file. Package: Util.IO Description: Extract and load files from a zip file.


    
    Extract and load files from a zip file.  
    Package: Util.IO  
    Description: Extract and load files from a zip file.  
    Input  : - Zip file name.  
    - A cell array of files to extract and load from the zip file.  
    Output : - A cellay array of the loaded files.  
    Tested : Matlab R2011b  
    By : Eran O. Ofek                    May 2014  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example:  
    Reliable:  
      
      
### io.files.loadh

Load a matrix from HDF5 file. Package: Util.IO Description: Load a matrix from HDF5 file. If dataset name is not provided than will read all


    
    Load a matrix from HDF5 file.  
    Package: Util.IO  
    Description: Load a matrix from HDF5 file.  
    If dataset name is not provided than will read all  
    datasets into a structure. This function doesn't support  
    groups.  
    This is becoming faster than matlab (2014a) for matices with  
    more than ~10^4 elements.  
    Input  : - File name.  
    - variable name (dataset). If empty or not provided than will  
    attempt to read all datasets.  
    - Get attribute. If empty then do not get attributes.  
    'h' - store attributes in an HEAD object.  
    's' - store attributes in a structure array.  
    Default is empty.  
    Output : - Datasets.  
    License: GNU general public license version 3  
    Tested : Matlab R2014a  
    By : Eran O. Ofek                    May 2015  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Data=Util.IO.loadh('R.hd5','R');  
    Reliable: 2  
      
      
### io.files.mgrep

grep-like utility for MATLAB. Search for substrings in a text file. Package: Util.files Description: grep-like utility for MATLAB. Search for substrings in a text file.


    
    grep-like utility for MATLAB. Search for substrings in a text file.  
    Package: Util.files  
    Description: grep-like utility for MATLAB. Search for substrings  
    in a text file.  
    Input  : - File name.  
    - lookup string.  
    * Arbitrary number of pairs of arguments:  
    ...,keyword,value,...  
    Available keywords are:  
    'CaseS'   - case sensitive {'y' | 'n'}, default is 'y'.  
    Output : - Cell array of all lines containing look up string.  
    - Vector of positions of lookup string within line.  
    - Vector of line numbers for each match.  
    Tested : Matlab 7.0  
    By : Eran O. Ofek        Feb 2006  
    URL : http://wise-obs.tau.ac.il/~eran/matlab.html  
    Example: [Res,IndLine]=mgrep('mgrep.m','length','CaseS','n');  
      
### io.files.rdir

recursive dir function Package: Util.IO Description: run the dir function recursively on a tree of directories looking for specific files.


    
    recursive dir function  
    Package: Util.IO  
    Description: run the dir function recursively on a tree of directories  
    looking for specific files.  
    Input  : - File template (e.g., '*.mat').  
    Output : - Structure array of files information.  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Feb 2017  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: io.files.rdir('*.mat');  
    Reliable: 2  
      
      
      
    Out = Util.struct.struct_def({'name','folder','date','bytes','isdir','datenum'},0,1);  
### io.files.read_delimted_head

Read delimited file with header Package: Util.IO Description: Read a delimited table in which one of the first lines is the header of the delimited table. The program returns


    
    Read delimited file with header  
    Package: Util.IO  
    Description: Read a delimited table in which one of the first lines  
    is the header of the delimited table. The program returns  
    a cell array in which each cell is a column in the table  
    and a structure containing the column header names.  
    The default parameters are optimized to deal with some  
    SQL output.  
    Input  : - File name.  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    The following keywords are available:  
    'HeadLine'    - The number of the line containing the header.  
    Default is 2.  
    'StartLine'   - The number of the line in which the table begins.  
    Default is 4.  
    'Delim'       - Delimiter. Default is '|'.  
    'CommentStyle'- Comment style to ignore. Default is '('.  
    'StringHead'  - A cell array which contains strings.  
    The substrings will be matched with the headers  
    and if an header contains this substring  
    than its corresponding column will be treated  
    as a column of strings. All the other columns  
    will be read as numbers.  
    Default is {'fits_'}.  
    Output : - Cell array in which is cell is a column in the table.  
    - Structure in which each field is the column name,  
    and the value of the field is the column index.  
    Tested : Matlab 7.11  
    By : Eran O. Ofek                    Apr 2011  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
      
      
### io.files.read_formatted

Read text with column position format. Package: Util.IO Description: Read text/data file with constant format (i.e., columns at specfied locations). The start and end


    
    Read text with column position format.  
    Package: Util.IO  
    Description: Read text/data file with constant format (i.e., columns  
    at specfied locations). The start and end  
    column for each entry should be specified.  
    Input  : - File name (string).  
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
    See also:  read_str_formatted.m  
    Tested : Matlab 7.3  
    By : Eran O. Ofek                    Jan 2008  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Data=Util.IO.read_formatted('file.txt',{1,5,'d'; 7,15,'f'});  
    Reliable: 2  
      
      
      
### io.files.read_user_pass_file

Read user/password from file Package: Util.files Description: Read user/password from file


    
    Read user/password from file  
    Package: Util.files  
    Description: Read user/password from file  
    Input  : - File name and path.  
    Output : - User  
    - Password  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Nov 2017  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example:  
    [User,Pass]=Util.files.read_user_pass_file('~/matlab/passwords/ztf_archive_pass');  
    Reliable: 2  
      
      
      
    assume user/pass are given in file  
### io.files.saveh

Save a matrix into HDF5 file. Package: Util.IO Description: Save a matrix into HDF5 file. If file exist then will add it as a new dataset to the same file.


    
    Save a matrix into HDF5 file.  
    Package: Util.IO  
    Description: Save a matrix into HDF5 file. If file exist then will  
    add it as a new dataset to the same file.  
    This is becoming faster than matlab (2014a) for matices with  
    more than ~10^5 elements.  
    Input  : - File name.  
    - Data to save (e.g., matrix).  
    - Dataset (variable) name under which to store the data  
    in the HDF5 file. If varaible already exist, then function  
    will fail. Default is 'V'.  
    If empty use 'V'.  
    - Cell array of attributes {key,val,...}.  
    Default is {}.  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    to pass to the h5create. See h5create.m for details.  
    Output : - Variable name (dataset) in which the matrix was stored.  
    License: GNU general public license version 3  
    Tested : Matlab R2014a  
    By : Eran O. Ofek                    May 2015  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: VarName=Util.IO.saveh('R.hd5',T,'G/A');  
    Reliable: 2  
      
      
### io.files.superdir

A 'dir'-like function that can deal with more types of wild cards Package: io.files Description: A version of the matlab 'dir' function that can deal with more sophisticated types of wild cards.


    
    A 'dir'-like function that can deal with more types of wild cards  
    Package: io.files  
    Description: A version of the matlab 'dir' function that can deal with  
    more sophisticated types of wild cards.  
    For example searching for: 'l*00[19-21].fits'.  
    Input  : - String of filenames to match  
    Output : - A dir structure (see dir.m)  
    Tested : Matlab 7.10  
    By : Eran O. Ofek                    May 2010  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reliable: 2  
      
      
### io.files.wc

Apply the Unix wc (word count) command for a file name. Package: Util.files Description: Apply the Unix wc (word count) command for a file name in the current directory.


    
    Apply the Unix wc (word count) command for a file name.  
    Package: Util.files  
    Description: Apply the Unix wc (word count) command for a file name in  
    the current directory.  
    Input  : - Character array containing file name.  
    - Options:  
    'c' - Return number of bytes only (fast).  
    'm' - Return number of chars only.  
    'l' - Return number of lines only (fast) - default.  
    'L' - Return max line length only.  
    'w' - Return number of words only.  
    Output : - Requested number.  
    Tested : Matlab 5.3 (on Linux).  
    By : Eran O. Ofek                    Jul 2004  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Res=wc('wc.m','c');  
      
### io.files.wcl

Count the number of lines in a file. Package: Util.files Description: Count the number of lines in a file.


    
    Count the number of lines in a file.  
    Package: Util.files  
    Description: Count the number of lines in a file.  
    Input  : - String containing file name.  
    Output : - Number of lines in file.  
    Tested : Matlab 7.8  
    By : Eran O. Ofek                    Nov 2009  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Counter=wcl('wcl.m');  
    Reliable: 2  
      
### io.files.which_dir

Return the directory in which a matlab program resides. Package: Util.files Description: Return the directory in which a matlab program resides. This program is a version of "which.m" that trim the


    
    Return the directory in which a matlab program resides.  
    Package: Util.files  
    Description: Return the directory in which a matlab program resides.  
    This program is a version of "which.m" that trim the  
    program name from the full path.  
    Input  : - Matlab program name.  
    Output : - Directory in which program resides.  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                       Feb 2007  
    URL : http://wise-obs.tau.ac.il/~eran/matlab.html  
    Example: which_dir('fits_xy2rd')  
    Reliable: 1  
      
      
