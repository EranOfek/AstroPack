# Package: imUtil.util.file


### imUtil.util.file.construct_filename

Construct image/catalog file name based on the LAST/ULTRASAT standard Package: +imUtil/+util/+file Description: Return data product file name and path according to the LAST/ULTRASAT standard.


    
    Construct image/catalog file name based on the LAST/ULTRASAT standard  
    Package: +imUtil/+util/+file  
    Description: Return data product file name and path according to the  
    LAST/ULTRASAT standard.  
    <ProjName>.<TelescopeID>_YYYYMMDD.HHMMSS.FFF_<filter>_<FieldID>_<type>_<level>.<sub level>_<Product>_<version>.<FileType>  
    Input  : * Pairs of ...,key,val,... Possible keywords include:  
    'ProjName' - Default is 'LAST.0.1'.  
    'Date' - If empty, then will use current computer time, and  
    will assume the time is in UTC.  
    If numeric then assume the time is in JD, if char then  
    assume the time is in the YYYY-MM-DDTHH:MM:SS.FFF  
    format.  
    Default is [].  
    'TimeZone' - Time zone [hours]. Default is 2.  
    'Filter' - Default is 'clear'.  
    'FieldID' - Default is ''.  
    'FormatFieldID' - Formatting of FieldID if is given as number.  
    Default is '06d'.  
    'Type' - either bias, dark, domeflat, twflat, skyflat, fringe,  
    sci, wave.  
    Default is 'sci'.  
    'Level' - either log, raw, proc, stack, coadd, ref.  
    Default is 'raw'.  
    'SubLevel' - Options are:  
    n - normal  
    s - proper subtraction S  
    sp - proper subtraction S tag.  
    d - proper subtraction D  
    t - Translient  
    r - proper coaddition R  
    m - matched filter with unspecified filter  
    Default is ''.  
    'Product' - either: im, back, var, imflag, exp, Nim, psf, cat, spec.  
    Default is 'im'.  
    'Version' - Default is 1.  
    'FormatVersion' - Formatting of Version if is given as number.  
    Default is '03d'.  
    'FileType' - Default is 'fits'.  
    'RefVersion' - Reference image version. Default is 1.  
    'FormatRefVersion' - Format for numeric reference version.  
    Default is '03d'.  
    'SubDir' - This is the area/location directory below the  
    coadd/ref directory. Default is ''.  
    'DataDir' - Default is 'data'.  
    'Base' - Default is '/home/last'.  
    Output : -File name.  
    - Path string.  
    Example: FileName=imUtil.util.file.construct_filename  
    [FileName,Path]=imUtil.util.file.construct_filename('FieldID',100)  
      
      
### imUtil.util.file.construct_path

Construct image/catalog file path based on the LAST/ULTRASAT standard Package: +imUtil/+util/+file Description: Construct file path for ULTRASAT/LAST standard. The file path is described in the LAST/ULTRASAT file naming


    
    Construct image/catalog file path based on the LAST/ULTRASAT standard  
    Package: +imUtil/+util/+file  
    Description: Construct file path for ULTRASAT/LAST standard.  
    The file path is described in the LAST/ULTRASAT file naming  
    convension document.  
    Options are:  
    /data/YYYY/MM/DD/raw/ - contains all the science raw data  
    /data/YYYY/MM/DD/log/Â  - contains all the log files  
    /data/YYYY/MM/DD/proc/ - contains all the single processed images including: image, mask, back (if provided), var (if provided), PSF (if provided), and catalogs.  
    /data/YYYY/MM/DD/calib/ - contains all the processed calibration images/variance/masks/catalogs  
    /data/YYYY/MM/DD/stacked/ - contains all the processed coadd images (coaddition of images of the same field taken continuously only) - images/masks/catalogs/PSF/subtraction productsÂ  
    /data/ref/version<#>/area/ - All sky reference/coadd image - images/masks/catalogs/PSF  
    /data/coadd/area/ - arbitrary coadded images (coadd images of arbitrary field over arbitrary time periods)  
    Input  : * Pairs of ...,key,val,... Possible keywords include:  
    'Date' - If empty, then will use current computer time, and  
    will assume the time is in UTC.  
    If numeric then assume the time is in JD, if char then  
    assume the time is in the YYYY-MM-DDTHH:MM:SS.FFF  
    format.  
    Default is [].  
    'TimeZone' - Time zone [hours]. Default is 2.  
    'Level' - either log, raw, proc, stack, coadd, ref.  
    Default is 'raw'.  
    'Type' - either bias, dark, domeflat, twflat, skyflat, fringe,  
    sci, wave.  
    Default is 'sci'.  
    'RefVersion' - Reference image version. Default is 1.  
    'FormatRefVersion' - Format for numeric reference version.  
    Default is '03d'.  
    'SubDir' - This is the area/location directory below the  
    coadd/ref directory. Default is ''.  
    'DataDir' - Default is 'data'.  
    'Base' - Default is '/home/last'.  
    Output : - Path string.  
    By : Eran Ofek                            Aug 2020  
    Example: Path=imUtil.util.file.construct_path  
    Path=imUtil.util.file.construct_path('Level','ref','SubDir','x')  
    Path=imUtil.util.file.construct_path('Level','proc','Type','bias')  
      
      
      
### imUtil.util.file.date_directory

Return the D/M/Y for a directory given a date Package: +imUtil.util.file Description: When associating a date with a file/directory name, the nearest day at the previous noon is chosen.


    
    Return the D/M/Y for a directory given a date  
    Package: +imUtil.util.file  
    Description: When associating a date with a file/directory name, the  
    nearest day at the previous noon is chosen.  
    Input  : - A JD, [D M Y F], [D M Y H M S], 'YYYYMMDDTHHMMSS.FFF'.  
    If empty or not provided than will use current UTC date.  
    - Time zone [hours]. Default is 2.  
    Output : - Year  
    - Month  
    - Day  
    Example: [Y,M,D]=imUtil.util.file.date_directory  
      
      
### imUtil.util.file.filename2prop

Convert file name to properties Package: +imUtil.util.file


    
    Convert file name to properties  
    Package: +imUtil.util.file  
    Input  : - A file name or a cell array of file names  
    - Flag indicating if to convert Field ID to numeric.  
    Default is false.  
    Output : - A structure array of properties constructed from the file  
    name.  
    Example: Prop=imUtil.util.file.filename2prop('LAST_20200914.222852.890_clear_0.54_dark_proc.n_var_001.fits')  
      
### imUtil.util.file.load2

Load a mat file into a variable Package: imUtil.util.file Description: load a mat file containing a single variable to a variable name (rather than a structure, like load.m).


    
    Load a mat file into a variable  
    Package: imUtil.util.file  
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
      
      
      
### imUtil.util.file.load_check

Load, but check if variable exist in workspace. Package: +imUtil.util.file Description: Load a matlab variable or file from disk (similar to the load.m command). However, before the variable is loaded the


    
    Load, but check if variable exist in workspace.  
    Package: +imUtil.util.file  
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
    Example: Var=imUtil.util.file.load_check('Tycho2.mat');  
    Reliable: 1  
      
      
