# Package: imUtil.util


### imUtil.util.filelist

Generate a cell array of files list from file name/regular expression Package: @imUtil.util


    
    Generate a cell array of files list from file name/regular expression  
    Package: @imUtil.util  
    Input  : - A file name, a file name containing wild  
    cards or regular expression, a cell array of  
    file names, or a structure arrawy which is the  
    output of the dir command.  
    - Either: 'regexp', or 'wild'.  
    'wild' allows for simple wild cards (default).  
    'regexp' allows for full regular expressions.  
    Output : - A cell array of file names.  
    Example: List=imUtil.util.filelist('\w*.fits','regexp');  
      
### imUtil.util.filename_constructor

Package: @imUtil.util


    
      
    Package: @imUtil.util  
    Input  : - Project name string.  
    - Str date, or numeric JD.  
    - Field number (up to 6 digits), or name  
    - Filter  
    - Detector  
    - SubImage number, 0 for full. up to 3 digits  
    - Image type: bias, dark, flat, domeflat, focus, science  
    - Processing stage: raw, proc, back, var, nim,  exp, mask, psf, cat  
    - Extension: fits, hdf5  
    Output : - A cell array of file names.  
    Example: FileName =  
    imUtil.util.filename_constructor('LAST_1.1',2451545,1,'mono','qhy600',0,'science','raw','fits')  
      
### imUtil.util.get_env

attempt to read environment variable and print error if failes Package: @imUtil.util


    
    attempt to read environment variable and print error if failes  
    Package: @imUtil.util  
    Input  : - Envirinment variable name.  
    - Logical flag indicating if to attempt to convert variable to  
    numeric. Default is true.  
    Output : - Value of environment variable.  
    Example: imUtil.util.get_env('imClass.specCl.get_Pickles.DataDir')  
      
      
