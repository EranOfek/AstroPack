# Package: tools.code


### tools.code.fun_template

Generate a function file template Pacakge: Util.code Description: Generate a functionm template with help section and basic optional commands.


    
    Generate a function file template  
    Pacakge: Util.code  
    Description: Generate a functionm template with help section and basic  
    optional commands.  
    Input  : - Function name (e.g., 'my_fun1.m').  
    - Package name (e.g., '+Util/+string/').  
    - Toolbox path. Default is '~/matlab/fun/'.  
    Output : null  
    Tested : Matlab R2011b  
    By : Eran O. Ofek                    Mar 2014  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: fun_template('trya1.m','+Util/+code/');  
    Reliable: 2  
      
      
      
### tools.code.install_maat

Install Package: Util.code Description:


    
    Install  
    Package: Util.code  
    Description:  
    Input  : -  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    Output : -  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    May 2017  
    Edited: Ygal Y. Klein 29.5.2017  
    URL : http://weizmann.ac.il/home/eofek/matlabn/  
    Example:  
    Reliable:  
      
      
### tools.code.openb

Open matlab editor and save a backup copy of previous file Package: Util.code Description: open a matlab function in matlab editor (like the open command). Before the file is opened it is backuped in a backup (default is 'old') directory in the file directory


    
    Open matlab editor and save a backup copy of previous file  
    Package: Util.code  
    Description: open a matlab function in matlab editor (like the open  
    command). Before the file is opened it is backuped in  
    a backup (default is 'old') directory in the file directory  
    under the name FileName.current_date  
    Input  : - File name to backup and open.  
    Output : null  
    Tested : Matlab R2014a  
    By : Eran O. Ofek                    Jan 2015  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reliable: 2  
      
      
### tools.code.prep_function_list

Prepare list of all functions in the Astro Toolbox Package: tools.code Description: Prepare list of all functions in the Astro Toolbox under current directory.


    
    Prepare list of all functions in the Astro Toolbox  
    Package: tools.code  
    Description: Prepare list of all functions in the Astro Toolbox under  
    current directory.  
    Input  : - Input argument for recursive calls.  
    Output : - List of all functions.  
    - Cell array of selected data for each function.  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Feb 2017  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [List,Table]=tools.code.prep_function_list  
    Reliable: 2  
      
      
### tools.code.prep_maat_website

Prepare the Matlab Astronomy & AStrophysics Toolbox website Package: tools.code Description: Prepare the Matlab Astronomy & AStrophysics Toolbox website


    
    Prepare the Matlab Astronomy & AStrophysics Toolbox website  
    Package: tools.code  
    Description: Prepare the Matlab Astronomy & AStrophysics Toolbox website  
    Input  : -  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    Output : -  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Apr 2017  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: tools.code.prep_maat_website  
    Reliable: 1  
      
      
### tools.code.read_user_pass_file

Read user/password from file Package: Util.code Description: Read user/password from file


    
    Read user/password from file  
    Package: Util.code  
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
