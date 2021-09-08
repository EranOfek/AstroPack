# Package: tools.os


### tools.os.cdmkdir

cd to directory - if not exist than create Package: +lastpipe.util


    
    cd to directory - if not exist than create  
    Package: +lastpipe.util  
    Input  : - Directory name.  
    Output : - A logical flag indicating if the directory was created (true)  
    or already exist (false).  
    Example: lastpipe.util.cdmkdir('a/b')  
      
      
### tools.os.getTestDataDir

Eaxmple: DataSampleDir = tools.os.getTestDataDir


    
      
    Eaxmple: DataSampleDir = tools.os.getTestDataDir  
      
### tools.os.get_computer

Get computer name Package: Uti.OS Description: Get computer name.


    
    Get computer name  
    Package: Uti.OS  
    Description: Get computer name.  
    Input  : null  
    Output : - Computer name.  
    License: GNU general public license version 3  
    Tested : Matlab R2015b  
    By : Eran O. Ofek                    Jul 2016  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Ans=Util.OS.get_computer  
    Reliable: 2  
      
      
### tools.os.get_user

Get user name Package: Util.OS Description: Get user name.


    
    Get user name  
    Package: Util.OS  
    Description: Get user name.  
    Input  : null.  
    Output : - User name.  
    License: GNU general public license version 3  
    Tested : Matlab R2015b  
    By : Eran O. Ofek                    Jul 2016  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Ans=Util.OS.get_user;  
    Reliable: 2  
      
      
### tools.os.get_userhome

Get user home directory path Package: Util.OS Description: Get user home directory path.


    
    Get user home directory path  
    Package: Util.OS  
    Description: Get user home directory path.  
    Input  : null.  
    Output : - User home directory path.  
    License: GNU general public license version 3  
    Tested : Matlab R2015b  
    By : Eran O. Ofek                    Jul 2016  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Ans=Util.OS.get_userhome  
    Reliable: 2  
      
      
### tools.os.islinux




    
      
### tools.os.iswindows




    
      
### tools.os.matlab_pid

Return the matlab PID and computer host name Example: [PID, Host] = tools.os.matlab_pid;


    
    Return the matlab PID and computer host name  
    Example: [PID, Host] = tools.os.matlab_pid;  
      
### tools.os.system_list

Run the system command on a list of files. Package: Util.OS Description: Run the system command for a list of files.


    
    Run the system command on a list of files.  
    Package: Util.OS  
    Description: Run the system command for a list of files.  
    Input  : - System command string (e.g., 'gzip -d s').  
    - List of files on which to run the system command.  
    This can be any valid input to create_list.m which generate  
    a cell array of files.  
    Output : - Vector of Status per file.  
    - Cell array of Results per file.  
    Tested : Matlab R2014a  
    By : Eran O. Ofek                    Jan 2015  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [Stat,Res]=Util.OS.system_list('gzip -d s',{'File1.gz','File2.gz'});  
    Reliable: 2  
      
      
      
### tools.os.systemarg

Running the UNIX system command. Package: Util.OS Description: Running the UNIX system command.


    
    Running the UNIX system command.  
    Package: Util.OS  
    Description: Running the UNIX system command.  
    Input  : * Arbitrary number of arguments that will be concatenated  
    with spaces to a single string and will be run using  
    the system command.  
    If you want to use ' sign use ''' instead.  
    Output : - Results.  
    - Status.  
    Tested : Matlab R2014a  
    By : Eran O. Ofek                    Jul 2014  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: import Util.OS.*  
    systemarg sed "s#_# #g" try > try1  
    systemarg awk '''{ print }''' /etc/passwd  
    Reliable: 2  
      
      
### tools.os.user_name

Get user name Package: Util.OS Description: Get the current user name.


    
    Get user name  
    Package: Util.OS  
    Description: Get the current user name.  
    Input  : null  
    Output : - User name.  
    Tested : Matlab R2014a  
    By : Eran O. Ofek                    Jan 2015  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    BUGS : Does not work on Windows  
    Example: User=Util.OS.user_name;  
    Reliable: 2  
      
      
      
