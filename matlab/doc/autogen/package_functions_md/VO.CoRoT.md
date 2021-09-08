# Package: VO.CoRoT


### VO.CoRoT.read_corot_file

Read CoRoT file Package: VO.CoRoT Description: Read CoRoT fits files.


    
    Read CoRoT file  
    Package: VO.CoRoT  
    Description: Read CoRoT fits files.  
    Input  : - String containing a CoRoT fits file names in one of the  
    following formats:  
    (1) A cell vector containing a file name in each cell.  
    (2) A string containing wild cards (i.e., '*' or '?'),  
    in this case, the program will produce a list of  
    all matched files in the current directory.  
    (3) A string containing a file name (the file name not  
    necesserly exist).  
    (4) A string begining with '@' containing a file name  
    that contains a list of file (one per line).  
    - Exposure time in second. If given than find also  
    points in the LCs which are not seperated by  
    the exposure time+/-0.1s.  
    Output : - Cell array containing the LCs. One cell per target.  
    Each cell containing 8 (for 'MON') or 14 (for 'CHR')  
    cells each contain one column.  
    fits file.  
    - Col{1} - structure containing fields in 'CHR' files.  
    Col{2} - structure containing fields in 'MON' files.  
    - JD0 to add to tabulated JDs.  
    - Cell array containing all the fits files names.  
    - Cell array of vectors. For each file indicate the indices  
    in the file in which a discontinuity was found.  
    Tested : Matlab 7.6  
    By : Eran O. Ofek                    Jun 2009  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
      
### VO.CoRoT.read_corot_master

Read CoRoT master file Package: VO.CoRoT Description: Read the CoRoT master file - this file contains a list of all the CoRoT target stars with their mean properties.


    
    Read CoRoT master file  
    Package: VO.CoRoT  
    Description: Read the CoRoT master file - this file contains a list of  
    all the CoRoT target stars with their mean properties.  
    Input  : null.  
    Output : - Cell array containing the master file - cell per column.  
    - Structure containing flags indicating by which run/ccd a star  
    was observed.  
    - Cell array of full file names.  
    - Start JD column.  
    - End JD column.  
    - Structure of selected table columns.  
    Instellation: Requires the CoRoT master file  
    Tested : Matlan 7.8  
    By : Eran O. Ofek                    Oct 2009  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
      
      
