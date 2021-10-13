# Package: VO.Fermi


### VO.Fermi.wget_lat_weekly_data

Retrieve the Fermi/LAT weekly photon data in FITS format Package: VO.Fermi Description: Retrieve the last version of the Fermi/LAT weekly files from the HEASARC FTP site in FITS format. The function can get the list of files and their URLs,


    
    Retrieve the Fermi/LAT weekly photon data in FITS format  
    Package: VO.Fermi  
    Description: Retrieve the last version of the Fermi/LAT weekly files  
    from the HEASARC FTP site in FITS format.  
    The function can get the list of files and their URLs,  
    retrieve all files or retrieve specific files.  
    Input  : - A vector of files to retrieve.  
    If Inf then get all files. Default is Inf.  
    - Get files: true|false. Default is true.  
    - A cell array of additional arguments to pass to www.pwget.  
    Default is {}.  
    Output : - A cell array of the links to retrive.  
    By : Eran O. Ofek                    Jan 2017  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: List=VO.Fermi.wget_lat_weekly_data(Inf,false);  
    Reliable: 2  
      
