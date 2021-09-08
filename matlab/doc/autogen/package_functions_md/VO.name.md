# Package: VO.name


### VO.name.server_ned

Resolve an astronomical object name into coordinates using NED. Package: VO.name Description: Resolve an astronomical object name into coordinates using NASA Extragalactic Database (NED).


    
    Resolve an astronomical object name into coordinates using NED.  
    Package: VO.name  
    Description: Resolve an astronomical object name into coordinates using  
    NASA Extragalactic Database (NED).  
    Input  : - String of object name (e.g., 'm81');  
    - Output units {'d','r','SH'}. Default is 'd'.  
    Output : - J2000.0 RA [deg].  
    - J2000.0 Dec [deg].  
    Tested : Matlab R2011b  
    By : Eran O. Ofek                    Jun 2014  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [RA,Dec]=VO.name.server_ned('m31');  
    Reliable: 2  
      
      
### VO.name.server_simbad

Resolve an astronomical object name into coordinates using SIMBAD Package: VO.name Description: Resolve an astronomical object name into coordinates using SIMBAD database.


    
    Resolve an astronomical object name into coordinates using SIMBAD  
    Package: VO.name  
    Description: Resolve an astronomical object name into coordinates using  
    SIMBAD database.  
    Input  : - String of object name (e.g., 'm81');  
    - Output units ['d','r','SH']. Default is 'd'.  
    - Coordinate type ['ICRS','FK5','FK4']. Default is 'ICRS'.  
    Output : - J2000.0 RA [deg].  
    - J2000.0 Dec [deg].  
    Tested : Matlab R2011b  
    By : Eran O. Ofek                    Jun 2014  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [RA,Dec]=VO.name.server_simbad('m31');  
    Reliable: 2  
      
      
