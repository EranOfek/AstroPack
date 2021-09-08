# Package: imUtil.proj


### imUtil.proj.gnomonic

tangential (gnomonic) projection - spherical to plannar Package: +imUtil.tan


    
    tangential (gnomonic) projection - spherical to plannar  
    Package: +imUtil.tan  
    Input  : - An array of longitudes.  
    - An array of latitudes.  
    - Center projection point [Long, Lat].  
    Default is [0 0].  
    - The radius of the unit sphere which scales the output.  
    Effectively this is the scale of the output coordinates.  
    For example, 1 - output is close to radians.  
    180./pi - output is close to deg.  
    Default is 180./pi;  
    - Input coordinates and Center units.  
    Default is 'deg'.  
    Output : - X coordinates.  
    - Y coordinates.  
    By: Eran O. Ofek                         May 2020  
    Example: Lon = rand(10,1); Lat=rand(10,1);  
    [X,Y]=imUtil.proj.gnomonic(Lon,Lat,[0.5 0.5])  
      
### imUtil.proj.gnomonic_inv

tangential (gnomonic) projection - spherical to planar Package: +imUtil.tan


    
    tangential (gnomonic) projection - spherical to planar  
    Package: +imUtil.tan  
    Input  : - An array of longitudes.  
    - An array of latitudes.  
    - Center projection point [Long, Lat].  
    Default is [0 0].  
    - A scalar that represents the scale of X/Y.  
    For example, 180./pi means that X/Y are in units of deg  
    per unit-pix.  
    1, means that X/Y are in radians.  
    Default is 180./pi;  
    - A scalr that controls the units of the output coordinates  
    relative to degrees.  
    1 - will output the X,Y coordinates in degrees (i.e., at the  
    limit of small distances, unit distance equal 1 deg).  
    3600 - will output the X,Y coordinates in arcsec...  
    Default is 1.  
    - Input coordinates and Center units.  
    Default is 'deg'.  
    Output : - X coordinates.  
    - Y coordinates.  
    By: Eran O. Ofek                         May 2020  
    Example: Lon = rand(10,1); Lat=rand(10,1);  
    [X,Y]=imUtil.proj.gnomonic(Lon,Lat,[0.5 0.5],1,'deg')  
    [Lon1,Lat1]=imUtil.proj.gnomonic_inv(X,Y,[0.5 0.5],1,'deg')  
      
