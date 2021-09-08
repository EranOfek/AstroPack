# Package: celestial.search


### celestial.search.find_coo

Cone search in a table with spherical coordinates. Package: celestial.search Description: Search for a coordinate within a radius in a table of spherical coordinates.


    
    Cone search in a table with spherical coordinates.  
    Package: celestial.search  
    Description: Search for a coordinate within a radius in a table of  
    spherical coordinates.  
    Input  : - Table sorted by latitude. By default [Long, Lat] in radians.  
    - Longitude to search [radians] or sexagesimal string.  
    - Latitude to search [radians] or sexagesimal string.  
    - Search radius [radians].  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    'CheckSorted' - Check if table is sorted. Default is false.  
    'ColRA'       - Column of longitude. Default is 1.  
    'ColDec'      - Column of latitude. Default is 1.  
    Output : - Indices of entries found in search radius.  
    - Distance [radians] of found entries from search point.  
    - Position angle [radians] of found entries from search point.  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Jul 2017  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [Ind,DistI,PAI]=celestial.search.find_coo(rand(10,2),0.4,0.4,0.1)  
    Reliable: 2  
      
      
### celestial.search.match_coo

Match two lists by spherical coordinates. Package: celestial.search Description: Given two lists with spherical coordinates, search for objects in the second list associated with each object in


    
    Match two lists by spherical coordinates.  
    Package: celestial.search  
    Description: Given two lists with spherical coordinates, search for  
    objects in the second list associated with each object in  
    the first list.  
    Input  : - First list. By default [Lon, Lat]. Coordinates in radians.  
    - Second list sorted by lat. By default [Lon, Lat].  
    Coordinates in radians.  
    - Match radius [radians]. Scalar or vector.  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    'MinDist'     - Minimum distance [radians].  
    Objects with distance smaller than this  
    distance will be excluded.  
    Default is 0.  
    'CheckSorted' - Verify that second list is sorted by Lat.  
    Default is false.  
    'ColRA1'      - Column of longitude in 1st list.  
    'ColDec1'     - Column of latitude in 1st list.  
    'ColRA2'      - Column of longitude in 2nd list.  
    'ColDec2'     - Column of latitude in 2nd list.  
    Output : - A structure array in which the number of elements is like  
    the number of lines in the first list.  
    The 'Ind' field is a list of indices in the second list,  
    found within the search radius from the 1st list entry.  
    - Vector of the number of matched per source.  
    - Like 1st output, but for angular distance [radians].  
    - Like 1st output, but for position angle [radians].  
    - Flag indicating if an entry in the 2nd list was matched.  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Jul 2017  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Res=celestial.search.match_coo(Data1,Data2,0.001);  
    Reliable: 2  
      
      
### celestial.search.match_coo_nearest

Match two lists by spherical coordinates for nearest source only. Package: celestial.search Description: Given two lists with spherical coordinates, search for nearest object in the second list associated with each


    
    Match two lists by spherical coordinates for nearest source only.  
    Package: celestial.search  
    Description: Given two lists with spherical coordinates, search for  
    nearest object in the second list associated with each  
    object in the first list.  
    Input  : - First list. By default [Lon, Lat]. Coordinates in radians.  
    - Second list sorted by lat. By default [Lon, Lat].  
    Coordinates in radians.  
    - Match radius [radians]. Scalar or vector.  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    'MinDist'     - Minimum distance [radians].  
    Objects with distance smaller than this  
    distance will be excluded.  
    Default is 0.  
    'CheckSorted' - Verify that second list is sorted by Lat.  
    Default is false.  
    'ColRA1'      - Column of longitude in 1st list.  
    'ColDec1'     - Column of latitude in 1st list.  
    'ColRA2'      - Column of longitude in 2nd list.  
    'ColDec2'     - Column of latitude in 2nd list.  
    Output : - A vecor of indices of the nearest source in the 2nd list found  
    within the angular distance from a source in the 1st list.  
    - Vector of the number of matched per source.  
    - Like 1st output, but for angular distance [radians].  
    - Like 1st output, but for position angle [radians].  
    - Flag indicating if an entry in the 2nd list was matched.  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Jul 2017  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Res=celestial.search.match_coo_nearest(Data1,Data2,0.001);  
    Reliable: 2  
      
      
