# Package: celestial.htm


### celestial.htm.cone_in_polysphere

Check if a cone (small circle) is within a convex spherical polygon Package: celestial.htm Description: Check if a cone (small circle) is within a convex spherical polygon which sides are great circles.


    
    Check if a cone (small circle) is within a convex spherical polygon  
    Package: celestial.htm  
    Description: Check if a cone (small circle) is within a convex spherical  
    polygon which sides are great circles.  
    Input  : - Matrix in which each column represent the longitude of the  
    poles of the half-spaces of a spherical polygin, where the  
    pole is directed into the polygon center of mass [rad].  
    - Matrix in which each column represent the latitude of the  
    poles of the half-spaces of a spherical polygin, where the  
    pole is directed into the polygon center of mass [rad].  
    - Vector of longitudes of the cones center [rad].  
    The size is either 1 or like the number of columns of the  
    first and second input arguments.  
    - Vector of latitudes of the cones center [rad].  
    The size is either 1 or like the number of columns of the  
    first and second input arguments.  
    - Vector of radii of the cones [rad].  
    The size is either 1 or like the number of columns of the  
    first and second input arguments.  
    Output : - Flag of logical indicating if cone is in polygon.  
    By : Eran O. Ofek                    Feb 2017  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: HTM=celestial.htm.htm_build(4);  
    Flag=celestial.htm.cone_in_polysphere(HTM(end).PolesCoo(:,1),HTM(end).PolesCoo(:,2),5.5,-0.6,0.01);  
    PLong=rand(3,1000); PLat=rand(3,1000); Long=rand(1,1000); Lat=rand(1000,1); Radius=0.01.*rand(1000,1);  
    Flag=celestial.htm.cone_in_polysphere(PLong,PLat,Long,Lat,Radius);  
    Reliable: 2  
      
      
      
### celestial.htm.gc_mid_section

Mid point on great circle between two points Package: celestial.htm Description: Given two points on a sphere, find the central point lying on the the shortest great circle section connecting the two points.


    
    Mid point on great circle between two points  
    Package: celestial.htm  
    Description: Given two points on a sphere, find the central point lying  
    on the the shortest great circle section connecting the  
    two points.  
    Input  : - A list of the first points. This is a three columns matrix  
    in which each row is a unit vector (R1).  
    Alternatively, by setting Dim=2, this can be a three rows  
    matrix in which each column is a unit vector (R1).  
    If R is a two column matrix, then assume the columns are  
    [Long1, Lat1] in radians.  
    If Dim=2, then the input should be a two rows matrix with  
    [Long1, Lat1] in each column.  
    - A list of the second points (similar the the first point).  
    - Dimension along to operate.  
    If 1, then assume the position vectors in (R) and (C) are  
    in rows, while if 2, then assume they are in columns.  
    Default is 1.  
    - Output type: either 'r' for [Long, Lat] in radians,  
    or 'v' for unit vectors. Default is 'v',  
    Output : - The coordinates, either unit vectors or [Long, Lat]  
    in radians, of the mid point on the great circle  
    connecting the two points.  
    Tested : Matlab 7.11  
    By : Eran O. Ofek                    Jul 2011  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: C=gc_mid_section([1 1 1],[0 1 0])  
    Reliable: 2  
      
      
      
### celestial.htm.htm_build

Build Hierarchical Triangular Mesh (HTM) structure Package: celestial.htm Description: Build Hierarchical Triangular Mesh (HTM) structure. This structure can be use for fast searches of data in catalogs on a sphere.


    
    Build Hierarchical Triangular Mesh (HTM) structure  
    Package: celestial.htm  
    Description: Build Hierarchical Triangular Mesh (HTM) structure.  
    This structure can be use for fast searches of data  
    in catalogs on a sphere.  
    Input  : - The number of levels in the HTM structure.  
    - A flag indicating if to return a ClassHTM object. Default is  
    false.  
    Output : - The HTM structure array with the follwoing fields.  
    .level  - Level depth  index (0 for the first level).  
    .coo    - Coordinates of triangular mesh [Long, Lat] in  
    radians. (3x2 matrix).  
    The coordinates are ordered such that the  
    right-hand rule is pointing toward the  
    center of the polygon.  
    .cosd   - Cosine directions of triangular mesh.  
    3x3 matrix in which each line corresponds to  
    each vertex of the triangle.  
    .id     - Triangle id. This is a vector in which the  
    number of elements equal to the number of levels.  
    The first level is between 0 to 7 and all  
    the other levels are between 0 to 3.  
    .father - The index of the father.  
    If empty matrix then there is no father.  
    .son    - Vector of indices of all sons.  
    If empty matrix then there are no sons.  
    - A structure array of list of levels (LevList).  
    Number of elements corresponds to the number of levels.  
    The structure contains the following fields:  
    .level - Level depth  index (0 for the first level).  
    .ptr   - Vector of indices of all elements in HTM  
    which are in this level.  
    .side  - Length of side of triangles in level [radians].  
    Tested : Matlab 7.11  
    By : Eran O. Ofek                      July 2011  
    URL : http://wise-obs.tau.ac.il/~eran/matlab.html  
    Example: [HTM,LevList]=celestial.htm.htm_build(4);  
    Reliable: 2  
      
      
### celestial.htm.htm_build_son

An auxilary function for htm_build Package: celestial.htm Description: An auxilary function for htm_build.m for building Hierarchical Triangular Mesh (HTM) structure.


    
    An auxilary function for htm_build  
    Package: celestial.htm  
    Description: An auxilary function for htm_build.m for building  
    Hierarchical Triangular Mesh (HTM) structure.  
    Input  : - The HTM structure array (see htm_build.m).  
    - The LevList structure array (see htm_build.m).  
    - Number of levels required.  
    - The last populated index  
    Output : - The HTM structure array (see htm_build.m).  
    - The LevList structure array (see htm_build.m).  
    Tested : Matlab 7.11  
    By : Eran O. Ofek                    Jul 2011  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [HTM,LevList]=celestial.htm.htm_build(4);  
    [HTM,LevList]=htm_build_son(HTM,LevList,2)  
    Reliable: 2  
      
      
    Ind        = numel(HTM);  
### celestial.htm.htm_search_cone

Search for all HTM leafs interscting a small circle (cone search) Package: celestial.htm Description: Search for all HTM leafs interscting a small circle (i.e., cone search).


    
    Search for all HTM leafs interscting a small circle (cone search)  
    Package: celestial.htm  
    Description: Search for all HTM leafs interscting a small circle  
    (i.e., cone search).  
    Input  : - HTM structure. See celestial.htm.htm_build  
    - Longitude [radians] to search.  
    - Latitude [radians] to search.  
    - Search radius [radians].  
    Example:  [HTM,LevList]=celestial.htm.htm_build(4);  
    ID=celestial.htm.htm_search_cone(HTM,1,1,0.0001)  
    Reliable : 2  
      
### celestial.htm.htm_search_point

Search for a single point-like coordinate in an HTM tree Package: celestial.htm Description: Search for a single point-like coordinate in an HTM tree.


    
    Search for a single point-like coordinate in an HTM tree  
    Package: celestial.htm  
    Description: Search for a single point-like coordinate in an HTM tree.  
    Input  : - HTM tree as generated by htm_build.m  
    - [Long, Lat] in radians or [X,Y,Z] cosine direction of  
    a single point to search in the HTM.  
    Output : - Index of the leaf in the HTM in which the search point is  
    contained.  
    Tested : Matlab 7.11  
    By : Eran O. Ofek                    Jul 2014  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [HTM,LevList]=celestial.htm.htm_build(7);  
    I=celestial.htm.htm_search_point(HTM,[1,1])  
    Reliable: 2  
      
      
### celestial.htm.in_halfspace

Is point in half space Package: celestial.htm Description: Given a unit vector R and half space (N,C) test if the point is contained inside the half space (N dot R > C). A halfspace is a plane that splits the sphere in two.


    
    Is point in half space  
    Package: celestial.htm  
    Description: Given a unit vector R and half space (N,C) test if the  
    point is contained inside the half space (N dot R > C).  
    A halfspace is a plane that splits the sphere in two.  
    It is defined by a direction vector (N) and a signed scalar  
    (C), measured along the normal vector from the origin of  
    the sphere.  
    Input  : - A three columns matrix in which each row is a unit vector (R).  
    Alternatively, by setting Dim=2, this can be a three rows  
    matrix in which each column is a unit vector (R).  
    If R is a two column matrix, then assume the columns are  
    [Long, Lat] in radians.  
    If Dim=2, then the input should be a two rows matrix with  
    [Long, Lat] in each column.  
    - A unit vector (3 elements) or [Long, Lat] coordinates  
    (two elements) specifing the half space position vector (N).  
    Alternatively, this can be a matrix which dimensions are  
    identical to the input position vectors (R). In this case,  
    each vector R will be compared with the corresponding vector  
    (N).  
    - A signed scalar (C) between -1 and 1, measured along the  
    normal vector from the origin of the sphere.  
    Alternatively, C may be a vector which length is identical  
    to the number of vectors in (R).  
    (C) is the cosine of the angular radius of the cap.  
    - Dimension along to operate.  
    If 1, then assume the position vectors in (R) and (C) are  
    in rows, while if 2, then assume they are in columns.  
    - Flag indicating if to use ">" or ">=".  
    If true then use (N dot R > C),  
    If flase then use (N dot R) >= C  
    Output : - A flag indicating if the vector is inside the halfspace (true)  
    or on or outside the halfspace (false).  
    Tested : Matlab 7.11  
    By : Eran O. Ofek                    Jul 2011  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Flag=celestial.htm.in_halfspace([1 1 1],[1 1 1],0.5,1,1)  
    Reliable: 2  
      
      
    Def.Dim  = 1;  
    Def.Crit = true;  
    if (nargin3),  
    Dim  = Def.Dim;  
    Crit = Def.Crit;  
    elseif (nargin4),  
    Crit = Def.Crit;  
    elseif (nargin5),  
    do nothing  
    else  
    error('Illegal number of input arguments');  
    end  
      
### celestial.htm.in_polysphere

Is point inside a convex spherical polygon Package: celestial.htm Description: Check if a list of positions are found within a convex polygon on the celestial sphere in which its sides are great circles.


    
    Is point inside a convex spherical polygon  
    Package: celestial.htm  
    Description: Check if a list of positions are found within a convex  
    polygon on the celestial sphere in which its sides are  
    great circles.  
    The polygon should be defined according to the  
    right-hand rule.  
    Input  : - List of positions to check if they are inside the convex  
    spherical polygon. Each row corrsponds to a position.  
    This is a matrix of either 2 or 3 columns.  
    If two columns are provided then these are [Long, Lat]  
    in radians. If three columns are given, these are  
    cosine directions.  
    - The verteces of a convex polygon on the celestial sphere  
    in which its sides are great circles.  
    Each row correspond to one vertex.  
    This is a matrix of either 2 or 3 columns.  
    If two columns areprovided then these are [Long, Lat]  
    in radians. If three columns are given, these are  
    cosine directions.  
    The coordinates should be ordered such that the  
    right-hand rule is pointing toward the  
    center of the polygon.  
    - Flag indicating if to use ">" or ">=" in in_halfspace.m.  
    If 1 (default) then use (N dot R > C),  
    If 2 then use (N dot R) >= C.  
    Output : - A flag indicating if each position (row in Positions  
    matrix) is inside (true) or on/outside the polygon (false).  
    Tested : Matlab 7.11  
    By : Eran O. Ofek                    Jul 2011  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Corners=[0 0;1 0;1 1;0 1];  
    Positions=[0.5 0.5;2 2; 0 0; eps eps];  
    Flag = celestial.htm.in_polysphere(Positions,Corners);  
    Reliable: 2  
      
      
### celestial.htm.nhtm2level

Given number of HTM elements calculate number of levels. Package: celestial Description: Given number of HTM elements calculate number of HTM levels.


    
    Given number of HTM elements calculate number of levels.  
    Package: celestial  
    Description: Given number of HTM elements calculate number of HTM levels.  
    Input  : - Number of HTM elements.  
    Output : - Number of HTM levels.  
    - Number of HTMs in the lowest level.  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Jan 2018  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Level=celestial.htm.nhtm2level(43688)  
    Reliable: 2  
      
      
### celestial.htm.polysphere_poles

Given a spherical polygon vertces, find the poles of each of sides Package: celestial.htm Description: Given the longitude and latitude of a convex polygon on the sphere in which its sides are great circles, find the poles of each great circle aiming the polygon center of mass.


    
    Given a spherical polygon vertces, find the poles of each of sides  
    Package: celestial.htm  
    Description: Given the longitude and latitude of a convex polygon on the  
    sphere in which its sides are great circles, find the poles  
    of each great circle aiming the polygon center of mass.  
    Input  : - Vector of the longitude of the vertces [radians].  
    - Vector of the latitude of the vertces [radians].  
    Output : - Vector of poles longitude [radians].  
    - Vector of poles latitude [radians].  
    By : Eran O. Ofek                    Feb 2017  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: VertLong=[0 1 0]'; VertLat=[0 0 1]';  
    [PoleLong,PoleLat]=celestial.htm.polysphere_poles(VertLong,VertLat)  
    Reliable: 2  
      
      
### celestial.htm.polysphere_sort

Sort a convex spherical polygon Package: celestial.htm Description: Given an (unsorted) convex spherical polygon vertices, sort the vertices acording to their descending position angle as measured from the polygon center of mass.


    
    Sort a convex spherical polygon  
    Package: celestial.htm  
    Description: Given an (unsorted) convex spherical polygon vertices, sort the  
    vertices acording to their descending position angle as  
    measured from the polygon center of mass.  
    Input  : - Column vector of the Long coordinates of the vertices of a  
    polygon [rad].  
    - Column vector of the Lat coordinates of the vertices of a  
    polygon [rad].  
    Output : - Column vector of the Long coordinates of the vertices of a  
    convex polygon [rad].  
    - Column vector of the Lat coordinates of the vertices of a  
    convex polygon [rad].  
    - Indices of the input polygon, such that PolyX(I) is the  
    output polygon.  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                    Sep 2016  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [OutPolyLong,OutPolyLat]=celestial.htm.polysphere_sort([0 1 2 1]',[0 0 1 1]');  
    Reliable: 2  
      
      
### celestial.htm.search_htm_coocat

search_htm_coocat function                                           htm Description:


    
      
    search_htm_coocat function                                           htm  
    Description:  
    Input  : - RA (J2000.0) (radians, sexahesimal string or [H M S]).  
    - Dec (J2000.0) (radians, sexahesimal string or [Sign D M S]).  
    - Search radius [radians].  
    - Either a file name containing the catalog of HTM centers or  
    the HTM centers.  
    The HTM centers is a structure array with a 'Cat' field  
    containing a 3 columns matrix of [RA, Dec, Ptr], sorted by  
    the declination  
    - Use search_cat.m (true) or use simple sphere_dist (false.  
    Use search_cat.m for large catalogs. Default is true.  
    Output : - Indices of the low level HTM in which the search coordinates  
    may reside in, and there are more than 0 sources.  
    Tested : Matlab R2014a  
    By : Eran O. Ofek                    Feb 2015  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: HTMind=celestial.htm.search_htm_coocat(1,1,1./RAD,'FIRST_htm.mat')  
    Reliable: 2  
      
### celestial.htm.tree_collect_leafs

Collect leafs in a tree Package: celestial.htm Description: Given a tree and a pointer to a node, collect all the leafs found at the last level below this node.


    
    Collect leafs in a tree  
    Package: celestial.htm  
    Description: Given a tree and a pointer to a node, collect all the leafs  
    found at the last level below this node.  
    Input  : - A tree structure array (e.g., htm_build.m).  
    - Pointer in the tree below to collect all leafs.  
    - Vector of collected pointers used for recusion.  
    For internal use. Default is empty matrix.  
    Output : - Vector of pointers to all tree elements at the last level  
    below the input node (i.e., all leafs that belong to a node).  
    Tested : Matlab 7.11  
    By : Eran O. Ofek                    Aug 2011  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [HTM,LevList]=celestial.htm.htm_build(4);  
    [Ptr]=tree_collect_leafs(HTM,100)  
    Reliable: 2  
      
      
