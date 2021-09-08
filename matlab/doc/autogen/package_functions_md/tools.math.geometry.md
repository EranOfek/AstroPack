# Package: tools.math.geometry


### tools.math.geometry.boundingCircle

fit the smallest-radius bounding circle to set of X, Y points


    
    fit the smallest-radius bounding circle to set of X, Y points  
    Input  : - An array containing X coordinates.  
    - An array containing Y coordinates (corresponding to the X  
    coordunates).  
    Output : - A two element vector of best circle position [X,Y].  
    - The minimum radius around the best center than encompass all  
    the data points.  
    Author : Eran Ofek (Apr 2021)  
    Example: X = rand(10,1); Y = rand(10,1);  
    [BestXY, BestRadius] = tools.math.geometry.boundingCircle(X,Y);  
    plot(X,Y,'+'); hold on; plot.plot_ellipse(BestXY, [BestRadius, BestRadius],[],0);  
      
### tools.math.geometry.cells_intersect_line

Find cells in 2D grid that intersets a line. Package: Util.Geom Description: Given a grid defining the positions of cells in a 2-D plane and equation of a line in this plane, find all the cells in


    
    Find cells in 2D grid that intersets a line.  
    Package: Util.Geom  
    Description: Given a grid defining the positions of cells in a 2-D plane  
    and equation of a line in this plane, find all the cells in  
    the grid that intersects the line.  
    Input  : - X coordinate of the cells in grid.  
    - Y coordinate of the cells in grid.  
    The X and Y vectors should be uniformly sampled with  
    identical steps.  
    - Matrix of cells in grid, default is zeros(length(Y),length(X)).  
    If empty matrix then use default.  
    - parameters [a b] of line of the form Y=a*x+b;  
    Output : - X vector  
    - Y vector  
    - Matrix of flags indicating if the line crossing the cell (1)  
    or not (0).  
    Tested : Matlab 7.10  
    By : Eran O. Ofek                    Aug 2010  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
      
      
### tools.math.geometry.cross1_fast

Fast version of cross product of two 3-elements vectors Package: Util.math Description: cross product of two 3-elements vectors. This is a fast version of the cross.m function. This function will work


    
    Fast version of cross product of two 3-elements vectors  
    Package: Util.math  
    Description: cross product of two 3-elements vectors. This is a fast  
    version of the cross.m function. This function will work  
    only between two vectors.  
    Input  : - First 3-element vector.  
    - Second 3-element vector.  
    Output : - Vector of cross product between the two input vectors.  
    The output vector is always a raw vector.  
    Tested : Matlab R2014a  
    By : Eran O. Ofek                    Jul 2014  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    See also: cross_fast.m  
    Example: C=Util.math.cross1_fast([1 2 3],[1 2 1])  
    Reliable: 2  
      
      
### tools.math.geometry.cross_fast

Fast cross product of two 3-elements matrices Package: Util.math Description: cross product of two 3-columns matrices. This is a fast version of the cross.m function.


    
    Fast cross product of two 3-elements matrices  
    Package: Util.math  
    Description: cross product of two 3-columns matrices. This is a fast  
    version of the cross.m function.  
    Input  : - First 3-element vector.  
    - Second 3-element vector.  
    Output : - Vector of cross product between the two input vectors.  
    The output vector is always a raw vector.  
    Tested : Matlab R2014a  
    By : Eran O. Ofek                    Jan 2015  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    See also: cross1_fast.m  
    Example: C=Util.math.cross_fast(rand(10,3),rand(10,3))  
    Reliable: 2  
      
      
### tools.math.geometry.curvlen

Calculate the length of a curve numerically. Package: Util.Geom Description: Calculate the length of a curve by summing the distances (sqrt[X^2+Y^2]) between successive points.


    
    Calculate the length of a curve numerically.  
    Package: Util.Geom  
    Description: Calculate the length of a curve by summing the  
    distances (sqrt[X^2+Y^2]) between successive points.  
    Input  : - Data matrix [X, Y], sorted by X.  
    Output : - Curve length.  
    Tested : Matlab 4.2  
    By : Eran O. Ofek                    Nov 1993  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Len=Util.Geom.curvlen(sortrows(rand(10,2),2));  
    Reliable: 2  
      
### tools.math.geometry.dist_box_edge

Distance of points froma rectangular box. Package: Util.Geom Description: Given a rectangular box and a scalar position, calculate the distance to the nearest rectangular edge.


    
    Distance of points froma rectangular box.  
    Package: Util.Geom  
    Description: Given a rectangular box and a scalar position, calculate  
    the distance to the nearest rectangular edge.  
    Input  : - Vector of X position.  
    - Vector of Y position.  
    - X position of edge [low, high].  
    - Y position of edge [low, high].  
    Output : - Minimum distance.  
    Tested : Matlab R2011b  
    By : Eran O. Ofek                    Jan 2014  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: MinDist=Util.Geom.dist_box_edge([5;16],[15;3],[1 2048],[1 4096]);  
    Reliable: 2  
      
      
### tools.math.geometry.dist_p2line

Distance between point and a line. Package: Util.Geom Description: Calculate the minimum distance in a 2-d space between a line and a point.


    
    Distance between point and a line.  
    Package: Util.Geom  
    Description: Calculate the minimum distance in a 2-d space between a line  
    and a point.  
    Input  : - Line definition,  
    If a two element vector is given [a b] then the line is  
    defined by y=ax+b;  
    If 2x2 matrix [x1 x2; y1 y2] is given then the line is  
    defined as going through the points (x1,y1) and (x2,y2).  
    - Point position [xp,yp].  
    Output : - Minimum distance between line and point.  
    - The coordinates (x,y) of the nearest-point-on-the-line  
    to the point [xp,yp].  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Feb 2004  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [MinDist,IP]=Util.Geom.dist_p2line([1,0],[1,0]);  
    Reliable: 2  
      
### tools.math.geometry.plane_dist

Distance between points on a 2D plane. Package: Util.Geom Description: Calculate the planner distance and angle between points.


    
    Distance between points on a 2D plane.  
    Package: Util.Geom  
    Description: Calculate the planner distance and angle between points.  
    Input  : - Column vector of X1 coordinates.  
    If only two input arguments are provided than this a matrix of  
    the [X1,Y1] coordinates.  
    - Column vector of Y1 coordinates.  
    If only two input arguments are provided than this a matrix of  
    the [X2,Y2] coordinates.  
    - Column vector of X2 coordinates.  
    - Column vector of Y2 coordinates.  
    Output : - Column vector of distance between pairs of points.  
    - Angles between pairs of points.  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Jan 2004  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    See also: sphere_dist.m  
    Example: [Dist,PA]=Util.Geom.plane_dist(1,1,2,2);  
    [Dist,PA]=Util.Geom.plane_dist([1 1;2 1],[1 2; 2 2]);  
    Reliable: 1  
      
### tools.math.geometry.plane_dist_thresh

Check if the distance between points on a plane is below some value. Package: Util.Geom Description: Given X and Y coordinates and a reference coordinates return a flag indicating if each point is within a distance


    
    Check if the distance between points on a plane is below some value.  
    Package: Util.Geom  
    Description: Given X and Y coordinates and a reference coordinates  
    return a flag indicating if each point is within a distance  
    from a reference point (using planer distance).  
    Input  : - Matrix of X coordinates.  
    - Matrix of Y coordinates.  
    - Reference X coordinates.  
    - Reference Y coordinates.  
    - Distance threshold.  
    - Search shape {'box'|'circ'}. Default is 'circ'.  
    Output : - Flag indicating if the input X/Y points are in the requested  
    region.  
    - Distance.  
    - Position angle [radians].  
    License: GNU general public license version 3  
    Tested : Matlab R2015b  
    By : Eran O. Ofek                    Mar 2016  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [Flag,Dist,PA]=Util.Geom.plane_dist_thresh(rand(100,1),rand(100,1),0.5,0.5,0.2,'box')  
    Reliable: 2  
      
      
### tools.math.geometry.polysort

Sort the vertices of convex polygon by position angle. Package: Util.Geom Description: Given an (unsorted) convex polygon vertices, sort the vertices acording to their position angle as measured from


    
    Sort the vertices of convex polygon by position angle.  
    Package: Util.Geom  
    Description: Given an (unsorted) convex polygon vertices, sort the  
    vertices acording to their position angle as measured from  
    the polygon center of mass.  
    Input  : - Column vector of the X coordinates of the vertices of a  
    polygon.  
    - Column vector of the Y coordinates of the vertices of a  
    polygon.  
    Output : - Column vector of the X coordinates of the vertices of a  
    convex polygon.  
    - Column vector of the Y coordinates of the vertices of a  
    convex polygon.  
    - Indices of the input polygon, such that PolyX(I) is the  
    output polygon.  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                    Mar 2007  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reliable: 2  
      
      
### tools.math.geometry.rotm

Return a numeric or symbolic 3-D rotation matrix about the X, Y or Z axis Package: Util.Geom Description: Return a numeric or symbolic 3-D rotation matrix about the X, Y or Z axis.


    
    Return a numeric or symbolic 3-D rotation matrix about the X, Y or Z axis  
    Package: Util.Geom  
    Description: Return a numeric or symbolic 3-D rotation matrix about the  
    X, Y or Z axis.  
    Input  : - If numeric scalar than this is the rotation angle [radians].  
    Alternatively if this is a string then the function will  
    return a symbolic rotation matrix and the string will be the  
    symbolic angle to use in the rotation matrix.  
    - Rotation matrix type - options are:  
    {1|'x'} - for rotation about the X axis.  
    {2|'y'} - for rotation about the Y axis.  
    {3|'z'} - for rotation about the Z axis.  
    {'sx'}  - Return symbolic rotation matrix about the X axis.  
    {'sy'}  - Return symbolic rotation matrix about the Y axis.  
    {'sz'}  - Return symbolic rotation matrix about the Z axis.  
    Output : - Rotation matrix.  
    Tested : Matlab 5.0  
    By : Eran O. Ofek                    Feb 1994  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Rot=rotm(45./RAD,'x');    3-D rotation 40deg about z-axis.  
    Rot=rotm(45./RAD,'Z'); Rot(1:2,1:2);  2-D rotation 45deg.  
    Rot=rotm('Theta','x');   symbolic rotation matrix  
    Reliable: 1  
      
      
### tools.math.geometry.traj_mindist

Time of minimum distance between two 2-D linear trajetories Package: Util.Geom Description: Given two linear trajectories in the 2-D plane (x and y position as function of time), calculate the time in which


    
    Time of minimum distance between two 2-D linear trajetories  
    Package: Util.Geom  
    Description: Given two linear trajectories in the 2-D plane (x and y  
    position as function of time), calculate the time in which  
    the distance between the two trajectories is minimal and  
    the distance at that time. Each line is defined by:  
    x_i = A_i + B_i*(t - T_i)  
    y_i = D_i + E_i*(t - T_t)  
    Input  : - A1  
    - B1  
    - D1  
    - E1  
    - T1  
    - A2  
    - B2  
    - D2  
    - E2  
    - T2  
    Output : - Time in which the distance is minimal.  
    - Minimum distance.  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Jun 2005  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Note   : This function was previously called: linemindist_t.m  
    Reliable: 2  
      
      
### tools.math.geometry.tri_equidist_center

Poistion of circumscribed circle. Package: Util.Geom Description: Find the position of a  point found in equal distances from the verteces of a planer triangle.


    
    Poistion of circumscribed circle.  
    Package: Util.Geom  
    Description: Find the position of a  point found in equal distances  
    from the verteces of a planer triangle.  
    Input  : - Matrix of 3XN of triangles X coordinate verteces. Triangle per  
    line.  
    - Matrix of 3XN of triangles Y coordinate verteces. Triangle per  
    line.  
    Output : - Vector of triangle equi-distance X position from all verteces.  
    - Vector of triangle equi-distance Y position from all verteces.  
    Tested : Matlab R2014a  
    By : Eran O. Ofek                    Feb 2015  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [Xc,Yc]=tri_equidist_center([0;1;2],[1;0;4])  
    Reliable: 2  
      
      
    solution is given by:  
    syms x1 x2 x3 y1 y2 y3 xc yc  
    A=solve('(xc-x1)^2+(yc-y1)^2=(xc-x2)^2+(yc-y2)^2','(xc-x2)^2+(yc-y2)^2=(xc-x3)^2+(yc-y3)^2',xc,yc)  
      
