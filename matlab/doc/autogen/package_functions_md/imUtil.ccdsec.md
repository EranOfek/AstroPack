# Package: imUtil.ccdsec


### imUtil.ccdsec.ccdsec2str

Conver a CCDSEC [Xmin Xmax Ymin Ymax] to a string. Example: Str = imUtil.ccdsec.ccdsec2str([1 1000 21 900])


    
    Conver a CCDSEC [Xmin Xmax Ymin Ymax] to a string.  
    Example: Str = imUtil.ccdsec.ccdsec2str([1 1000 21 900])  
      
### imUtil.ccdsec.center_ccdsec

Calculate the [X, Y] centers of lines in CCDSEC


    
    Calculate the [X, Y] centers of lines in CCDSEC  
    Input  : - A 4 column matrix of CCDSEC, one per line.  
    Output : - The X center of the CCDSEC (each entry corresponds to a  
    line in the input CCDSEC).  
    - The Y center of the CCDSEC.  
    Author : Eran Ofek (Aug 2021)  
    Example: [X,Y] = imUtil.ccdsec.center_ccdsec([1 100 1 201; 1 10 1 11])  
      
### imUtil.ccdsec.flag_ccdsec

Flag the pixels within or outside a ccd section


    
    Flag the pixels within or outside a ccd section  
    Input  : - An image size [I J] for the full image.  
    - CCDSEC [Xmin, Xmax, Ymin, Ymax]. The inside (including edge)  
    or outside of this CCSEC will be flaged.  
    - A logical indicating if to flag the inside (true) or  
    outside (false) of the CCDSEC. Default is false.  
    Output : - A matrix of size like the full image size with true in/out  
    the CCDSEC.  
    Author : Eran Ofek (May 2021)  
    Example: Flag = imUtil.ccdsec.flag_ccdsec([10 10], [2 8 3 9], false)  
      
### imUtil.ccdsec.remove_edge_section

Remove a CCDSEC region located at an edge of another CCDSEC


    
    Remove a CCDSEC region located at an edge of another CCDSEC  
    Input  : - Image Size [I, J]  
    - CCDSEC to remove [xmin xmax ymin ymax]  
    - Direction (2, in x-axis).  
    Output : - The new CCDSEC  
    Author : Eran Ofek (May 2021)  
    Example: CCDSEC = imUtil.ccdsec.remove_edge_section([100 200],[1 10 1 100],2)  
    CCDSEC = imUtil.ccdsec.remove_edge_section([100 200],[1 200 91 100],1)  
      
      
      
    calculate CCDSEC (the non-overscan region)  
### imUtil.ccdsec.selectNearEdges

construct a vector of matrix indices for pixels near the edge of the CCDSEC


    
    construct a vector of matrix indices for pixels near the edge of the CCDSEC  
    Input  : - Image size [I, J], or CCDSEC [1 Xmax 1 Ymax].  
    - Distance from edge to mark. Default is 5.  
    Output : - A matrix of logicals (same size as indicated in the image size)  
    with true for pixels near the edges.  
    Author : Eran Ofek (May 2021)  
    Example: Flag = imUtil.ccdsec.selectNearEdges([10 12], 3)  
      
### imUtil.ccdsec.trim_ccdsec

trim edges from a CCDSEC vector


    
    trim edges from a CCDSEC vector  
    Input  : - Either a CCDSEC [Xmin Xmax Ymin Ymax]  
    or size(Image) sizeIJ.  
    - Number of pixels to trim in [X, Y].  
    Output : - An updated CCDSEC.  
    Author : Eran Ofek (Jul 2021)  
    Example: CCDSEC = imUtil.ccdsec.trim_ccdsec([100 100],[2 3])  
      
