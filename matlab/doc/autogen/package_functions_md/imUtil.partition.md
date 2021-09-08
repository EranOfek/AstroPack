# Package: imUtil.partition


### imUtil.partition.image_partitioning

Partition a 2D image into sub images. OBSOLETE: use imUtil.image.partition_subimages instead Package: ImUtil Description: Partition a 2D image into sub images.


    
    Partition a 2D image into sub images. OBSOLETE: use  
    imUtil.image.partition_subimages instead  
    Package: ImUtil  
    Description: Partition a 2D image into sub images.  
    Input  : - A single 2D image (matrix).  
    - BlockSize [X,Y], or [X] (will be copied as [X, X]).  
    Alternatively, if this is empty then will use ListEdge and  
    ListCenter parameters.  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    'ListEdge' - [xmin, xmax, ymin, ymax] as returned by  
    imUtil.partition.subimage_boundries.  
    This is used only if BlockSize is empty.  
    Default is empty.  
    'ListCenter' - [xcenter,ycenter] as returned by  
    imUtil.partition.subimage_boundries.  
    This is used only if BlockSize is empty.  
    Default is empty.  
    'Overlap' - Overlapping buffer. Default is 10 pix.  
    'FieldNameIm' - Image field name in output structure.  
    Default is 'Im'.  
    'FieldNameX' - X center field name in output structure.  
    Default is 'CenterX'.  
    'FieldNameY' - Y center field name in output structure.  
    Default is 'CenterY'.  
    Output : - A structure array in which each element contains a  
    sub image (stored in the .Im field).  
    - Four column matrix of list of blocks in image  
    [Xmin, Xmax, Ymin, Ymax].  
    - Four column matrix of list of block centers [Xcen, Ycen].  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Sep 2019  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example:  
    [Sub,ListEdge,ListCenter]=imUtil.partition.image_partitioning(Image,[256 256]);  
    Reliable:  
      
      
      
### imUtil.partition.interp_sparse2full

Interpolate sparse image to a full image OBSOLETE: use imUtil.image.sparse2full instead. Package: imUtil.partition Description:


    
    Interpolate sparse image to a full image  
    OBSOLETE: use imUtil.image.sparse2full instead.  
    Package: imUtil.partition  
    Description:  
    Input  : - Matrix of X poisitions.  
    - Matrix of Y poisitions.  
    - Image.  
    - Output size [X,Y]  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    Output : - Interpolated full image.  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Sep 2019  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example:  
    Reliable:  
      
      
### imUtil.partition.subimage_boundries

Define boundries (ccdsec) for image subsections. Package: imUtil.partition Description: Given image size, block size or number of blocks divide define the bounderies of sub blocks.


    
    Define boundries (ccdsec) for image subsections.  
    Package: imUtil.partition  
    Description: Given image size, block size or number of blocks divide  
    define the bounderies of sub blocks.  
    Input  : - Image size [X,Y]. Default is [2048 4096].  
    - A two element vector defines the block size. A positive number  
    is interpreted as a block size, while negaive number is  
    interpreted as the number of blocks.  
    Default is [-1 -1].  
    - Buffer size to add around blocks. Default is 0.  
    - A parameter indicating what to do if a whole number of blocks  
    can not be fitted into the image. Options are:  
    'simple' -  
    Output : - Four column matrix of list of blocks in image  
    [Xmin, Xmax, Ymin, Ymax].  
    - Four column matrix of list of block centers [Xcen, Ycen].  
    License: GNU general public license version 3  
    Tested : Matlab R2015b  
    By : Eran O. Ofek                    Feb 2016  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [ListEdge,ListCenter]=imUtil.partition.subimage_boundries([2048 4096],[256 256],10,'simple');  
    Reliable: 2  
      
      
