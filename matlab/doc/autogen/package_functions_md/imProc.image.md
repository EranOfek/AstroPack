# Package: imProc.image


### imProc.image.cutouts

Break a single image to a cube of cutouts around given positions including optional sub pixel shift. Uses ImageComponent/cutouts.


    
    Break a single image to a cube of cutouts around given positions  
    including optional sub pixel shift.  
    Uses ImageComponent/cutouts.  
    Input  : - A single element AstroImage object.  
    - A two column matrix of [X, Y] positions around  
    which to generate the cutouts.  
    * ...,key,val,...  
    'HalfSize' - Cutout half size (actual size will be  
    1+2*HalfSize. Default is 8.  
    'PadVal' - padding value for cutouts near edge or  
    without circular shifts.  
    'CutAlgo' - Algorithm: ['mex'] | 'wmat'.  
    'IsCircle' - If true then will pad each cutout  
    with NaN outside the HalfSize radius.  
    Default is false.  
    'Shift' - A logical indicating if to shift  
    'ShiftAlgo' - Shift algorithm ['lanczos3'] |  
    'lanczos2' | 'fft'.  
    'IsCircFilt' - While using lanczos, is circshift  
    is circular or not. Default is false.  
    'DataPropIC' - Data property inside ImageComponent,  
    from which to extract  
    the cutouts. Default is 'Image'.  
    'DataProp' - Data property from which to extract  
    the cutouts. Default is 'ImageData'.  
    Outout : - A cube of size 1+2*HalfSize X 1+2*HalfSize X  
    numberOfCutouts. each layer contain a cutout  
    and 3rd dim is for cutout index.  
    - A two column matrix of the actual positions  
    [X,Y], around which the cutouts are extracted.  
    These may be rounded if 'RoundPos' is set to true.  
    Author : Eran Ofek (Apr 2021)  
    Example: IC = ImageComponent({rand(1000,1000)});  
      
    Example: AI=AstroImage({rand(1000,1000)});  
    XY = rand(10000,2).*900 + 50;  
    Cube = imProc.image.cutouts(AI, XY);  
    Cube = imProc.image.cutouts(AI, XY,'Shift',true);  
    Cube = imProc.image.cutouts(AI, XY,'Shift',true,'IsCircFilt',true);  
      
### imProc.image.image2subimages

Partition an AstroImage image into sub images


    
    Partition an AstroImage image into sub images  
    Input  : - An AstroImage object with a single element.  
    - BlockSize [X, Y] of sub images. or [X] (will be copied as [X, X]).  
    If empty, will use imUtil.image.subimage_grid  
    Default is [256 256].  
    * Arbitrary number of pairs of input arguments ...,key,val,...  
    The following keywords are available:  
    'Output' - Output type {['cell'] | 'struct'}  
    'FieldName' - Field name in a struct output in which to store  
    the sub images. Default is 'Im'.  
    'SubSizeXY' - Sub image size [X,Y]. Default is [128 128].  
    'CCDSEC' - CCDSEC of image to partition. If empty,  
    use full image. Default is empty.  
    'Nxy' - Number of sub images along each dimension [Nx, Ny].  
    If empty then use SubSizeXY. Default is [].  
    'OverlapXY' - Overlapping extra [X, Y] to add to SubSizeXY  
    from each side. Default is [32 32].  
    'UpdateMask' - A logical indicating if to update the mask  
    image with near edge bit and overlap bit.  
    Default is true.  
    'EdgeDist' - Distance from edge, which to flag as a near  
    edge pixel. Default is 10 pix.  
    'NearEdge_BitName' - NearEdge bit name.  
    Deafult is 'NearEdge'.  
    'Overlap_BitName' - Overlap bit name.  
    Default is 'Overlap'.  
    'BitDict' - Bit dictionary to create, if not exits yet.  
    Default is BitDictionary('BitMask.Image.Default').  
    'UpdateCat' - A logical indicating if to update the  
    CatalogData. Including:  
    crop sources outside image.  
    Default is true.  
    'UpdateXY' - A logical indicating if to update the X/Y coordinates  
    according to the new image boundries.  
    Default is true.  
    'ColX' - A cell array of X column dictionary names by  
    which to perform the catalog cropping, and  
    shifting.  
    Default is AstroCatalog.DefNamesX.  
    'ColY' - Like 'ColX', but for the Y axis.  
    Default is AstroCatalog.DefNamesY.  
    'AddX' - A cell array of additional X column names to  
    shift. Default is {}.  
    'AddY' - Like 'AddX', but for the Y-axis. Default is {}.  
    Output : - An AstroImage of sub images.  
    - A structure with CCDSEC info, including:  
    EdgesCCDSEC  
    ListCenters  
    NoOverlapCCDSEC  
    NewNoOverlap  
    Author : Eran Ofek (May 2021)  
    Example: AI = AstroImage({rand(1024, 1024)},'Back',{rand(1024, 1024)});  
    AI.HeaderData.insertKey({'A',1});  
    Result = imProc.image.image2subimages(AI,[256 256])  
      
### imProc.image.images2cube

Convert the images in AstroImage object into a cube. Each data property (e.g., 'ImageData', 'BackData') produce a cube.


    
    Convert the images in AstroImage object into a cube.  
    Each data property (e.g., 'ImageData', 'BackData')  
    produce a cube.  
    Input  : - An array of AstroImage objects.  
    * ...,key,val,...  
    'CCDSEC' - A 4 column matrix of CCDSEC of each  
    image in ImageComponent to insert into the  
    cube [Xmin, Xmax, Ymin, Ymax].  
    If single line, then use the same CCDSEC  
    for all images. If empty, use entore image.  
    Default is [].  
    'DataPropIn' - Data property, in ImageComponent,  
    from which to take  
    the image. Default is 'Image'.  
    'DimIndex' - Cube dimension of the image index.  
    Either 1 or 3. Default is 3.  
    'DataProp' - The data properties for which the  
    cubes will be calculated.  
    Default is {'ImageData','BackData',  
    'VarData', 'MaskData'}.  
    Output : * A cube for each DataProp, by the order of their  
    appearnce in DataProp.  
    Author : Eran Ofek (Apr 2021)  
    Notes  : Doing this operation directly (without  
    astroImage2ImageComponent) will be only a few percents  
    faster.  
    Example: AI = AstroImage({rand(1000,1000), rand(1000,1000), rand(1000,1000)})  
    [CubeImage, CubeBack] = imProc.image.images2cube(AI)  
    [CubeImage] = imProc.image.images2cube(AI,'CCDSEC',[1 2 2 5])  
      
### imProc.image.interpOverNan

interpolate AstroImage over NaN values


    
    interpolate AstroImage over NaN values  
    Input  : - An AstroImage object.  
    * ...,key,val,...  
    'Method' - Interpolation method.  
    Default is 'inpaint_nans'.  
    'MethodInpaint' - inpaint_nans method. Default is 0.  
    See inpaint_nans for options.  
    'DataProp' - A cell array of data properties on which to operate the  
    interpolation. Default is {'Image'}.  
    'MaskInterpolated' - A logical indicating if to mark  
    interpolated data in the Mask image.  
    Default is true.  
    'BitNameInterpolated' - BitName to mark interpolated data.  
    Default is 'Interpolated'.  
    'CreateNewObj' - A logical indicating if to copy the input  
    object. Default is false.  
    Outout : - An AstroImage object with interpolation over NaNs  
    Author : Eran Ofek (Jul 2021)  
    Example: AI = AstroImage({ones(100,100)});  
    AI.Image(50,50:51)=NaN;  
    AI.Image(70,70) = NaN;  
    imProc.image.interpOverNan(AI);  
      
### imProc.image.unitTest

unitTest for the +imProc.image package


    
    unitTest for the +imProc.image package  
