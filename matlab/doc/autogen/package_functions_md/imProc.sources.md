# Package: imProc.sources


### imProc.sources.classifySources

Classify sources found by findMeasureSources. The source classification includes: Example: [Result, Flag] = classifySources(Obj)


    
    Classify sources found by findMeasureSources.  
    The source classification includes:  
      
    Example: [Result, Flag] = classifySources(Obj)  
      
      
### imProc.sources.cleanSources

Clean sources found by findMeasureSources (bad S/N and CR). Cleaning include - flaging or removal of CRs based on hypothesis testing between a delta function and some PSF, and finding sources which S/N based on the annulus std is low (i.e. identify cases in which the local background/variance estimation is not reliable).


    
    Clean sources found by findMeasureSources (bad S/N and CR).  
    Cleaning include - flaging or removal of CRs based on hypothesis  
    testing between a delta function and some PSF, and finding sources  
    which S/N based on the annulus std is low (i.e. identify cases in  
    which the local background/variance estimation is not reliable).  
    Input  : - An Astrocatalog or AstroImage (w/ Astrocatalog) object.  
    The AstroCatalog must contains at least two S/N columns  
    measured using two PSF matched filters,  
    an 'STD_ANNULUS' column of the std measured in an annulus  
    around each source, and at least one flux column.  
    * ...,key,val,...  
    'ColNamsSN' - A cell array of at least two column names containg the  
    S/N measured for PSFs with different width.  
    The first column name must corresponds to a sharp  
    PSF (i.e., delta function), and the second column  
    is for some wider PSF.  
    The CR flag is calculated by SN(1)>SN(2).  
    Default is {'SN_1','SN_2'}.  
    'ColNameFlux' - A cell array of column names containing  
    some flux measurments. These, along with the annulus  
    std, are used to calculate the annulus-std-based  
    S/N. Default is {'FLUX_CONV_2','FLUX_CONV_3'}.  
    'SigmaPSF' - The width of the PSF (i.e., unit of sigma).  
    This is a vector which must have the same number of  
    elements as ColNameFlux. This is used to calculate  
    the PSF effective area (4\pi\sigma^2).  
    Default is [].  
    'ColNameStdAnn' - Annulus std column name.  
    Default is 'STD_ANNULUS'.  
    'ThresholdSN' - S/N threshold. annulus-std S/N smaller than  
    this threshold are declared as bad.  
    Default is 5.  
    'MaskCR' - A logical indicating if to update the mask  
    image with cosmic rays found using 'CR_DeltaHT'.  
    This will be activated only if the input is an  
    AstroImage. Default is true.  
    'BitNameCR' - A bit name in the Mask image for flagging  
    the CR (cosmic rays). Default is 'CR_DeltaHT'.  
    'RemoveBadSources' - A logical indicating if to remove bad  
    sources from the output catalog. Default is true.  
    'CreateNewObj' - A logical indicating if to create a new  
    copy of (only) the AstroCatalog object.  
    Default is false.  
    Output : - An AstroCatalog/AstroImage with the updated catalog.  
    - A structure array with the flaged sources. Abailable  
    fields are:  
    .CR - possible CR.  
    .BadSN - std-annulud-based S/N smaller than threshold.  
    Author : Eran Ofek (Oct 2021)  
    Example: [Result, Flag] = imProc.sources.cleanSources(AI,'SigmaPSF',[1.2 1.5])  
      
      
### imProc.sources.findMeasureSources

Basic sources finder and measurments on AstroImage object. This function uses the +imUtil.sources.find_sources function.


    
    Basic sources finder and measurments on AstroImage object.  
    This function uses the +imUtil.sources.find_sources function.  
    Input  : - An AstroImage object (multi elements are supported).  
    'RemoveBadSources' - A logical indicating if to remove  
    bad sources using imProc.sources.cleanSources.  
    This will work only if the following columns are requested  
    'SN_1','SN_2','FLUX_CONV_2','FLUX_CONV_3','STD_ANNULUS'.  
    Default is false.  
    'ReFind' - A logical indicating if to find stars if the  
    catalog is already populated. Default is true.  
    'Threshold' - Detection threshold above background in units of  
    the background std.  
    Default is 5.  
    'Psf' - A PSF stamp or a cube of PSFs. If a cube then the PSF  
    index is the third dimesnion.  
    The input image will be filtered with each PSF and  
    local maxima will be searched in all the filtered  
    images.  
    If provided, this parameter overrid the PsfFun input  
    argument.  
    Default is [].  
    'PsfFun' - A function handle to generate PSF or a cube of  
    PSFs.  
    Default is @imUtil.kernel2.gauss.  
    'PsfFunPar' - A cell array of parameters to pass to the PsfFun  
    function.  
    Default is {[0.1;1.5;3]} (i.e., will generate a cuve of  
    templates with Gaussian PSF with sigmas of 0.1, 1.5 and  
    3 pixels).  
    'RemoveEdgeDist' - Indicating if to remove sources which  
    XPEAK/YPEAK are near the image edges.  
    If NaN, then no sources are removed. Otherwise, this is  
    the distance in pixels from the edge to remove the  
    sources. For example, 0 will remove sources located  
    exactly at the edge pixels.  
    Default is 0.  
    'ForcedList' - An [X,Y] coordinates on which to perform forced  
    photometry measurments.  
    Forced photometry requestes have TEMP_ID=NaN.  
    Rounded coordinates must be within image boundries.  
    Default is [].  
    'OnlyForced' - A logical flag indicating if to run only forced  
    photometry.  
    Default is false.  
    'BackPar' - A cell array of additional parameters to pass to  
    the imProc.image.background function.  
    Default is {}.  
    'ReCalcBack' - A logical indicating if to recalculate  
    background, even if it is already exist in the  
    AstroImage. Default is false.  
    'MomPar' - A cell array of additional parameters to pass to  
    the imUtil.image.moment2 function.  
    Default is {}.  
    'Conn' - Connectivity parameter for local maxima  
    identification.  
    Default is 8.  
    'ColCell' - A cell array of column names to generate in the  
    output.  
    Default is  
    {'XPEAK','YPEAK','TEMP_ID','SN','FLUX_CONV','BACK_IM','VAR_IM',...  
    'X', 'Y',...  
    'X2','Y2','XY',...  
    'FLUX_APER', 'APER_AREA', 'FLUX_BOX','BACK_ANNULUS', 'STD_ANNULUS', 'FLUX_WAPER', ...  
    'FLUXERR_APER',...  
    'MAG_APER', 'MAGERR_APER', 'BACKMAG_ANNULUS',...  
    'MAG_CONV'};  
    'CreateNewObj' - Indicating if the output  
    is a new copy of the input (true), or an  
    handle of the input (false).  
    If empty (default), then this argument will  
    be set by the number of output args.  
    If 0, then false, otherwise true.  
    This means that IC.fun, will modify IC,  
    while IB=IC.fun will generate a new copy in  
    IB.  
    Output : - An AstroImage object in which the CatData is populated  
    with sources found in the image.  
    Example: Im=imUtil.kernel2.gauss(2,[128 128]);  
    Im=Im.*1000 +randn(size(Im));  
    AI = AstroImage({Im});  
    Result = imProc.sources.findMeasureSources(AI);  
      
