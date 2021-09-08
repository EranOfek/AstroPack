# Package: imUtil.sources


### imUtil.sources.connected_prop

Package: imUtil.sources Description:


    
      
    Package: imUtil.sources  
    Description:  
    Input  : - A matrix (2D image).  
    Alternatively, this can be a structure (or an imCl object)  
    with image, background and variance fields. Field names can be  
    changed using the 'ImageField', 'BackField', and 'VarField'  
    arguments.  
    * Pairs of ...,key,val,... The following keywords are available:  
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
    'ForcedList' - An [X,Y] coordinates on which to perform forced  
    photometry measurments.  
    Forced photometry requestes have TEMP_ID=NaN.  
    Rounded coordinates must be within image boundries.  
    Default is [].  
    'OnlyForced' - A logical flag indicating if to run only forced  
    photometry.  
    Default is false.  
    'BackIm' - A background image. If provided, will overrid the  
    Back field in the input.  
    If empty, and background is not provided, then it will  
    be calculated using the imUtil.background.background  
    function.  
    Default is [].  
    'VarIm' - A variance image. If provided, will overrid the  
    Var field in the input.  
    If empty, and variance is not provided, then it will  
    be calculated using the imUtil.background.background  
    function.  
    Default is [].  
    'BackPar' - A cell array of additional parameters to pass to  
    the imUtil.background.background function.  
    Default is {}.  
    'MomPar' - A cell array of additional parameters to pass to  
    the imUtil.image.moment2 function.  
    Default is {}.  
    'OutType' - Output type. Options are:  
    'mat' - a matrix.  
    'table' - a table.  
    'catCl' - An catCl object.  
    Default is 'catCl'.  
    'ColCell' - A cell array of column names to generate in the  
    output.  
    Default is  
    {'XPEAK','YPEAK','TEMP_ID','SN','FLUX_CONV','BACK_IM','VAR_IM',...  
    'X', 'Y',...  
    'X2','Y2','XY',...  
    'FLUX_APER', 'APER_AREA', 'FLUX_BOX','BACK_ANNULUS', 'STD_ANNULUS', 'FLUX_WAPER'});  
    'Conn' - Connectivity parameter for local maxima  
    identification.  
    Default is 8.  
    'ImageField' - Image field. Default is 'Im'.  
    'BackField' - Background field. Default is 'Back'.  
    'VarField' - Variance field. Default is 'Var'.  
    Output : - A catalog of sources and their properties.  
    Forced photometry requestes will have TEMP_ID=NaN.  
    - A cell array of column names in the output catalog.  
    - A structure with additional calculated output (e.g., the  
    filtered image).  
    License: GNU general public license version 3  
    Tested : Matlab R2015b  
    By : Eran O. Ofek                    Apr 2016  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [Cat,ColCell,Res]=imUtil.sources.find_sources(I1.Im,'Threshold',5)  
    Reliable: 2  
      
      
### imUtil.sources.find_crHT

Find cosmic rays in a catalog using hypothesis testing Package: +imUtil.sources Description:


    
    Find cosmic rays in a catalog using hypothesis testing  
    Package: +imUtil.sources  
    Description:  
    Input  : - A catalog. A matrix or a single element catCl object.  
    - Column index or column name in which the H0 hypothesis is  
    stored. For CR detection this is the S/N for a delta function.  
    - Column index or column name in which the H1 hypothesis is  
    stored. For CR detection this is the S/N for a PSF.  
    - Threshold for CR detection ((H0-H1)>Threshold). Default is 0.  
    Output :  
    Example: FlagCR=imUtil.sources.find_crHT(Cat,ColH0,ColH1,Threshold)  
      
### imUtil.sources.find_sources

find sources in an image Package: imUtil.sources Description: Find sources in an image using a matched filter of template bank and calculate basic properties for all the sources


    
    find sources in an image  
    Package: imUtil.sources  
    Description: Find sources in an image using a matched filter of template  
    bank and calculate basic properties for all the sources  
    including first and second moment, aperture photometry, and  
    PSF flux (using convolution).  
    The program can filter the image simultanously with multiple  
    filters, and the local maxima will be selected in the  
    maximum of all the filtered images.  
    Input  : - A matrix (2D image).  
    Alternatively, this can be a structure (or an imCl object)  
    with image, background and variance fields. Field names can be  
    changed using the 'ImageField', 'BackField', and 'VarField'  
    arguments.  
    * Pairs of ...,key,val,... The following keywords are available:  
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
    'BackIm' - A background image. If provided, will overrid the  
    Back field in the input.  
    If empty, and background is not provided, then it will  
    be calculated using the imUtil.background.background  
    function.  
    Default is [].  
    'VarIm' - A variance image. If provided, will overrid the  
    Var field in the input.  
    If empty, and variance is not provided, then it will  
    be calculated using the imUtil.background.background  
    function.  
    Default is [].  
    'BackPar' - A cell array of additional parameters to pass to  
    the imUtil.background.background function.  
    Default is {}.  
    'MomPar' - A cell array of additional parameters to pass to  
    the imUtil.image.moment2 function.  
    Default is {}.  
    'OutType' - Output type. Options are:  
    'mat' - a matrix.  
    'table' - a table.  
    'catCl' - An catCl object.  
    Default is 'catCl'.  
    'ColCell' - A cell array of column names to generate in the  
    output.  
    Default is  
    {'XPEAK','YPEAK','TEMP_ID','SN','FLUX_CONV','BACK_IM','VAR_IM',...  
    'X', 'Y',...  
    'X2','Y2','XY',...  
    'FLUX_APER', 'APER_AREA', 'FLUX_BOX','BACK_ANNULUS', 'STD_ANNULUS', 'FLUX_WAPER'});  
    'Conn' - Connectivity parameter for local maxima  
    identification.  
    Default is 8.  
    'ImageField' - Image field. Default is 'Im'.  
    'BackField' - Background field. Default is 'Back'.  
    'VarField' - Variance field. Default is 'Var'.  
    Output : - A catalog of sources and their properties.  
    Forced photometry requestes will have TEMP_ID=NaN.  
    - A cell array of column names in the output catalog.  
    - A structure with additional calculated output (e.g., the  
    filtered image).  
    License: GNU general public license version 3  
    Tested : Matlab R2015b  
    By : Eran O. Ofek                    Apr 2016  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [Cat,ColCell,Res]=imUtil.sources.find_sources(I1.Im,'Threshold',5)  
    Im=imUtil.kernel2.gauss(2,[128 128]);  
    Im=Im.*1000 +randn(size(Im));  
    [Cat,ColCell,Res]=imUtil.sources.find_sources(Im,'Threshold',5);  
    Reliable: 2  
      
      
