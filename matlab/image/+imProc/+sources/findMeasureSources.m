function Result = findMeasureSources(Obj, Args)
    % Basic sources finder and measurments on AstroImage object.
    %   This function uses the +imUtil.sources.find_sources function.
    % Input  : - An AstroImage object (multi elements are supported).
    %            'ReFind' - A logical indicating if to find stars if the
    %                   catalog is already populated. Default is true.
    %            'Threshold' - Detection threshold above background in units of
    %                   the background std. 
    %                   Default is 5.
    %            'Psf' - A PSF stamp or a cube of PSFs. If a cube then the PSF
    %                   index is the third dimesnion.
    %                   The input image will be filtered with each PSF and 
    %                   local maxima will be searched in all the filtered
    %                   images.
    %                   If provided, this parameter overrid the PsfFun input
    %                   argument.
    %                   Default is [].
    %            'PsfFun' - A function handle to generate PSF or a cube of
    %                   PSFs.
    %                   Default is @imUtil.kernel2.gauss.
    %            'PsfFunPar' - A cell array of parameters to pass to the PsfFun
    %                   function.
    %                   Default is {[0.1;1.5;3]} (i.e., will generate a cuve of
    %                   templates with Gaussian PSF with sigmas of 0.1, 1.5 and
    %                   3 pixels).
    %            'RemoveEdgeDist' - Indicating if to remove sources which
    %                   XPEAK/YPEAK are near the image edges.
    %                   If NaN, then no sources are removed. Otherwise, this is
    %                   the distance in pixels from the edge to remove the
    %                   sources. For example, 0 will remove sources located
    %                   exactly at the edge pixels.
    %                   Default is 0.
    %            'ForcedList' - An [X,Y] coordinates on which to perform forced
    %                   photometry measurments.
    %                   Forced photometry requestes have TEMP_ID=NaN.
    %                   Rounded coordinates must be within image boundries.
    %                   Default is [].
    %            'OnlyForced' - A logical flag indicating if to run only forced
    %                   photometry.
    %                   Default is false.
    %            'BackPar' - A cell array of additional parameters to pass to
    %                   the imProc.image.background function.
    %                   Default is {}.
    %            'MomPar' - A cell array of additional parameters to pass to
    %                   the imUtil.image.moment2 function.
    %                   Default is {}.
    %            'Conn' - Connectivity parameter for local maxima
    %                   identification.
    %                   Default is 8.
    %            'ColCell' - A cell array of column names to generate in the
    %                   output.
    %                   Default is
    %                   {'XPEAK','YPEAK','TEMP_ID','SN','FLUX_CONV','BACK_IM','VAR_IM',...           
    %                                            'X', 'Y',...
    %                                            'X2','Y2','XY',...
    %                                            'FLUX_APER', 'APER_AREA', 'FLUX_BOX','BACK_ANNULUS', 'STD_ANNULUS', 'FLUX_WAPER', ...
    %                                            'FLUXERR_APER',...
    %                                            'MAG_APER', 'MAGERR_APER', 'BACKMAG_ANNULUS',...
    %                                            'MAG_CONV'};
    %            'CreateNewObj' - Indicating if the output
    %                   is a new copy of the input (true), or an
    %                   handle of the input (false).
    %                   If empty (default), then this argument will
    %                   be set by the number of output args.
    %                   If 0, then false, otherwise true.
    %                   This means that IC.fun, will modify IC,
    %                   while IB=IC.fun will generate a new copy in
    %                   IB.
    % Output : - An AstroImage object in which the CatData is populated
    %            with sources found in the image.
    % Example: Im=imUtil.kernel2.gauss(2,[128 128]);
    %          Im=Im.*1000 +randn(size(Im));        
    %          AI = AstroImage({Im});
    %          Result = imProc.sources.findMeasureSources(AI);
   
    arguments
        Obj
        Args.ReFind(1,1) logical           = true;
        Args.Threshold                     = 5;
        Args.Psf                           = [];
        Args.PsfFun function_handle        = @imUtil.kernel2.gauss;
        Args.PsfFunPar cell                = {[0.1;1.5;3]};
        Args.RemoveEdgeDist                = 0;  % NaN for non removal
        Args.ForcedList                    = [];
        Args.OnlyForced(1,1) logical       = false;
        
        Args.BackPar cell                  = {};
        
        Args.MomPar cell                   = {};
        Args.Conn                          = 8;
        Args.ColCell cell                  = {'XPEAK','YPEAK','TEMP_ID','SN','FLUX_CONV','BACK_IM','VAR_IM',...           
                                                'X', 'Y',...
                                                'X2','Y2','XY',...
                                                'FLUX_APER', 'APER_AREA', 'FLUX_BOX','BACK_ANNULUS', 'STD_ANNULUS', 'FLUX_WAPER', ...
                                                'FLUXERR_APER',...
                                                'MAG_APER', 'MAGERR_APER', 'BACKMAG_ANNULUS',...
                                                'MAG_CONV'};
        Args.CreateNewObj                  = [];
        
        % hidden
        Args.ImageProp char            = 'ImageData';
        Args.ImagePropIn char          = 'Image';
        Args.BackProp char             = 'BackData';
        Args.BackPropIn char           = 'Image';
        Args.VarProp char              = 'VarData';
        Args.VarPropIn char            = 'Image';
        Args.CatProp char              = 'CatData';
    end
    
    if isempty(Args.CreateNewObj)
        if nargout==0
            Args.CreateNewObj = false;
        else
            Args.CreateNewObj = true;
        end
    end
    if Args.CreateNewObj
        Result = Obj.copyObject;
    else
        Result = Obj;
    end
    
    % calculate background
    imProc.background.background(Result, 'CreateNewObj',false, Args.BackPar{:});
    
    Nobj = numel(Obj);
    for Iobj=1:1:Nobj
        % call the source finder and measurments
        if Args.ReFind || ~isempty(Result(Iobj).(Args.CatProp).Catalog)
            Result(Iobj).(Args.CatProp) = imUtil.sources.find_sources(Result(Iobj).(Args.ImageProp).(Args.ImagePropIn), ...
                                                        'OutType','AstroCatalog',...
                                                        'BackIm',Result(Iobj).(Args.BackProp).(Args.BackPropIn),...
                                                        'VarIm',Result(Iobj).(Args.VarProp).(Args.VarPropIn),...
                                                        'Threshold',Args.Threshold,...
                                                        'Psf',Args.Psf,...
                                                        'PsfFun',Args.PsfFun,...
                                                        'PsfFunPar',Args.PsfFunPar,...
                                                        'RemoveEdgeDist',Args.RemoveEdgeDist,...
                                                        'ForcedList',Args.ForcedList,...
                                                        'OnlyForced',Args.OnlyForced,...
                                                        'MomPar',Args.MomPar,...
                                                        'Conn',Args.Conn,...
                                                        'ColCell',Args.ColCell);
        end
                                                    
                                                        
                                                        
    end
    
end