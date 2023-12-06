function Result = findSources(Obj, Args)
    % Find sources (only) in AstroImage using imUtil.sources.findSources
    % Input  : - An AstroImage object
    %            'Threshold' - Detection threshold above background in units of
    %                   the background std. 
    %                   Default is 5.
    %            'Psf' - A PSF stamp or a cube of PSFs. If a cube then the PSF
    %                   index is the third dimesnion.
    %                   If empty, then attempt getting the PSF from the
    %                   AstroImage PSFData object.
    %                   If function handle, then use it to generate a pSF
    %                   cube.
    %                   Default is @imUtil.kernel2.gauss
    %                  
    %            'PsfArgs' - A cell array of parameters to pass to the Psf
    %                   if a function_handle.
    %                   Default is {[0.1;1.5;3]} (i.e., will generate a cuve of
    %                   templates with Gaussian PSF with sigmas of 0.1, 1.5 and
    %                   3 pixels).
    %
    %            'ForcedList' - An [X,Y] coordinates on which to perform forced
    %                   photometry measurments.
    %                   Forced photometry requestes have TEMP_ID=NaN.
    %                   Rounded coordinates must be within image boundries.
    %                   Default is [].
    %            'OnlyForced' - A logical flag indicating if to run only forced
    %                   photometry.
    %                   Default is false.
    %
    %            'Conn' - Connectivity parameter for local maxima
    %                   identification.
    %                   Default is 8.
    %            'CleanSources' - A logocal indicating if to remove bad
    %                   sources (delta functions and near edge) using
    %                   imUtil.sources.cleanSources
    %                   Default is false.
    %            'cleanSourcesArgs' - A cell array of additional args to
    %                   pass to imUtil.sources.cleanSources.
    %                   Default is {}.
    %            'CreateNewObj' - A logical indicating if to copy the
    %                   content of the CatData object. Default is false.
    % Output : - An AstroImage object with the sources found in the CatData
    %            object.
    %            The catalog include the following columns:
    %            'XPEAK', 'YPEAK' - X and Y whole pixel position
    %            'TEMP_ID' - The template index that maximizes the S/N.
    %            'SN_%d - S/N for each template.
    %            'FLUX_CONV_%d - convolution based PSF flux for each
    %                   template.
    %            'BACK_IM', 'VAR_IM' - Background and variance at source
    %                   position as read from the Back and Var images.
    % Author : Eran Ofek (Jan 2022)
    % Example: AI=AstroImage('PTF_201411204943_i_p_scie_t115144_u023050379_f02_p100037_c02.fits');
    %          AI=imProc.background.background(AI);
    %          AI = imProc.sources.findSources(AI)
    
    arguments
        Obj AstroImage        
        Args.Threshold                     = 5;
        Args.Psf                           = @imUtil.kernel2.gauss;
        Args.PsfArgs cell                  = {[0.1;1.5;3]};
        Args.ForcedList                    = [];
        Args.OnlyForced(1,1) logical       = false;
    
        Args.Conn                          = 8;
        Args.CleanSources logical          = false;
        Args.cleanSourcesArgs cell         = {};
        
        Args.CreateNewObj logical          = false;  % only for CatData
    end
    
    Result = Obj;
    
    if any(isemptyImage(Obj, {'Image','Back','Var'}),'all')
        error('Image, Back, and Var properties must be not empty - run imProc.background.background');
    end    
    
    Nobj = numel(Obj);
    for Iobj=1:1:Nobj
        
        if Args.CreateNewObj
            Result(Iobj).CatData = Obj(Iobj).CatData.copy;
        end
        
        if isempty(Args.Psf)
            % attempt using the PSF object
            Psf = Obj(Iobj).PSFData.getPSF;
        else
            if isa(Args.Psf, 'function_handle')
                Psf = Args.Psf(Args.PsfArgs{:});
            else
                Psf = Args.Psf;
            end
        end
        Npsf = size(Psf,3);
        
                
        [ResSt] = imUtil.sources.findSources(Obj(Iobj).Image, 'Threshold',Args.Threshold,...
                                                              'Psf',Psf,...
                                                              'ForcedList',Args.ForcedList,...
                                                              'OnlyForced',Args.OnlyForced,...
                                                              'BackIm',Obj(Iobj).Back,...
                                                              'VarIm',Obj(Iobj).Var,...
                                                              'Conn',Args.Conn,...
                                                              'CleanSources',Args.CleanSources,...
                                                              'cleanSourcesArgs',Args.cleanSourcesArgs,...
                                                              'OutType','struct',...
                                                              'BackField','Back',...
                                                              'VarField','Var');
        % put ResSt in CatData
        Result(Iobj).CatData.Catalog = [ResSt.XPEAK, ResSt.YPEAK, ResSt.TEMP_ID, ResSt.SN, ResSt.FLUX_CONV, ResSt.BACK_IM, ResSt.VAR_IM];
        
        SN_Cell   = tools.cell.cellNumericSuffix('SN', (1:1:Npsf));
        Flux_Cell = tools.cell.cellNumericSuffix('FLUX_CONV', (1:1:Npsf));
        Result(Iobj).CatData.ColNames = [{'XPEAK', 'YPEAK', 'TEMP_ID'}, SN_Cell, Flux_Cell, {'BACK_IM', 'VAR_IM'}];
        
        
    end
end
