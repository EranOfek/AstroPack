function AI=photometry(AI, Args)
    %
    
    arguments
        AI AstroImage
        Args.ReCalcBack logical      = false;
        Args.backgroundArgs cell     = {};
        
        Args.Threshold               = 5;
        Args.ThresholdPSF            = 20;
        Args.InitPsf                 = @imUtil.kernel2.gauss
        Args.InitPsfArgs cell        = {[0.1;2]};
        Args.Conn                    = 8;
        Args.CleanSources            = true;
        Args.cleanSourcesArgs cell   = {};
        Args.backgroundCubeArgs cell = {};
    end
   
    
    % estimate background if not exist
    FlagCalcBack = AI.isemptyImage('Back') | Args.ReCalcBack; 
    if any(FlagCalcBack)
        AI(FlagCalcBack) = imProc.background.background(AI(FlagCalcBack), Args.backgroundArgs{:});
    end
    
    Nobj = numel(Obj);
    for Iobj=1:1:Nobj
        % find bright sources for PSF
        [FindSrcSt] = imUtil.sources.findSources(Obj(Iobj).Image, 'Threshold',Args.ThresholdPSF,...
                                                              'Psf',Args.InitPsf,...
                                                              'PsfArgs',Args.InitPsfArgs,...
                                                              'ForcedList',[],...
                                                              'OnlyForced',false,...
                                                              'BackIm',AI(Iobj).Back,...
                                                              'VarIm',AI(Iobj).Var,...
                                                              'Conn',Args.Conn,...
                                                              'CleanSources',Args.CleanSources,...
                                                              'cleanSourcesArgs',Args.cleanSourcesArgs,...
                                                              'OutType','struct',...
                                                              'BackField','Back',...
                                                              'VarField','Var');
    
        % Select sources for PSF
        % select by SN diff
        
        % select by moments
        % select by neighboors
        
        
        % Cube of sources
        [Cube, RoundX, RoundY, X, Y] = imUtil.cut.image2cutouts(Obj(Iobj).Image, FindSrcSt.XPEAK, FindSrcSt.YPEAK, MaxRadius, Args)

        % subtract background from cube
        if Args.BackPsfUseAnnulus
            % calculate background in annulus
            [Back, Std] = imUtil.sources.backgroundCube(Cube, Args.backgroundCubeArgs{:});
        else
            % use background from Back image
            Back = Obj(Iobj).getImageVal(FindSrcSt.XPEAK, FindSrcSt.YPEAK, 'DataProp',{'Back'});
        end
        % subtract background from cube
        Cube = Cube - Back;
        
        
        % construct PSF
        [Mean, Var, Nim, FlagSelected] = constructPSF_cutouts(Image, XY, Args)



        % find all sources using PSF + delta + extended

    
    % iterations
    
    % measure sources
    
    
    
    
    
    
    
    
    
    
    
    
    
end