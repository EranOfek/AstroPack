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
        
    end
   
    
    % estimate background if not exist
    FlagCalcBack = AI.isemptyImage('Back') | Args.ReCalcBack; 
    if any(FlagCalcBack)
        AI(FlagCalcBack) = imProc.background.background(AI(FlagCalcBack), Args.backgroundArgs{:});
    end
    
    % find bright sources for PSF
    [ResSt] = imUtil.sources.findSources(Obj(Iobj).Image, 'Threshold',Args.ThresholdPSF,...
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
    
    % find all sources using PSF + delta + extended
    
    % iterations
    
    % measure sources
    
    
    
    
    
    
    
    
    
    
    
    
    
end