function AI=photometry(AI, Args)
    %
    
    arguments
        AI AstroImage
        Args.ReCalcBack logical        = false;
        Args.backgroundArgs cell       = {};
        
        Args.Threshold                 = 5;
        Args.ThresholdPSF              = 20;
        Args.InitPsf                   = @imUtil.kernel2.gauss
        Args.InitPsfArgs cell          = {[0.1;2]};
        Args.Conn                      = 8;
        Args.CleanSources              = true;
        Args.cleanSourcesArgs cell     = {};
        Args.backgroundCubeArgs cell   = {};
        
        Args.SNdiff                    = 0;  % if empty skip
        Args.moment2Args cell          = {};
        Args.DeltaSigma                = 0.5;   % if empty skip
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
        Nsrc = numel(FindSrcSt.XPEAK);
        FlagGoodPsf = true(Nsrc,1);
        if ~isempty(Args.SNdiff) && size(FindSrcSt.SN,2)>1
            Flag        = FindSrcSt.SN(:,2) > FindSrcSt.SN(:,1);
            FlagGoodPsf = FlagGoodPsf && Flag;
        end
        % select by moments
        if ~isempty(Args.DeltaSigma)
            [M1, M2]    = imUtil.image.moment2(Cube, FindSrcSt.XPEAK, FindSrcSt.YPEAK, Args.moment2Args{:});
            Sigma       = sqrt(abs(M2.X2)+abs(M2.Y2));
            MedSigma    = imUtil.background.mode(Sigma(FlagSN));
            FlagSig     = Sigma>(MedSigma - Args.DeltaSigma) & Sigma<(MedSigma + Args.DeltaSigma);
            FlagGoodPsf = FlagGoodPsf & FlagSig;
        end
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