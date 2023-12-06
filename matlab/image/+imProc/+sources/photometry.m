function AI=photometry(AI, Args)
    %
    
    arguments
        AI AstroImage
        Args.ReCalcBack logical        = false;
        Args.backgroundArgs cell       = {};
        
        Args.RadiusPSF                 = 9;
        Args.image2cutoutsArgs cell    = {};
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
        Args.NighRadius                = 5;     % if empty skip
        Args.MinNumGoodPsf             = 5;
        
        Args.SubBack logical           = false;
        Args.SubAnnulusBack logical    = true;
    end
   
    
    % estimate background if not exist
    FlagCalcBack = AI.isemptyImage('Back') | Args.ReCalcBack; 
    if any(FlagCalcBack)
        AI(FlagCalcBack) = imProc.background.background(AI(FlagCalcBack), Args.backgroundArgs{:});
    end
    
    Nobj = numel(Obj);
    for Iobj=1:1:Nobj
        % check if PSF already exist
        
        
        % find bright sources for PSF and construct PSF
        [Result, MeanPSF, VarPSF, NimPSF] = imUtil.psf.constructPSF(Obj(Iobj).Image)
        
        
        
    
        
        
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
                                                              'SortByY',true,...
                                                              'OutType','struct',...
                                                              'BackField','Back',...
                                                              'VarField','Var');
    
        % Cube of sources
        [Cube, RoundX, RoundY, X, Y] = imUtil.cut.image2cutouts(Obj(Iobj).Image, FindSrcSt.XPEAK, FindSrcSt.YPEAK, Args.RadiusPSF, Args.image2cutoutsArgs{:});

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
            MedSigma    = imUtil.background.modeVar_LogHist(Sigma(FlagSN));
            FlagGoodPsf = FlagGoodPsf & (Sigma>(MedSigma - Args.DeltaSigma) & Sigma<(MedSigma + Args.DeltaSigma));
        end
        % select by neighboors
        if ~isempty(Args.NighRadius)
            [MatchedInd] = VO.search.search_sortedY_multi([FindSrcSt.XPEAK, FindSrcSt.YPEAK], FindSrcSt.XPEAK, FindSrcSt.YPEAK, Args.NighRadius);
            FlagGoodPsf    = FlagGoodPsf & [MatchedInd.Nmatch]==1;
        end
        % FlagGoodPsf contains the good sources
        NgoodPsf = sum(FlagGoodPsf);
                
        
        
        % subtract background from cube is done as part of the PSF
        % construction
         % construct PSF
        if NgoodPsf<Args.MinNumGoodPsf
            % No PSF
            MeanPSF = [];
            VarPSF  = [];
            NimPSF  = 0;
        else
            XY = [FindSrcSt.XPEAK, FindSrcSt.YPEAK];
            XY = XY(FlagGoodPsf,:);
            if Args.SubBack
                Back = Obj(Iobj).Back;
            else
                Back = [];
            end
            % need to add multiply by cosine bell...
            [MeanPSF, VarPSF, NimPSF, FlagSelected] = imUtil.psf.constructPSF_cutouts(Obj(Iobj).Image, XY,...
                                                        'Back',Back,...
                                                        'SubAnnulusBack',Args.SubAnnulusBack, Args)
        end
        
%         if Args.BackPsfUseAnnulus
%             % calculate background in annulus
%             [Back, Std] = imUtil.sources.backgroundCube(Cube, Args.backgroundCubeArgs{:});
%         else
%             % use background from Back image
%             Back = Obj(Iobj).getImageVal(FindSrcSt.XPEAK, FindSrcSt.YPEAK, 'DataProp',{'Back'});
%         end
%         % subtract background from cube
%         Cube = Cube - Back;
        
       




        % find all sources using PSF + delta + extended

    
    % iterations
    
    % measure sources
    
    
    
    
    
    
    
    
    
    
    
    
    
end