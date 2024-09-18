function Result=mextractor(Obj, Args)
    % Multi-iteration PSF fitting and source extractor 
    % Example: imProc.sources.mextractor(AI)
    % 
    arguments
        Obj AstroImage
                       
        % PSF measurement:
        Args.populatePSFArgs cell      = {'CropByQuantile',false};
        Args.ThresholdPSF              = 20;
        Args.RangeSN                   = [50 1000];
        Args.InitPsf                   = @imUtil.kernel2.gauss
        Args.InitPsfArgs cell          = {[0.1;1.0;1.5]};  % PSF measurements
                
        Args.UseInterpolant = false;
        Args.SuppressEdges  = true;
        
        % source detection:
        Args.ReCalcPSF logical         = false;       
        Args.PsfFunPar cell            = {[0.1;1.0;1.5]};  % search for sources                 
        Args.Threshold                 = [30 10 5]; % [50 16.5 5]; % this also specifies the # of iterations
        
%         Args.ThresholdDiffSN         = 0;
        
%         Args.PrelimPsf               = @imUtil.kernel2.gauss;
%         Args.PrelimPsfArgs cell      = {[0.1 2]};
%         Args.PrelimThreshold         = 30;
%         Args.Conn                    = 8;
%         Args.PrelimCleanSrc logical  = true;
%         Args.PrelimCleanSrcArgs cell = {'ColSN_sharp',1, 'ColSN_psf',2, 'SNdiff',0, 'MinEdgeDist',15, 'RemoveBadSources',true};
       
        % background and variance measurement:

        Args.ReCalcBack  = true;
        Args.BackPar     = {'SubSizeXY',[128 128]}; % {'SubSizeXY',[]})

        Args.VarMethod   = 'LogHist';             
        Args.MomRadius   = [4 6 6]; % for each iteration % recommended MomRadius = 1.7 * FWHM ~ 3.8 (for LAST!)
        
        Args.RedNoiseFactor = 1.3; % increase the variance due to the sources found at previous iterations by this factor

%         Args.ReMeasBack logical      = true;       
%         Args.ReBack logical          = false; % remeasure if background exits 

        Args.CreateNewObj logical    = false;               
                       
        % miscellaneous:
        Args.RemoveMasked  = false;  % seems like 'true' does not influence much ?
        Args.RemovePSFCore = false;  % not decided on it as of yet
        
    end
    
    % create a new copy
    if Args.CreateNewObj
        Result = Obj.copy;
    else
        Result = Obj;
    end
    
    % populate background and variance
    FlagBack = Obj.isemptyProperty('Back') | Obj.isemptyProperty('Var');
    if any(FlagBack)
        Obj(FlagBack) = imProc.background.background(Obj(FlagBack), Args.BackPar{:});
    end
    
    % populate PSF
    [Result] = imProc.psf.populatePSF(Result, Args.populatePSFArgs{:},...
                                                      'ThresholdPSF',Args.ThresholdPSF,...
                                                      'RangeSN',Args.RangeSN,...
                                                      'InitPsf',Args.InitPsf,...
                                                      'InitPsfArgs',Args.InitPsfArgs);
                                                              
    % find sources using PSF - multi-iteration
    Niter = numel(Args.Threshold);
    Nobj  = numel(Obj);    
    
    for Iobj=1:1:Nobj
        
        AI              = Result(Iobj).copy;                 % this AI will be iterated
        Cat             = repmat(AstroCatalog,1,Niter);      % catalogs of each iteration
        SourceImage     = repmat(0,size(AI.Image,1),size(AI.Image,2),Niter);    % source image after each iteration
        SubtractedImage = repmat(0,size(AI.Image,1),size(AI.Image,2),Niter);    % subtracted image after each iteration
               
        for Iiter=1:1:Niter
            
            % re-measure background at each iteration > 1           
            if Iiter>1         % add the variance from the local sources from the previous iteration(s)
                imProc.background.background(AI, 'ReCalcBack', Args.ReCalcBack, Args.BackPar{:});
                AI.Var = AI.Var + Args.RedNoiseFactor .* SourceImage(:,:,Iiter-1);
            end

%             if Iiter>1 && Args.ReMeasBack                
%                 % Options:
%                 % use imProc.background.background
%                 % update variance                
%             end
            
            % find sources (without background recalculation) and add them
            % to the catalog (what if there are already some sources in the catalog?)
            AI = imProc.sources.findMeasureSources(AI,'Threshold', Args.Threshold(Iiter),'ReCalcBack',false,...
                    'MomPar',{'MomRadius',Args.MomRadius(Iiter)},'PsfFunPar',Args.PsfFunPar); 

%             % find sources
%             ResSrc(Iobj,Iiter) = imUtil.sources.findSources(SubImage, 'Threshold',Args.Threshold(Iiter),...
%                                                                       'Psf',Result(Iobj).PSFData.getPSF,...
%                                                                       'BackIm',Result(Iobj).Back,...
%                                                                       'VarIm',Result(Iobj).Var,...
%                                                                       'CleanSources',false,...
%                                                                       'AddValAtPos',true);
%             
%             % Clean sources
%             % Use VAL to calculate SN for delta function
%             % SN_delta : S/N for delta function 
%             SN_delta = (ResSrc(Iobj,Iiter).VAL - ResSrc(Iobj,Iiter).BACK_IM)./sqrt(ResSrc(Iobj,Iiter).VAR_IM);
%             SN_diff  = ResSrc(Iobj,Iiter).SN - SN_delta;
%             FlagGood = SN_diff>Args.ThresholdDiffSN;
%             % good stars are in ResSrc(Iobj,Iiter).XPEAK(FlagGood),YPEAK
          
            NumSrc = height(AI.CatData.Catalog);
            fprintf('Iter. %d: mean bkg = %.0f, mean var = %.0f, Nobj: %d\n',...
                Iiter,mean(AI.Back,'all'),mean(AI.Var,'all'),NumSrc);
            
            % insert a column with iteration number into the source catalog
            AI.CatData = insertCol(AI.CatData, repmat(Iiter,1,NumSrc)', Inf, 'ITER', {''});
            
            % measure the PSF (if we believe that the PSF is flux-dependent?) or use the previous one 
            if isempty(AI.PSF) || Args.ReCalcPSF
                AI = imProc.psf.populatePSF(AI,Args.populatePSFArgs{:});                
            end
                
%             % PSF fit sources
%             imUtil.psf.psfPhot(Result(Iobj).Image, 'PSF',Result(Iobj).PSFData.getPSF,...
%                                                    'Xinit',ResSrc(Iobj,Iiter).XPEAK,...
%                                                    'Yinit',ResSrc(Iobj,Iiter).YPEAK,...
%                                                    'PsfPeakVal',ResSrc(Iobj,Iiter).VAL,...
%                                                    'SN',ResSrc(Iobj,Iiter).SN);
            
            % fit the PSF to objects at the sub-pixel level and make PSF photometry
            [AI, Res] = imProc.sources.psfFitPhot(AI);  % produces PSFs shifted to RoundX, RoundY, so there is no need to Recenter
            
            % use interpolation or PSF shift + edge suppression
            if Args.UseInterpolant
                F = griddedInterpolant(AI.PSF,'linear','previous'); %
                Nx = size(AI.PSF,1);
                [X, Y] = meshgrid(1:Nx);
                ShiftedPSF = repmat(0,Nx,Nx,NumSrc);
                for Isrc = 1:NumSrc
                    ShiftedPSF(:,:,Isrc)  = F(X+Res.DX(Isrc),Y+Res.DY(Isrc))';
                end
                ShiftedPSF = ShiftedPSF./sum(ShiftedPSF,[1 2]); % renormalize
            else
                ShiftedPSF = imUtil.psf.suppressEdges(Res.ShiftedPSF, 'Fun',@imUtil.kernel2.cosbell, 'FunPars', [5, 8]);
            end            
    
            % subtract sources:
            % 1. construct a source image
            % 2. subtract the source image from the current image
            [CubePSF, XY]                = imUtil.art.createSourceCube(ShiftedPSF, [Res.RoundY Res.RoundX], Res.Flux, ...
                                                                        'Recenter', false,'PositivePSF',true);
            SourceImage(:,:,Iiter)       = imUtil.art.addSources(repmat(0,size(AI.Image)),CubePSF,XY,...
                                                                        'Oversample',[],'Subtract',false);
            SubtractedImage(:,:,Iiter)   = AI.Image - SourceImage(:,:,Iiter);  
            
            Cat(Iiter)                   = AI.CatData; 
            
            AI.Image                     = SubtractedImage(:,:,Iiter); % replace the subtracted image
            
            AI.CatData                   = []; % do we need to wipe out the catalog before the next iteration? 
            
        end
        
        Result(Iobj).CatData = merge(Cat);
    end
    
    % Find diffraction spikes
    
    % Cleaning
    
%     Nobj = numel(Obj);
%     for Iobj=1:1:Nobj
%         % measure background/variance if needed
%         if Args.ReBack || any(Obj(Iobj).isemptyImage({'Back','Var'}))
%             % measure background and variance
%             Obj(Iobj) = imProc.background.background(Obj(Iobj), Args.backgroundArgs{:});
% 
%         end
% 
%         if Obj(Iobj).PSFData.isemptyPSF
%             % NO PSF - attempt to measure
%         
%             % find sources using a prelimnary PSF
%             ResSt = imUtil.sources.findSources(Result(Iobj), 'Threshold',Args.PrelimThreshold,...
%                                                         'Psf',Args.PrelimPsf,...
%                                                         'PsfArgs',Args.PrelimPsfArgs,...
%                                                         'ForcedList',[],...
%                                                         'OnlyForced',false,...
%                                                         'Conn',Args.Conn,...
%                                                         'CleanSources',Args.PrelimCleanSrc,...
%                                                         'cleanSourcesArgs',Args.PrelimCleanSrcArgs);
% 
% 
%             % construct PSF            
% 
%             % what to do if no PSF?
% 
%         end
%         
%         % find sources using updated PSF
% 
%         % cleaning iteration
% 
%         % Measure properties
%       
%         % another cleaning iteration?
% 
%     end

end