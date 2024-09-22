function [Result, SourceLess] = mextractor(Obj, Args)
    % Multi-iteration PSF fitting and source extractor 
    % Input:  - a stack of AstroImage objects with Proc or Coadd images and (optionally) filled masks
    %         * ...,key,val,...
    %         'BackPar'   - parameters of background estimation
    %         'VarMethod' - variance estimation method
    %         'RedNoiseFactor' - variance increase around found sources (for the next iterations)
    %         'UseInterpolant' - (logical) interpolate the measured PSF (errors in flux estimation) or use FFT shifts (artifacts) 
    % Output: - the input AI's with catalogs filled by the data on revealed and measured sources   
    %         - (optional) same as above, but with AI.Image replaced by the sourceless image (result of consecutive subtractions)
    % Example: AI = imProc.sources.mextractor(AI, 'Threshold', [30 10 5]);
    % 
    arguments
        Obj AstroImage

        % background and variance measurement:
        Args.ReCalcBack                = true;
        Args.BackPar                   = {'SubSizeXY',[128 128]}; % {'SubSizeXY',[]})

        Args.VarMethod                 = 'LogHist';             
        Args.MomRadius                 = [4 6 6];  % [pix] for each iteration % recommended MomRadius = 1.7 * FWHM ~ 3.8 (for LAST!)
        
        Args.RedNoiseFactor            = 1.3; % increase the variance due to the sources found at previous iterations by this factor
        Args.BackgroundFactor          = 1.0; % multiplication factor to the additive background due to the objects recovered at previous iterations
%         Args.ReMeasBack logical      = true;       
%         Args.ReBack logical          = false; % remeasure if background exits 
                
        % PSF measurement:
        Args.populatePSFArgs cell      = {'CropByQuantile',false};
        Args.ThresholdPSF              = 20;
        Args.RangeSN                   = [50 1000];
        Args.InitPsf                   = @imUtil.kernel2.gauss
        Args.InitPsfArgs cell          = {[0.1;1.0;1.5]};  
                
        Args.UseInterpolant            = false;
        
        % source detection:        
        Args.FindWithEmpiricalPSF logical = false;
        Args.PsfFunPar cell            = {[0.1;1.0;1.5]};  % search for sources                 
        Args.Threshold                 = [30 10 5]; % [50 16.5 5]; % in sigma, this also specifies the # of iterations        
        
        % source PSF fitting:
        Args.ReCalcPSF logical         = false;       
        
%         Args.ThresholdDiffSN         = 0;
        
%         Args.PrelimPsf               = @imUtil.kernel2.gauss;
%         Args.PrelimPsfArgs cell      = {[0.1 2]};
%         Args.PrelimThreshold         = 30;
%         Args.Conn                    = 8;
%         Args.PrelimCleanSrc logical  = true;
%         Args.PrelimCleanSrcArgs cell = {'ColSN_sharp',1, 'ColSN_psf',2, 'SNdiff',0, 'MinEdgeDist',15, 'RemoveBadSources',true};

        % cleaning of the subtracted image:        
        Args.RemoveMasked              = true;   % the iput AI.Mask should be filled, but seems like this filter does not influence the result much ? 
        Args.RemovePSFCore             = false;  % not decided if this is useful and correct
                              
        % miscellaneous:
        Args.CreateNewObj logical      = false;                           
        Args.Verbose logical           = false;  
        Args.WriteDs9Regions logical   = false;
    end
    
    % create a new copy
    if Args.CreateNewObj
        Result = Obj.copy;
    else
        Result = Obj;
    end
    
    % measure background and variance
    FlagBack = Obj.isemptyProperty('Back') | Obj.isemptyProperty('Var');
    if any(FlagBack)
        Obj(FlagBack) = imProc.background.background(Obj(FlagBack), Args.BackPar{:});
    end
    
    % measure PSF
    [Result] = imProc.psf.populatePSF(Result, Args.populatePSFArgs{:},...
                                                      'ThresholdPSF',Args.ThresholdPSF,...
                                                      'RangeSN',Args.RangeSN,...
                                                      'InitPsf',Args.InitPsf,...
                                                      'InitPsfArgs',Args.InitPsfArgs);
                                                              
    % find and measure sources using multi-iteration PSF fitting
    Niter = numel(Args.Threshold);
    Nobj  = numel(Obj);   
    SourceLess = repmat(AstroImage,1,Nobj);
    
    for Iobj=1:1:Nobj
        
        AI              = Result(Iobj).copy;                                    % this AI will be iterated for each Obj
        Cat             = repmat(AstroCatalog,1,Niter);                         % catalogs produced at each iter, merged afterwards
        SourceImage     = repmat(0,size(AI.Image,1),size(AI.Image,2),Niter);    % source image after each iteration
        SubtractedImage = repmat(0,size(AI.Image,1),size(AI.Image,2),Niter);    % subtracted image after each iteration
               
        for Iiter=1:1:Niter
            
            % re-measure background at each iteration > 1 and add source noise to the variance           
            if Iiter>1        
                imProc.background.background(AI, 'ReCalcBack', Args.ReCalcBack, Args.BackPar{:});
                % add local variance from the sources revealed at all the previous iteration(s)
                AI.Var  = AI.Var  + Args.RedNoiseFactor   .* sum(SourceImage,3);
                % add local background from the sources revealed at all the previous iteration(s):
                AI.Back = AI.Back + Args.BackgroundFactor .* sum(SourceImage,3);
            end

%             if Iiter>1 && Args.ReMeasBack                
%                 % Options:
%                 % use imProc.background.background
%                 % update variance                
%             end
            
            % find sources (without background recalculation) with the empirical PSF or with a set of Gaussians
            if Args.FindWithEmpiricalPSF                
                AI = imProc.sources.findMeasureSources(AI,'Threshold', Args.Threshold(Iiter),'ReCalcBack',false,...
                    'MomPar',{'MomRadius',Args.MomRadius(Iiter)},'Psf',AI.PSF,'FlagCR',false);
                ColSN = 'SN_1';
                % NB: 1. If 'Psf' is provided, this parameter overrides the PsfFun input argument
                %     2. When a PSF stamp is used for source detection, the catalog does not contain SN_3, just SN_1 !                
            else
                AI = imProc.sources.findMeasureSources(AI,'Threshold', Args.Threshold(Iiter),'ReCalcBack',false,...
                    'MomPar',{'MomRadius',Args.MomRadius(Iiter)},'PsfFunPar',Args.PsfFunPar);
                ColSN = 'SN_3';
            end
               
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
            
                            if Args.Verbose
                                fprintf('Iter. %d: mean bkg = %.0f, mean var = %.0f, Nobj: %d\n',...
                                    Iiter,mean(AI.Back,'all'),mean(AI.Var,'all'),NumSrc);
                            end
            
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
            [AI, Res] = imProc.sources.psfFitPhot(AI,'ColSN',ColSN);  % produces PSFs shifted to RoundX, RoundY, so there is no need to Recenter
            
            % use either a) interpolation or b) FFT shift + edge suppression
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
                ShiftedPSF = imUtil.psf.suppressEdges(Res.ShiftedPSF, 'Fun',@imUtil.kernel2.cosbell, 'FunPars', [5, 8], 'Norm', true);
            end            
    
            % subtract sources:
            % 1. construct a source image
            % 2. subtract the source image from the current image
            [CubePSF, XY]                = imUtil.art.createSourceCube(ShiftedPSF, [Res.RoundY Res.RoundX], Res.Flux, ...
                                                                        'Recenter', false,'PositivePSF',true);
            SourceImage(:,:,Iiter)       = imUtil.art.addSources(repmat(0,size(AI.Image)),CubePSF,XY,...
                                                                        'Oversample',[],'Subtract',false);                                                                                          
            Subtracted                   = AI.Image - SourceImage(:,:,Iiter);  
            
            % set pixels with Mask > 0 to the background values
            if Args.RemoveMasked
                Ind = AI.Mask > 0;                
                Subtracted(Ind) = AI.Back(Ind);
            end
            % exclude pixels with reconstructed source PSFs
            if Args.RemovePSFCore
                Ind = SourceImage(:,:,Iiter) > 0;
                Subtracted(Ind) = AI.Back(Ind); % need to be tested and improved to operate only on a 3x3 (5x5?) pixel core
            end              
                        
            Cat(Iiter)                   = AI.CatData; 
            
            AI.Image                     = Subtracted; % replace the image with the subtracted image
            
            SubtractedImage(:,:,Iiter)   = Subtracted;
            
            % write region files with extracted objects 
            if Args.WriteDs9Regions
                RegName = sprintf('~/%s_it%d.reg',AI.getStructKey('OBJECT').OBJECT,Iiter);
                if     Iiter == 1
                    Clr = 'blue';
                elseif Iiter == 2
                    Clr = 'red';
                elseif Iiter == 3
                    Clr = 'green';
                end
                DS9_new.regionWrite([AI.CatData.getCol('X') AI.CatData.getCol('Y')],...
                    'FileName',RegName,'Color',Clr,'Marker','o','Size',1,'Width',4,'Precision','%.2f','PrintIndividualProp',0);
            end
            
            AI.CatData = []; % do we need to wipe out the catalog before the next iteration?             
        end 
        
        % merge the catalogs of objects extracted at all the iterations
        Result(Iobj).CatData = merge(Cat);
        % save a copy of the AI object with the image replaced by the final subtracted image
        SourceLess(Iobj)       = Result(Iobj).copy;
        SourceLess(Iobj).Image = SubtractedImage(:,:,Niter); % or just  = Subtracted ? 
        
                            if Args.Verbose
                                fprintf('Total %d objects extracted \n',height(Result(Iobj).CatData.Catalog));
                            end
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