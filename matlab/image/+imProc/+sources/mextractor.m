function [Result, SourceLess] = mextractor(Obj, Args)
    % Multi-iteration PSF fitting and source extractor 
    % Input:  - a stack of AstroImage objects with Proc or Coadd images and (optionally) filled masks
    %         * ...,key,val,...
    %         'ReCalcBack' - (logical) recalculate background at each iteration (def. true)
    %         'BackPar'    - parameters of background estimation
    %         'VarMethod'  - variance estimation method
    %         'MomRadius'  - radius to calculate image momentum (one for all or separate for each iteration)
    %         'RedNoiseFactor' - factor of variance increase around found sources 
    %
    %         'populatePSFArgs' - parameters of PSF estimation (cell array)
    %         'ThresholdPSF'    - threshold of object selection for PSF estimation
    %         'RangeSN'         - range of object SNRs selected for PSF estimation
    %         'InitPsf'         - initial PSF form employed to find objects for further empiric PSF measurements
    %         'InitPsfArgs'     - parameters of initial PSF form used to find objects for PSF measurements
    %         'UsePSFInterpolant' - (logical) interpolate the measured PSF (errors in flux estimation) or use FFT shifts (artifacts) 
    %
    %         'FindWithEmpiricalPSF' - (logical) find sources with empirical PSF or a set of gaussians (def. false)
    %         'PsfFunPar'      - PSF widths to be employed for source search
    %         'Threshold'      - a vector of threshold significance employed for source search: one component per iteration
    %                            NB: this parameter also sets the number of iterations!
    %         'maskCR_args'    - arguments for the imProc.mask.maskCR function employed to exclude CRs from the catalog  
    %         'FitRadius'      - [pix] PSF fit radius (one for all or separate for each iteration)
    %         'UseOriginalPSF' - (logical) use the PSF already attached to the input AstroImage
    %         'ReCalcPSF'      - (logical) remeasure PSF at each iteration (def. false)
    %
    %         'RemoveMasked'   - (logical) put the pixels masked in the original AI to the bkg level (def. false)
    %         'RemovePSFCore'  - (logical) put the pixels with some radius from found sources to the bkg level (def. false)
    %         'DeleteInputCatalog' - (logical) delete the catalog property from the input AI stack (def. true)
    %         'AddSkyCoo'      - (logical) add RA, Dec from the AstroImage WCS if it is present (def. true)
    %         'CreateNewObj'   - (logical) create a deep copy of the input AI (def. false)
    %         'Verbose'        - (logical) be verbous (def. false)
    %         'WriteDs9Regions'- (logical) at each iteration save the extracted source positions as ds9 region files (def. false)  
    % Output: - the input AI's with catalogs filled by the data on discovered and measured sources   
    %         - (optional) same as above, but with AI.Image replaced by the sourceless image (result of consecutive subtractions)
    % Example: AI = imProc.sources.mextractor(AI, 'Threshold', [30 10 5]);
    % 
    arguments
        Obj AstroImage

        % background and variance measurement:
        Args.ReCalcBack logical        = true; % remeasure background at every iteration   
        Args.BackPar                   = {'SubSizeXY',[128 128]}; % {'SubSizeXY',[]})

        Args.VarMethod                 = 'LogHist';             
        Args.MomRadius                 = [4 6 6 6 6];  % [pix] for each iteration % recommended MomRadius = 1.7 * FWHM ~ 3.8 (for LAST!)
        
        Args.RedNoiseFactor            = 1.3; % increase the variance due to the sources found at previous iterations by this factor
                
        % PSF measurement:
        Args.populatePSFArgs cell      = {'CropByQuantile',false}; % {'CropByQuantile',true,'Quantile',0.5}
        Args.ThresholdPSF              = 20;
        Args.RangeSN                   = [50 1000];
        Args.InitPsf                   = @imUtil.kernel2.gauss
        Args.InitPsfArgs cell          = {[0.1;1.0;1.5]};  
                
        Args.UsePSFInterpolant         = false;
        
        % source detection:        
        Args.FindWithEmpiricalPSF logical = false;
        Args.PsfFunPar cell            = {[0.1;1.0;1.5]};  % search for sources                 
        Args.Threshold                 = [30 10 5]; % [50 16.5 5]; % in sigma, this also specifies the # of iterations   
        Args.maskCR_Args cell          = {};
        Args.ColCell cell              = {'XPEAK','YPEAK',...
                                        'X1', 'Y1',...
                                        'X2','Y2','XY',...
                                        'SN','BACK_IM','VAR_IM',...
                                        'BACK_ANNULUS', 'STD_ANNULUS', ...
                                        'FLUX_APER', 'FLUXERR_APER',...
                                        'MAG_APER', 'MAGERR_APER'};
        % source PSF fitting:
        Args.FitRadius                 = [3 3 3 3 3];% PSF fit radius at each iteration
        Args.UseOriginalPSF logical    = true;   % use the PSF already attached to the input AstroImage
        Args.ReCalcPSF logical         = false;  % do not remeasure PSF at every iteration      
        
        % cleaning of the subtracted image:        
        Args.RemoveMasked              = false;  % the input AI.Mask should be filled, but seems like this filter does not influence the result much ? 
        Args.RemovePSFCore             = false;  % not decided if this is useful and correct
                              
        % miscellaneous:
        Args.DeleteInputCatalog        = true;  % delete the catalog property from the input AI stack 
        Args.AddSkyCoo                 = true;  % add RA, Dec from the AstroImage WCS if it is present 
        Args.CreateNewObj logical      = false;   
        Args.SaveSourcelessImage logical= false; % save the cleaned sourceless image as the second result
        Args.Verbose logical           = false;  
        Args.WriteDs9Regions logical   = false;
    end
    
    % check consistency
    if numel(Args.Threshold) > numel(Args.MomRadius) || numel(Args.Threshold) > numel(Args.FitRadius)
        error('The length of Args.Threshold does must comply with that of Args.MomRadius');
    end
    Niter = numel(Args.Threshold);
    Nobj  = numel(Obj); 
    % repair some parameters if needed: 
    if numel(Args.MomRadius) < Niter
        Args.MomRadius(1:Niter) = Args.MomRadius(1);
    end
    if numel(Args.FitRadius) < Niter
        Args.FitRadius(1:Niter) = Args.FitRadius(1);
    end
    
    % create a new object if requested  
    if Args.CreateNewObj
        Result = Obj.copy;
    else
        Result = Obj;
    end
     
    % exclude objects with empty images
    Result(Result.isemptyProperty('Image')) = [];    
    
    % measure background and variance if it is missing or if the object is new
    FlagBack = Result.isemptyProperty('Back') | Result.isemptyProperty('Var') | Args.CreateNewObj;
    if any(FlagBack)
        Result(FlagBack) = imProc.background.background(Result(FlagBack), Args.BackPar{:});
    end
    
    % measure PSF if it does not exist or if the user requested to re-calc
    % NB: if the input catalog is empty, the catalog struct need for PSF measurements
    % will be generated inside imUtil.psf.constructPSF by imUtil.sources.findSources 
    % at Threshold > 20 sigma, but the object's catalog property will not be populated
    FlagPSF = Result.isemptyPSF | ~Args.UseOriginalPSF; 
    if any(FlagPSF)
        [Result(FlagPSF)] = imProc.psf.populatePSF(Result(FlagPSF), Args.populatePSFArgs{:},...
            'ThresholdPSF',Args.ThresholdPSF,...
            'RangeSN',Args.RangeSN,...
            'InitPsf',Args.InitPsf,...
            'InitPsfArgs',Args.InitPsfArgs,...
            'RePopulatePSF',true);
    end
    
    % delete the object's input catalog 
    % if the catalog is not removed, it may conflict with the new ones 
    if Args.DeleteInputCatalog
        Result.deleteProp('CatData');
        Result.deleteProp('Table');  % NOT NEEDED!
    end    
                                                      
    % find and measure sources using multi-iteration PSF fitting    
    SourceLess = AstroImage([1 Nobj]);   
    
    for Iobj=1:1:Nobj
                            if Args.Verbose
                                fprintf('Image %d of %d \n',Iobj,Nobj);
                            end    
        % we need a deep copy here, otherwise, the initial image is not kept in the AI!
        AI              = Result(Iobj).copy;                                    % this AI will be iterated for each Obj 
        Cat             = AstroCatalog([1 Niter]);                              % catalogs produced at each iter, merged afterwards 
        SourceImage     = repmat(0,size(AI.Image,1),size(AI.Image,2),Niter);    % source image after each iteration
        SubtractedImage = repmat(0,size(AI.Image,1),size(AI.Image,2),Niter);    % subtracted image after each iteration
               
        for Iiter=1:1:Niter            
            % re-measure background at each Iter > 1 if Args.ReCalcBack = true and add source noise to the variance                
            if Iiter>1     
                imProc.background.background(AI, 'ReCalcBack', Args.ReCalcBack, Args.BackPar{:});
                % add local variance from the sources revealed at all the previous iterations
                AI.Var  = AI.Var  + Args.RedNoiseFactor   .* sum(SourceImage,3);                 
            end
            
            % find sources (without background recalculation) with the empirical PSF or with a set of Gaussians                     
            % in each case the sources identified as CRs are removed from the catalog
            % NB: 1. If 'Psf' is provided, this parameter overrides the PsfFun input argument
            %     2. When a PSF stamp is used for source detection, the output catalog does not contain SN_3, just SN_1 and SN_2!                
            if Args.FindWithEmpiricalPSF                   
                PSFTemplate(:,:,1) = Args.InitPsf(Args.InitPsfArgs{1}(1),size(AI.PSF)); % a narrow delta-like PSF for CR rejection                 
                PSFTemplate(:,:,2) = AI.PSF; % the empirical PSF 
                AI = imProc.sources.findMeasureSources(AI,'Threshold', Args.Threshold(Iiter),'ReCalcBack',false,...
                    'MomPar',{'MomRadius',Args.MomRadius(Iiter)},'Psf',PSFTemplate,...
                    'FlagCR',true,'maskCR_Args',Args.maskCR_Args,'ColCell',Args.ColCell);
                ColSN = 'SN_2';            
                clear PSFTemplate
            else
                AI = imProc.sources.findMeasureSources(AI,'Threshold', Args.Threshold(Iiter),'ReCalcBack',false,...
                    'MomPar',{'MomRadius',Args.MomRadius(Iiter)},'PsfFunPar',Args.PsfFunPar,...
                    'FlagCR',true,'maskCR_Args',Args.maskCR_Args,'ColCell',Args.ColCell);
                ColSN = 'SN_3';
            end                         
            
            NumSrc = height(AI.CatData.Catalog);
            
                            if Args.Verbose
                                fprintf('Iter. %d: S/N > %d, mean bkg = %.0f, mean var = %.0f, Nobj: %d\n',...
                                    Iiter,Args.Threshold(Iiter),mean(AI.Back,'all','omitnan'),mean(AI.Var,'all','omitnan'),NumSrc);
                            end            
            % insert a column with iteration number into the source catalog
            AI.CatData = insertCol(AI.CatData, repmat(Iiter,1,NumSrc)', Inf, 'ITER', {''});
            
            % measure the PSF (if we believe that the PSF is flux-dependent?) or use the previous one 
            if isempty(AI.PSF) || Args.ReCalcPSF
                AI = imProc.psf.populatePSF(AI,Args.populatePSFArgs{:});                
            end
            
            % fit the PSF to objects at the sub-pixel level and make PSF photometry
            if Iiter == 1 
%                 [M1,M2,Aper]=imUtil.image.moment2(AI.PSF,(size(AI.PSF,1)+1)/2,(size(AI.PSF,2)+1)/2);
%                 OPTIONS = optimset('MaxFunEvals',10000);
%                 [~,PSF_fit] = psf.fitPSFKernel(AI.PSF,'model','dgauss','FitRadius',3,'InerRadius',1,'ConvThresh',1e-4,'MinOpts',OPTIONS); % experimental: replace the empirical PSF with a model
% %                 [~,PSF_fit] = psf.fitPSFKernel(AI.PSF,'model','mtd','FitRadius',3,'InerRadius',0.5,'ConvThresh',1e-3,'MinOpts',OPTIONS); % experimental: replace the empirical PSF with a model
%                 AI.PSF = PSF_fit;
            end
            [AI, Res] = imProc.sources.psfFitPhot(AI,'ColSN',ColSN,'FitRadius',Args.FitRadius(Iiter));  % produces PSFs shifted to RoundX, RoundY, so there is no need to Recenter
            
            % use either a) interpolation (experimental) or b) FFT shift (obtained above as Res.ShiftedPSF) + edge suppression
            if Args.UsePSFInterpolant
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
    
            % subtract the newly found and measured sources:
            % 1. construct a source image
            % 2. subtract the source image from the current image
            [CubePSF, XY]                = imUtil.art.createSourceCube(ShiftedPSF, [Res.RoundY Res.RoundX], Res.Flux, ...
                                                                        'Recenter', false,'PositivePSF',true);
            SourceImage(:,:,Iiter)       = imUtil.art.addSources(repmat(0,size(AI.Image)),permute(CubePSF,[2,1,3]),XY,...
                                                                        'Oversample',[],'Subtract',false);                                                                                          
            Subtracted                   = AI.Image - SourceImage(:,:,Iiter);  
            
            % optionaly set pixels with Mask > 0 to the background values (in practice this does not influence the result?)
            if Args.RemoveMasked
                Ind = AI.Mask > 0;                
                Subtracted(Ind) = AI.Back(Ind);
            end
            % optionaly set pixels with reconstructed source PSFs to the background values 
            if Args.RemovePSFCore
                Ind = SourceImage(:,:,Iiter) > 0;
                Subtracted(Ind) = AI.Back(Ind); % need to be tested and improved to operate only on a 3x3 (5x5?) pixel core
            end              
                        
            Cat(Iiter)                   = AI.CatData; 
            
            AI.Image                     = Subtracted; % replace the image with the subtracted image
            
            SubtractedImage(:,:,Iiter)   = Subtracted; % populate the array of subtracted images 
            
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
            AI.CatData = []; % do we really need to wipe out the catalog before the next iteration?             
        end % end of iterations  
        
        % merge the catalogs of objects extracted at all the iterations
        Result(Iobj).CatData = merge(Cat);
               
        % add RA, Dec from the object's WCS if it is present
        if Args.AddSkyCoo && ~isempty(Result(Iobj).WCS)
            try
                [RA, Dec] = Result(Iobj).WCS.xy2sky(Result(Iobj).Table.X,Result(Iobj).Table.Y);
                Result(Iobj).CatData = insertCol(Result(Iobj).CatData, RA, Inf, 'RA', {''});
                Result(Iobj).CatData = insertCol(Result(Iobj).CatData, Dec, Inf, 'Dec', {''});
                Result(Iobj).CatData.sortrows('Dec');
            catch
                if Args.Verbose
                    fprintf('Image WCS is not clean. RA, Dec columns not added to the output catalog.\n');
                end
            end
        end        
        
        % save a copy of the AI object with the image replaced by the final subtracted image
        if Args.SaveSourcelessImage 
            SourceLess(Iobj)       = Result(Iobj).copy;
            SourceLess(Iobj).Image = SubtractedImage(:,:,Niter); % or just  = Subtracted ?
        else
            SourceLess = [];
        end        
                            if Args.Verbose
                                fprintf('Total %d objects extracted \n',height(Result(Iobj).CatData.Catalog));
                            end
    end    
    % Find diffraction spikes?    
    % Cleaning?     
end