function [Result, SourceLess] = multiIterExtractor(Obj, Args)
    % One line description
    %     Optional detailed description
    % Input  : - 
    %          - 
    %          * ...,key,val,... 
    % Output : - 
    % Author : Eran Ofek (2025 Nov) 
    % Example: [AI1,AI2]=imProc.sources.multiIterExtractor(AI);

    arguments
        Obj AstroImage

        % pre subtraction treatment
        Args.ExcludeEmpty              = true;

        % background
        Args.backVarArgs               = {'Block',[]};
        Args.ReCalcBackIter            = []; % list of iterations in which to re-calc the background. If 1, recalc also in the begining.

        % background and variance measurement:
        Args.ReCalcBack logical        = true; % remeasure background at every iteration   

        % measure PSF
        Args.UseOriginalPSF logical    = true;   % use the PSF already attached to the input AstroImage
        Args.populatePSFArgs cell      = {'CropByQuantile',false}; % {'CropByQuantile',true,'Quantile',0.5}
        Args.ThresholdPSF              = 30;
        Args.RangeSN                   = [50 1000];
        Args.InitPsf                   = @imUtil.kernel2.gauss
        Args.InitPsfArgs cell          = {[0.1 1.2]}; %{[0.1;1.0;1.5]};  
        
        Args.ReCalcPsfIter             = [];  % Index of iterations in which to re-calc PSF; if UseOriginalPSF=true, then no need to set this to 1.

        Args.psfFitPhotArgs            = {};
        Args.suppressEdgesArgs         = {'Fun',@imUtil.kernel2.cosbell, 'FunPars', [5, 8], 'Norm', true};

        Args.MomRadius                 = [4 6 6 6 6];  % [pix] for each iteration % recommended MomRadius = 1.7 * FWHM ~ 3.8 (for LAST!)
        
        Args.RedNoiseFactor            = 1.3; % increase the variance due to the sources found at previous iterations by this factor
                
        % PSF measurement:
        
                
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
    Args.MomRadius = Args.MomRadius.*ones(Niter,1);
    Args.FitRadius = Args.FitRadius.*ones(Niter,1);
    
    % create a new object if requested  
    if Args.CreateNewObj
        Result = Obj.copy;
    else
        Result = Obj;
    end
     
    % exclude objects with empty images
    if Args.ExcludeEmpty
        FlagEmptyImage = Result.isemptyProperty('Image');
        Result         = Result(~FlagEmptyImage);
    end
    
    % measure background and variance if it is missing or if the object is new
    ReCalcBackIter1 = any(Args.ReCalcBackIter==1); % re-calc backgroun in 1st iteration
    FlagBack = ReCalcBackIter1 | Result.isemptyProperty('Back') | Result.isemptyProperty('Var');
    if any(FlagBack)
        Result(FlagBack) = imProc.background.backVar(Result, Args.backVarArgs{:});
    end
    
    % measure PSF if it does not exist or if the user requested to re-calc
    % NB: if the input catalog is empty, the catalog struct need for PSF measurements
    % will be generated inside imUtil.psf.constructPSF by imUtil.sources.findSources 
    % at Threshold > 20 sigma, but the object's catalog property will not be populated
    FlagPSF = Result.isemptyPSF | ~Args.UseOriginalPSF; 
    if any(FlagPSF)
        [Result(FlagPSF)] = imProc.psf.populatePSF(Result(FlagPSF),...
                                                   Args.populatePSFArgs{:},...
                                                   'ThresholdPSF',Args.ThresholdPSF,...
                                                   'RangeSN',Args.RangeSN,...
                                                   'InitPsf',Args.InitPsf,...
                                                   'InitPsfArgs',Args.InitPsfArgs,...
                                                   'RePopulatePSF',true);
    end
    
    % delete the object's input catalog 
    % if the catalog is not removed, it may conflict with the new ones 
    FlagPopCat = Result.sizeCatalog>0;
    Result(FlagPopCat).deleteProp('CatData');
    
    % Define AstroImage of subtracted sources
    if nargout>1
        SourceLess = AstroImage(size(Result));   
    end

    % find and measure sources using multi-iteration PSF fitting    
    for Iobj=1:1:Nobj
        if Args.Verbose
            fprintf('Image %d of %d \n',Iobj,Nobj);
        end    

        % we need a deep copy here, otherwise, the initial image is not kept in the AI!
        AI              = Result(Iobj).copy;                                    % this AI will be iterated for each Obj 
        AI.CatData      = [];
        Cat             = AstroCatalog([1 Niter]);                              % catalogs produced at each iter, merged afterwards 
        SourceImage     = repmat(0,size(AI.Image,1),size(AI.Image,2),Niter);    % source image after each iteration
        if nargout>1
            SubtractedImage = repmat(0,size(AI.Image,1),size(AI.Image,2),Niter);    % subtracted image after each iteration
        end

        for Iiter=1:1:Niter     

            % find sources (without background recalculation) with the empirical PSF or with a set of Gaussians                     
            % in each case the sources identified as CRs are removed from the catalog
            % NB: 1. If 'Psf' is provided, this parameter overrides the PsfFun input argument
            %     2. When a PSF stamp is used for source detection, the output catalog does not contain SN_3, just SN_1 and SN_2!                
            if Args.FindWithEmpiricalPSF                   
                %??????????????????????????????????????????????
                % treat also delta+extended!
                
                PSFTemplate(:,:,1) = Args.InitPsf(Args.InitPsfArgs{1}(1),size(AI.PSF)); % a narrow delta-like PSF for CR rejection                 
                PSFTemplate(:,:,2) = AI.PSF; % the empirical PSF 
                AI = imProc.sources.findMeasureSources(AI,'Threshold', Args.Threshold(Iiter),'ReCalcBack',false,...
                    'MomPar',{'MomRadius',Args.MomRadius(Iiter)},'Psf',PSFTemplate,...
                    'FlagCR',true,'maskCR_Args',Args.maskCR_Args,'ColCell',Args.ColCell);
                ColSN = 'SN_2';            
                %clear PSFTemplate
            else
                AI = imProc.sources.findMeasureSources(AI,'Threshold', Args.Threshold(Iiter),'ReCalcBack',false,...
                    'MomPar',{'MomRadius',Args.MomRadius(Iiter)},'PsfFunPar',Args.PsfFunPar,...
                    'FlagCR',true,'maskCR_Args',Args.maskCR_Args,'ColCell',Args.ColCell);
                ColSN = 'SN_2';
            end                         
            
            NumSrc = height(AI.CatData.Catalog);
            
            if Args.Verbose
                fprintf('Iter. %d: S/N > %d, mean bkg = %.0f, mean var = %.0f, Nobj: %d\n',...
                    Iiter,Args.Threshold(Iiter),mean(AI.Back,'all','omitnan'),mean(AI.Var,'all','omitnan'),NumSrc);
            end            
            % insert a column with iteration number into the source catalog
            AI.CatData = insertCol(AI.CatData, repmat(Iiter,1,NumSrc)', Inf, 'ITER', {''});
            
            % measure the PSF (if we believe that the PSF is flux-dependent?) or use the previous one 
            ReCalcPSF = any(Args.ReCalcPsfIter==Iiter);
            if ReCalcPSF || isempty(AI.PSF)
                AI = imProc.psf.populatePSF(AI,Args.populatePSFArgs{:});                
            end
            
            [AI, Res] = imProc.sources.psfFitPhot(AI,'ColSN',ColSN,'FitRadius',Args.FitRadius(Iiter), Args.psfFitPhotArgs{:});  % produces PSFs shifted to RoundX, RoundY, so there is no need to Recenter
            
            % use either a) interpolation (experimental) or b) FFT shift (obtained above as Res.ShiftedPSF) + edge suppression
                

            if Args.UsePSFInterpolant
                ShiftedPSF = imUtil.trans.shift_interp(AI.SPFData.Data, Res.DX, Res.DY, 'Norm',true);
            else
                ShiftedPSF = imUtil.psf.suppressEdges(Res.ShiftedPSF, Args.suppressEdgesArgs{:});
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
            %if Args.RemoveMasked
            %    Ind = AI.Mask > 0;                
            %    Subtracted(Ind) = AI.Back(Ind);
            %end

            % NOT CLEAR WHAT IS THIS FOR?
            % optionaly set pixels with reconstructed source PSFs to the background values 
            %if Args.RemovePSFCore
            %    Ind = SourceImage(:,:,Iiter) > 0;
            %    Subtracted(Ind) = AI.Back(Ind); % need to be tested and improved to operate only on a 3x3 (5x5?) pixel core
            %end              
                        
            Cat(Iiter)                   = AI.CatData; 
            
            AI.Image                     = Subtracted; % replace the image with the subtracted image
            
            if nargout>1
                SubtractedImage(:,:,Iiter)   = Subtracted; % populate the array of subtracted images 
            end

            % re-measure background at each Iter > 1 if Args.ReCalcBack = true and add source noise to the variance                
            ReCalcBackIterI = any(Args.ReCalcBackIter==Iiter); % re-calc backgroun in Iiter iteration
            if ReCalcBackIterI
                FlagBack         = ReCalcBackIterI | Result.isemptyProperty('Back') | Result.isemptyProperty('Var');
                Result(FlagBack) = imProc.background.backVar(Result, Args.backVarArgs{:});
            end
            
            % add local variance from the sources revealed at all the previous iterations
            AI.Var  = AI.Var  + Args.RedNoiseFactor   .* sum(SourceImage,3);                 
             

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


            % Yes - this is needed
            % Should be replaced with a cleaner way...
            AI.CatData = []; 

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
        if nargout>1
            SourceLess(Iobj)       = Result(Iobj).copy;
            SourceLess(Iobj).Image = SubtractedImage(:,:,Niter); % or just  = Subtracted ?
        end        

        if Args.Verbose
            fprintf('Total %d objects extracted \n',height(Result(Iobj).CatData.Catalog));
        end
    end    
    % Find diffraction spikes?    
    % Cleaning?     
end


