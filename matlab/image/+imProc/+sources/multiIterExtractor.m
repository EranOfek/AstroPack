function [Result, SourceLess, SubtractedImage] = multiIterExtractor(Obj, Args)
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
        Args.BitDict                   = BitDictionary('BitMask.Image.Default');
        Args.JD                        = [];
        Args.KeyJD                     = [];

        % background and variance measurement:
        Args.backVarArgs               = {'Block',[128 128], 'Method',@imUtil.background.modeVar_LogHist, 'MethodArgs',{{'MinVal',5, 'MaxVal',3000},{}}};
        Args.ReCalcBackIter            = []; % list of iterations in which to re-calc the background. If 1, recalc also in the begining.

        % measure PSF
        Args.ReCalcPsfIter             = [];  % Index of iterations in which to re-calc PSF; if UseOriginalPSF=true, then no need to set this to 1.
        Args.UseOriginalPSF logical    = true;   % use the PSF already attached to the input AstroImage
        Args.populatePSFArgs cell      = {'CropByQuantile',false, 'SuppressWidth',2}; % {'CropByQuantile',true,'Quantile',0.5}
        Args.RadiusPSF                 = 12;
        Args.AperRadius                = [2, 4, 6];
        Args.Annulus                   = [10 12];
        Args.ThresholdPSF              = 100;
        Args.RangeSN                   = [100 1000];
        Args.InitPsf                   = @imUtil.kernel2.gauss
        Args.InitPsfArgs cell          = {[0.1; 1.2]}; %{[0.1;1.0;1.5]};  
        Args.ConvFunExtendedPSF        = @imUtil.kernel2.sersic;
        Args.ConvFunExtendedPSF_Args   = {[1 2 1]}; 
        
        % PSF fitting
        Args.MomRadius                 = [6 6 6 6 6];  % [pix] for each iteration % recommended MomRadius = 1.7 * FWHM ~ 3.8 (for LAST!)
        Args.psfFitPhotArgs            = {};
        Args.suppressEdgesArgs         = {'Fun',@imUtil.kernel2.cosbell, 'FunPars', [9, 10], 'Norm', true};
        Args.UsePSFInterpolant         = false;
        Args.FitRadius                 = [3 3 3 3 3];% PSF fit radius at each iteration

        % source cleaning and mask
        Args.RemoveEdgeDist            = 0;  % NaN for non removal
        Args.FlagCR logical            = true;
        Args.maskCR_Args cell          = {};
        Args.FlagDiffXY logical        = true;
        Args.maskDiffXY_Args cell      = {};
        
        % source detection:        
        Args.FindWithEmpiricalPSF logical = true;
        Args.PsfFunPar cell            = {[0.1;1.0;1.5]};  % search for sources                 
        Args.Threshold                 = [500 50 5]; % [50 16.5 5]; % in sigma, this also specifies the # of iterations   
        Args.ColCell cell              = {'XPEAK','YPEAK',...
                                        'X1', 'Y1',...
                                        'X2','Y2','XY',...
                                        'SN','BACK_IM','VAR_IM',...
                                        'BACK_ANNULUS', 'STD_ANNULUS', ...
                                        'FLUX_APER', 'FLUXERR_APER',...
                                        'MAG_APER', 'MAGERR_APER'};
        
        
        
        % Column names
        Args.ColRA                     = 'RA';
        Args.ColDec                    = 'Dec';
        Args.ColPITER                  = 'PITER';  % column name for PSF iteration


        % cleaning of the subtracted image:        
        Args.RemoveMasked              = false;  % the input AI.Mask should be filled, but seems like this filter does not influence the result much ? 
        Args.RemovePSFCore             = false;  % not decided if this is useful and correct

        Args.RedoUpIter = [1];

        Args.mexCutout = true;

        Args.ColFlux           = 'FLUX_APER';
        Args.ColFluxErr        = 'FLUXERR_APER';
        Args.ColMag            = 'MAG_APER';
        Args.ColMagErr         = 'MAGERR_APER'
        Args.ZP                = 25;


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

    if isempty(Args.JD)
        JD = Result.julday('KeyJD',Args.KeyJD);
    else
        JD = Args.JD;
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
                                                   'RadiusPSF',Args.RadiusPSF,...
                                                   'Annulus',Args.Annulus,...
                                                   'ThresholdPSF',Args.ThresholdPSF,...
                                                   'RangeSN',Args.RangeSN,...
                                                   'InitPsf',Args.InitPsf,...
                                                   'InitPsfArgs',Args.InitPsfArgs,...
                                                   'RePopulatePSF',true);
    end
    
    % delete the object's input catalog 
    % if the catalog is not removed, it may conflict with the new ones 
    FlagPopCat = Result.sizeCatalog>0;
    if any(FlagPopCat)
        Result(FlagPopCat).deleteProp('CatData');
    end
    
    % Define AstroImage of subtracted sources
    if nargout>1
        ExtraOutput = true;
    else
        ExtraOutput = false;
    end
    if ExtraOutput
        SourceLess = AstroImage(size(Result));   
    end

    % find and measure sources using multi-iteration PSF fitting    
    for Iobj=1:1:Nobj
        if Args.Verbose
            fprintf('Image %d of %d \n',Iobj,Nobj);
        end    
        %Result(Iobj).Table = [];
        %FWHM = Result(Iobj).PSFData.fwhm;

        % PSFTemplate = Args.InitPsf(Args.InitPsfArgs{:});
        % PSFTemplate = repmat(single(0), )

        
        % we need a deep copy here, otherwise, the initial image is not kept in the AI!
        AI              = Result(Iobj).copy;                                    % this AI will be iterated for each Obj 
        AI.CatData      = [];
        %AI.Table        = [];
        Cat             = AstroCatalog([1 Niter]);                              % catalogs produced at each iter, merged afterwards 
        
        SizeImage       = size(AI.ImageData.Image);
        SourceImage     = repmat(single(0),SizeImage(1), SizeImage(2), Niter);    % source image after each iteration
        SumSourceImage  = repmat(single(0),SizeImage(1), SizeImage(2));    % source image after each iteration
        if ExtraOutput
            SubtractedImage = repmat(single(0), SizeImage(1), SizeImage(2), Niter);    % subtracted image after each iteration
        end

        for Iiter=1:1:Niter     

            % find sources (without background recalculation) with the empirical PSF or with a set of Gaussians                     
            % in each case the sources identified as CRs are removed from the catalog
            % NB: 1. If 'Psf' is provided, this parameter overrides the PsfFun input argument
            %     2. When a PSF stamp is used for source detection, the output catalog does not contain SN_3, just SN_1 and SN_2!                
            if Args.FindWithEmpiricalPSF                   
                
                if Iiter==1
                    SizePSF = size(AI.PSFData.DataPSF);
                    if ~isempty(Args.ConvFunExtendedPSF)
                        ConvExtended = Args.ConvFunExtendedPSF(Args.ConvFunExtendedPSF_Args{:}, SizePSF);
                        PSFTemplate = repmat(single(0), [SizePSF, 3]);
                        PSFTemplate(:,:,3) = conv2(AI.PSFData.DataPSF, ConvExtended, 'same');
                    else
                        PSFTemplate = repmat(single(0), [SizePSF, 2]);
                    end
                    
                    PSFTemplate(:,:,1) = Args.InitPsf(Args.InitPsfArgs{1}(1),size(AI.PSF)); % a narrow delta-like PSF for CR rejection                 
                    PSFTemplate(:,:,2) = AI.PSFData.DataPSF; % the empirical PSF
                    
                    % check the information content overlap between the PSF
                    % and extended PSF:
                    % tools.math.filter.infoOverlapFilters(squeeze(PSFTemplate(:,:,3)),squeeze(PSFTemplate(:,:,2)))

                    %PSFTemplate = AI.PSF; % the empirical PSF
                    
                end

                AI = imProc.sources.findMeasureSources(AI,'Threshold', Args.Threshold(Iiter),'ReCalcBack',false,...
                                                          'RemoveEdgeDist',Args.RemoveEdgeDist,...
                                                          'MomPar',{'MomRadius',Args.MomRadius(Iiter), 'AperRadius',Args.AperRadius, 'Annulus',Args.Annulus},...
                                                          'Psf',PSFTemplate,...
                                                          'FlagCR',Args.FlagCR,'maskCR_Args',Args.maskCR_Args,...
                                                          'FlagDiffXY',Args.FlagDiffXY, 'maskDiffXY_Args',Args.maskDiffXY_Args,...
                                                          'ColCell',Args.ColCell,...
                                                          'BitDict',Args.BitDict,...
                                                          'JD',JD);
               
                ColSN = 'SN_2';            
                %clear PSFTemplate
            else
                AI = imProc.sources.findMeasureSources(AI,'Threshold', Args.Threshold(Iiter),'ReCalcBack',false,...
                                                          'RemoveEdgeDist',Args.RemoveEdgeDist,...
                                                          'MomPar',{'MomRadius',Args.MomRadius(Iiter), 'AperRadius',Args.AperRadius, 'Annulus',Args.Annulus},...
                                                          'PsfFunPar',Args.PsfFunPar,...
                                                          'FlagCR',Args.FlagCR,'maskCR_Args',Args.maskCR_Args,...
                                                          'FlagDiffXY',Args.FlagDiffXY, 'maskDiffXY_Args',Args.maskDiffXY_Args,...
                                                          'ColCell',Args.ColCell,...
                                                          'BitDict',Args.BitDict,...
                                                          'JD',JD);
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
            
            % PSF photometry
            [AI, Res] = imProc.sources.psfFitPhot(AI,'ColSN',ColSN,'FitRadius',Args.FitRadius(Iiter), Args.psfFitPhotArgs{:});  % produces PSFs shifted to RoundX, RoundY, so there is no need to Recenter

            
            % use either a) interpolation (experimental) or b) FFT shift (obtained above as Res.ShiftedPSF) + edge suppression
            if Args.UsePSFInterpolant
                ShiftedPSF = imUtil.trans.shift_interp(AI.SPFData.Data, Res.DX, Res.DY, 'Norm',true);
            else
                ShiftedPSF = Res.ShiftedPSF;
                % already done in PSF construction
                %ShiftedPSF = imUtil.psf.suppressEdges(Res.ShiftedPSF, Args.suppressEdgesArgs{:}); 
            end            
    
            % subtract the newly found and measured sources:
            % 1. construct a source image
            % 2. subtract the source image from the current image
            [CubePSF, XY]                = imUtil.art.createSourceCube(ShiftedPSF, [Res.RoundY Res.RoundX], Res.Flux, ...
                                                                        'Recenter', false,'PositivePSF',false, 'FunEdge',[]);
           
            SourceImage(:,:,Iiter)       = imUtil.art.addSources(repmat(single(0), SizeImage), permute(CubePSF,[2,1,3]),XY,...
                                                                        'Oversample',[],'Subtract',false);  
            SumSourceImage = SumSourceImage + SourceImage(:,:,Iiter);
            Subtracted                   = AI.ImageData.Image - SourceImage(:,:,Iiter);  
            
            
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
            
            % add PITER to catalog
            Nsrc = size(Cat(Iiter).Catalog, 1);
            Cat(Iiter).insertCol(Iiter.*ones(Nsrc,1),Inf, Args.ColPITER,'');
            
            AI.Image                     = Subtracted; % replace the image with the subtracted image
            
            if ExtraOutput
                SubtractedImage(:,:,Iiter)   = Subtracted; % populate the array of subtracted images 
            end

            % re-measure background at each Iter > 1 if Args.ReCalcBack = true and add source noise to the variance                
            ReCalcBackIterI = any(Args.ReCalcBackIter==Iiter); % re-calc backgroun in Iiter iteration
            if ReCalcBackIterI
                FlagBack         = ReCalcBackIterI | Result.isemptyProperty('Back') | Result.isemptyProperty('Var');
                Result(FlagBack) = imProc.background.backVar(Result, Args.backVarArgs{:});
            end
            
            % add local variance from the sources revealed at all the previous iterations
            % This is not enough - for bright stars the PSF is more
            % extended and the star edges are not subtracted
            Nimages = 20;
            AI.VarData.Image  = AI.VarData.Image  + SumSourceImage./Nimages;
            AI.BackData.Image = AI.BackData.Image + SumSourceImage;  


            if Iiter==1
                % Add noise/back around bright sources
                %GK = imUtil.kernel2.gauss(FWHM);
                %AI.Var  = conv2(AI.Var, GK, 'same'); 
                LK = imUtil.kernel2.lorentzian(4,[101 101]);
                CK = imUtil.kernel2.circ(7,[15 15]);
                CK = CK./max(CK,[],'all');
                EdgesVarMap = repmat(single(0), SizeImage);
                ColData = AI.CatData.getCol({'XPEAK','YPEAK','FLUX_APER_3'});
                LinIndex = imUtil.image.sub2ind_fast(SizeImage, ColData(:,2), ColData(:,1));
                %LinIndex = sub2ind(SizeImage, AI.CatData.Table.YPEAK, AI.CatData.Table.XPEAK);
                %EdgesVarMap(LinIndex) = AI.CatData.Table.FLUX_APER_3;
                ScatteredLightFrac = 0.03;
                EdgesVarMap(LinIndex) = ColData(:,3).*ScatteredLightFrac.*max(1, log10(ColData(:,3)./1e5));
                %AI.Back(AI.Image>5000) = 5000;
                ConvBright = conv2(EdgesVarMap, LK, 'same');
                ConvCore   = conv2(EdgesVarMap, CK, 'same')./ScatteredLightFrac;
                AI.VarData.Image  = AI.VarData.Image  + ConvBright./Nimages + ConvCore;
                AI.BackData.Image = AI.BackData.Image + ConvBright + ConvCore;
            end
            

            % write region files with extracted objects 
            if Args.WriteDs9Regions
                writeDS9region(AI, Args);
            end 


            % Yes - this is needed
            % Should be replaced with a cleaner way...
            AI.CatData = []; 

        end % end of iterations  

        
        if ~isempty(Args.RedoUpIter)
            if Niter<=Args.RedoUpIter
                error('Niter is <= RedoUpIter - does not make sense');
            end
            % yet another iteration for the bright sources only
            SubImageFaint = Result(Iobj).ImageData.Image - sum(SourceImage(:,:,(Args.RedoUpIter+1):end),3) - Result(Iobj).BackData.Image;
            
            % merge the Catalogs
            CatBright = Cat(1:Args.RedoUpIter).merge;
            CatFaint  = Cat((Args.RedoUpIter+1):end).merge;
            % do aper phot only on bright stars
            BrightXY = CatBright.getXY;

            % See alos:
            % R=imProc.sources.aperPhot(AI);

            % perform only aperture photometry on brightest sources
            [Cube] = imUtil.cut.image2cutouts(SubImageFaint, BrightXY(:,1), BrightXY(:,2), Args.RadiusPSF, 'mexCutout',Args.mexCutout, 'Circle',false);
            ResAperBright = imUtil.sources.aperPhotCube(Cube, 'AperRad',Args.AperRadius, 'AnnulusRad',Args.Annulus);

            % insert aper phot data to catalog of Cat(1:Args.RedoUpIter)
             
            Naper      = numel(Args.AperRadius);
            ColFlux    = tools.cell.cellNumericSuffix(Args.ColFlux, (1:Naper));
            ColFluxErr = tools.cell.cellNumericSuffix(Args.ColFluxErr, (1:Naper));
            ColMag     = tools.cell.cellNumericSuffix(Args.ColMag, (1:Naper));
            ColMagErr  = tools.cell.cellNumericSuffix(Args.ColMagErr, (1:Naper));
            ColsToAdd  = [ColFlux, ColFluxErr, ColMag, ColMagErr];
            %[C1{1:Naper.*2}] = deal('');
            %[C2{1:Naper.*2}] = deal('mag');
            %ColUnits         = [C1, C2];

            
            FluxMagData = [ResAperBright.AperPhot,...
                           ResAperBright.AperPhotErr,...
                           convert.luptitude(ResAperBright.AperPhot, 10.^(0.4.*Args.ZP)),...
                           1.086.*ResAperBright.AperPhotErr./ResAperBright.AperPhot];
             
            CatBright.replaceCol(FluxMagData, ColsToAdd);
            
            Result(Iobj).CatData.Catalog = [CatBright.Catalog; CatFaint.Catalog];
            Result(Iobj).CatData.ColNames = CatBright.ColNames;
        else
            % merge the catalogs of objects extracted at all the iterations
            Result(Iobj).CatData = merge(Cat);
        end




        

        % Cleaning:
        % remove sources on edge
        % mask CR
        % remove CR

        % remove bad sources
            % % works only for Gaussian PSF
            % if Args.FlagCR && ~isemptyImage(Obj(Iobj), 'Mask')
            %     Result(Iobj) = imProc.mask.maskCR(Result(Iobj), Args.maskCR_Args{:});
            % end
            % if Args.FlagDiffXY
            %     Result(Iobj) = imProc.mask.xpeak_x1_diff(Result(Iobj), Args.maskDiffXY_Args{:});
            % end
            % 
            % if Args.RemoveBadSources
            %     [Result(Iobj)] = imProc.sources.cleanSources(Result(Iobj), 'SigmaPSF',Args.PsfFunPar{1}(1:2),...
            %                                                                'ColNamsSN',{'SN_1','SN_2'},...
            %                                                                'RemoveBadSources',Args.RemoveBadSources,...
            %                                                                'CreateNewObj',false);
            % end
               
        % add RA, Dec from the object's WCS if it is present
        if Args.AddSkyCoo && ~isempty(Result(Iobj).WCS)
            XY        = Result(Iobj).CatData.getXY();
            [RA, Dec] = Result(Iobj).WCS.xy2sky(XY(:,1), XY(:,2));
            Result(Iobj).CatData = insertCol(Result(Iobj).CatData, RA, Inf, Args.ColRA, {''});
            Result(Iobj).CatData = insertCol(Result(Iobj).CatData, Dec, Inf, Args.ColDec, {''});
            Result(Iobj).CatData.sortrows(Args.ColDec);    
        end        
        
        % save a copy of the AI object with the image replaced by the final subtracted image
        if ExtraOutput
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

%------------------------------------------------

function writeDS9region(AI, Args)
    % help function for writing ds9 region file

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
