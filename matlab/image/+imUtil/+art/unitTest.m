function Result = unitTest()
    % imUtil.art.unitTest
    % Example: imUtil.art.unitTest
    io.msgLog(LogLevel.Test, 'imUtil.art.unitTest started');
    
    %%% create an artificial sky image from object coordinates and PSFs
    
    % set the number of sources, the image size and the flux range 
    Nsrc = 1000; Nx = 2000; Ny = Nx; MaxSrcFlux = 1000;
    % define the source fluxes and their PSFs
    Flux = MaxSrcFlux .* rand(Nsrc,1);
    X1Y1 = Nx.* rand(Nsrc,2);
    Oversample = 3;
    for i = 1:Nsrc; PSF(:,:,i) = imUtil.kernel2.gauss([4 4 0],[24 24]) + 1e-2*rand(24,24); end
    
    % create a list of shifted and resampled fluxed PSF stamps
    [CubePSF, XY] = imUtil.art.createSourceCube(PSF, X1Y1, Flux, 'Recenter', true, ...
        'RecenterMethod','fft','Oversample', Oversample, 'PositivePSF', true);
    
    % create an empty image with some small noise (relative to MaxSrcFlux) 
    Image0 = rand(Nx,Ny);
    
    % fill the image with sources
    ImageSrc = imUtil.art.addSources(Image0,CubePSF,XY,'Oversample',[],'Subtract',false);
    
    % add background with some spatial variations
    Back = 0.05 .* MaxSrcFlux + rand(Nx,Ny);    
    ImageSrcBack = imUtil.art.addBackground(ImageSrc, Back, 'Subtract', false);
    
    % add noise
    Image = imUtil.art.addNoise(ImageSrcBack,'normal');
    
%     Res = imUtil.sources.findSources(Image);

    %%% build an image from a source list measured from some real data 
    %%% and compare it with the original one
    tic;
    % a high-latitude (tenuous) field:  
    fprintf('LAST subimage from a high-latitude field 346+79:\n');   
    AI1(1)  = AstroImage('~/matlab/data/TestImages/unitTest/LAST_346+79_crop10.fits');    
    Res1(1) = FitRestoreSubtract(AI1(1), 'VarMethod', 'LogHist', 'Threshold', 30, ...
        'RemoveMasked', false, 'RemovePSFCore', false, ...
        'BackPar',{'SubSizeXY','full'}); 
    AC1(1)  = Res1(1).Cat;        
    
    for It = 2:5
        AI1(It)  = AstroImage({Res1(It-1).Diff}); 
        AI1(It).Back = AI1(It-1).Back; AI1(It).Var = AI1(It-1).Var; AI1(It).CatData.JD = AI1(1).CatData.JD;
        Res1(It) = FitRestoreSubtract(AI1(It), 'PSF', Res1(It-1).PSF, 'ReCalcPSF', false, ...
            'ReCalcBack', true, 'VarMethod', 'LogHist', 'Threshold', 5, 'Iteration',It, ...
            'RemoveMasked', false, 'RemovePSFCore', false, ...
            'BackPar',{'SubSizeXY',[128 128]});
        AC1(It) = Res1(It).Cat;        
    end
    
    AI1(1).CatData = merge(AC1); % NB: AC1(1).Table is not updated 
        
    toc; tic;
    % a low-latitude (dense) field:
    fprintf('LAST subimage from a low-latitude field 275-16:\n');    
    AI2(1)  = AstroImage('~/matlab/data/TestImages/unitTest/LAST_275-16_crop22.fits');
    Res2(1) = FitRestoreSubtract(AI2(1),'VarMethod','LogHist','Threshold', 30, ...
        'RemoveMasked', false, 'RemovePSFCore', false,...
        'BackPar',{'SubSizeXY','full'});
    AC2(1)  = Res2(1).Cat;     
        
    for It = 2:5
        Thresh = 5; % 50/It^1.4;
        AI2(It)  = AstroImage({Res2(It-1).Diff}); 
        AI2(It).Back = AI2(It-1).Back; AI2(It).Var = AI2(It-1).Var; AI2(It).CatData.JD = AI2(1).CatData.JD;        
        Res2(It) = FitRestoreSubtract(AI2(It), 'PSF', Res2(It-1).PSF, 'ReCalcPSF', true, ...
            'ReCalcBack', true, ...
            'VarMethod', 'LogHist', 'Threshold', Thresh, 'Iteration',It, ...
            'RemoveMasked', false, 'RemovePSFCore', false,...
            'BackPar',{'SubSizeXY',[128 128]});
        AC2(It) = Res2(It).Cat;        
    end
    
    AI2(1).CatData = merge(AC2); % NB: AC2(1).Table is not updated 
    
    toc;
    
%     ds9(AI1(1).Image,1)
    ds9(Res1(1).Diff,2)
    ds9(Res1(5).Diff,3)
%     ds9(AI2(1).Image,4)
    ds9(Res2(1).Diff,5)
    ds9(Res2(5).Diff,6)
    %
    io.msgLog(LogLevel.Test, 'imUtil.art.unitTest passed');
    Result = true;
    
    DS9_new.regionWrite([AI1(1).CatData.Catalog(:,3) AI1(1).CatData.Catalog(:,4)],'FileName','~/LAST_346+79.reg','Color','cyan','Marker','o','Size',1,'Width',4,'Precision','%.2f','PrintIndividualProp',0);
    DS9_new.regionWrite([AC1(1).Catalog(:,3) AC1(1).Catalog(:,4)],'FileName','~/LAST_346+79_it1.reg','Color','blue','Marker','o','Size',1,'Width',4,'Precision','%.2f','PrintIndividualProp',0);
    DS9_new.regionWrite([AC1(2).Catalog(:,3) AC1(2).Catalog(:,4)],'FileName','~/LAST_346+79_it2.reg','Color','red','Marker','o','Size',1,'Width',4,'Precision','%.2f','PrintIndividualProp',0);
    
    DS9_new.regionWrite([AI2(1).CatData.Catalog(:,3) AI2(1).CatData.Catalog(:,4)],'FileName','~/LAST_275_16.reg','Color','cyan','Marker','o','Size',1,'Width',4,'Precision','%.2f','PrintIndividualProp',0);
    DS9_new.regionWrite([AC2(1).Catalog(:,3) AC2(1).Catalog(:,4)],'FileName','~/LAST_275_16_it1.reg','Color','blue','Marker','o','Size',1,'Width',4,'Precision','%.2f','PrintIndividualProp',0);
    DS9_new.regionWrite([AC2(2).Catalog(:,3) AC2(2).Catalog(:,4)],'FileName','~/LAST_275_16_it2.reg','Color','red','Marker','o','Size',1,'Width',4,'Precision','%.2f','PrintIndividualProp',0);
 
%     S = readtable('~/LAST_275_16_sextractor.cat','FileType','text','NumHeaderLines',14);
%     DS9_new.regionWrite([S.Var2 S.Var3],'FileName','~/LAST_275_16_sextractor.reg','Color','blue','Marker','b','Size',1,'Width',4,'Precision','%.2f','PrintIndividualProp',0);

end

%%% internal functions

function Result = FitRestoreSubtract(AI, Args)

    arguments
       AI
       Args.Iteration   = 1;
       Args.PSF         = [];
       
       Args.VarMethod   = 'LogHist';
       
       Args.MomRadius   = 4; % recommended MomRadius = 1.7 * FWHM ~ 3.8
       
       Args.Threshold   = 5;
       
       Args.PSFFunPar   = {[0.1; 1.0; 1.5]}; % {[0.1; 1.0; 1.5; 2.6; 5]} 
       Args.ReCalcPSF   = false;
       Args.CropPSF     = false;
       
       Args.ReCalcBack  = true;
       
       Args.BackPar     = {'SubSizeXY',[128 128]}; % {'SubSizeXY',[]})
       
       Args.RemoveMasked  = false;  % seem like 'true' does not influence much ? 
       Args.RemovePSFCore = false;  % not decided on it yet
    end
    % measure background and variance
    imProc.background.background(AI, 'ReCalcBack', Args.ReCalcBack, Args.BackPar{:});
    
%     Ind = AI.Var > 1.3 .* AI.Back;
%     AI.Var = AI.Var .* (1-Ind) + AI.Back .* Ind; % experimental
    
    % find sources (without background recalculation)
    AI = imProc.sources.findMeasureSources(AI,'Threshold', Args.Threshold,'ReCalcBack',false,...
        'MomPar',{'MomRadius',Args.MomRadius},'PsfFunPar',Args.PSFFunPar); 
%             'BackPar',{'BackFun',@median,'BackFunPar',{'all'},'VarFun',@imUtil.background.rvar,'SubSizeXY','full'});     
    %
    NumSrc = height(AI.Table);
    fprintf('Iter. %d: bkg = %.0f, var = %.0f, Nobj: %d\n',...
        Args.Iteration,mean(AI.Back,'all'),mean(AI.Var,'all'),NumSrc);
    % insert a column with iteration number into the catalog
    AI.CatData = insertCol(AI.CatData, repmat(Args.Iteration,1,NumSrc)', Inf, 'ITER', {''});
    % measure the PSF or use the previous one (from Args.PSF) 
    if isempty(Args.PSF) || Args.ReCalcPSF
        AI = imProc.psf.populatePSF(AI,'CropByQuantile',Args.CropPSF);
        if isempty(AI.PSF) % if no PSF was measured, use the input PSF
            AI.PSF = Args.PSF;
        end      
    else
        AI.PSF = Args.PSF;        
    end
    % find sources once more with the measured PSF?
    AI = imProc.sources.findMeasureSources(AI,'Threshold', Args.Threshold,'ReCalcBack',false,...
        'MomPar',{'MomRadius',Args.MomRadius},'Psf',AI.PSF,'FlagCR',false); 
    % make PSF photometry
    [AI, Res] = imProc.sources.psfFitPhot(AI);  % produces PSFs shifted to RoundX, RoundY, so there is no need to Recenter     
    % construct and inject sources
    [CubePSF, XY] = imUtil.art.createSourceCube(Res.ShiftedPSF, [Res.RoundY Res.RoundX], Res.Flux, 'Recenter', false);
    ImageSrc = imUtil.art.addSources(repmat(0,size(AI.Image)),CubePSF,XY,'Oversample',[],'Subtract',false);
%     ImageSrcBack = imUtil.art.addBackground(ImageSrc, AI.Back, 'Subtract', false);    % just for testing, do not use it further 
    % make a difference image    
    DiffImage    = AI.Image - ImageSrc;    
    % set pixels with Mask > 0 to the background values
    if Args.RemoveMasked
        Ind = AI.Mask > 0;
        DiffImage(Ind) = AI.Back(Ind);
    end
    % exclude pixels with reconstructed source PSFs 
    if Args.RemovePSFCore
        Ind = ImageSrc > 0;
        DiffImage(Ind) = AI.Back(Ind); % need to be tested and improved to operate only on a 3x3 (5x5?) pixel core 
    end
    % 
    Result.NSrc    = NumSrc;
    Result.PSF     = AI.PSF;
    Result.Cat     = AI.CatData;
    Result.Src     = ImageSrc;
%     Result.SrcBack = ImageSrcBack; % just for testing, we do not use it further 
    Result.Diff    = DiffImage;    
end

