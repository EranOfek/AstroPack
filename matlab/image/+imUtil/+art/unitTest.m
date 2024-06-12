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
    % high-latitude (tenuous) field:  
    fprintf('LAST subimage from a high-latitude field 346+79:\n');   
    AI1(1)  = AstroImage('~/matlab/data/TestImages/unitTest/LAST_346+79_crop10.fits');    
    Res1(1) = FitRestoreSubtract(AI1, 'VarMethod', 'LogHist', 'Threshold', 30, ...
        'RemoveMasked', false, 'RemovePSFCore', false, ...
        'BackPar',{'SubSizeXY','full'}); 
    AC1(1)  = Res1(1).Cat;        
    
    for It = 2:5
        AI1(It)  = AstroImage({Res1(It-1).Diff}); AI1(It).Back = AI1(It-1).Back; AI1(It).Var = AI1(It-1).Var;
        Res1(It) = FitRestoreSubtract(AI1(It), 'PSF', Res1(1).PSF, 'ReCalcBack', true, ...
            'VarMethod', 'LogHist', 'Threshold', 5, 'Iteration',It, ...
            'RemoveMasked', false, 'RemovePSFCore', false, ...
            'BackPar',{'SubSizeXY','full'});
        AC1(It) = Res1(It).Cat;        
    end
    
    AI1(1).CatData = merge(AC1); % NB: AC1(1).Table is not updated 
        
    toc; tic;
    % low-latitude (dense) field:
    fprintf('LAST subimage from a low-latitude field 275-16:\n');    
    AI2(1)  = AstroImage('~/matlab/data/TestImages/unitTest/LAST_275-16_crop22.fits');
    Res2(1) = FitRestoreSubtract(AI2(1),'VarMethod','LogHist','Threshold', 30, ...
        'RemoveMasked', false, 'RemovePSFCore', false,...
        'BackPar',{'SubSizeXY','full'});
    AC2(1)  = Res2(1).Cat;     
        
    for It = 2:5
        AI2(It)  = AstroImage({Res2(It-1).Diff}); AI2(It).Back = AI2(It-1).Back; AI2(It).Var = AI2(It-1).Var;
        Res2(It) = FitRestoreSubtract(AI2(It), 'PSF', Res2(1).PSF, 'ReCalcBack', true, ...
            'VarMethod', 'LogHist', 'Threshold', 5, 'Iteration',It, ...
            'RemoveMasked', false, 'RemovePSFCore', false,...
            'BackPar',{'SubSizeXY','full'});
        AC2(It) = Res2(It).Cat;        
    end
    
    AI2(1).CatData = merge(AC2); % NB: AC2(1).Table is not updated 
    
    toc;
    
    ds9(Res1(1).Diff,2)
    ds9(Res1(5).Diff,3)
    ds9(AI2(1).Image,4)
    ds9(Res2(1).Diff,5)
    ds9(Res2(5).Diff,6)
    %
    io.msgLog(LogLevel.Test, 'imUtil.art.unitTest passed');
    Result = true;
end

%%% internal functions

function Result = FitRestoreSubtract(AI, Args)

    arguments
       AI
       Args.Iteration   = 1;
       Args.PSF         = [];
       
       Args.Threshold   = 5;
       
       Args.PSFFunPar   = {[0.1; 1.0; 1.5]}; % {[0.1; 1.0; 3.0; 5.0]};
       
       Args.VarMethod   = 'LogHist';   
       Args.ReCalcBack  = true;
       
       Args.BackPar     = {'SubSizeXY',[128 128]}; % {'SubSizeXY',[]})
       
       Args.RemoveMasked  = false;  % seem like 'true' does not influence much ? 
       Args.RemovePSFCore = false;
    end
    % find sources, measure background
    if strcmpi(Args.VarMethod,'loghist')
        AI = imProc.sources.findMeasureSources(AI,'Threshold', Args.Threshold,'ReCalcBack',Args.ReCalcBack,...
              'PsfFunPar',Args.PSFFunPar,'BackPar',Args.BackPar); 
    elseif strcmpi(Args.VarMethod,'median')
        AI = imProc.sources.findMeasureSources(AI,'Threshold', Args.Threshold,'ReCalcBack',Args.ReCalcBack,...
            'BackPar',{'BackFun',@median,'BackFunPar',{'all'},'VarFun',@imUtil.background.rvar,'SubSizeXY','full'});     
    else
        error('not supported VarMethod ');
    end
    NumSrc = height(AI.Table);
    fprintf('Iter. %d: bkg = %.0f, var = %.0f, Nobj: %d\n',...
        Args.Iteration,mean(AI.Back,'all'),mean(AI.Var,'all'),NumSrc);
    % insert a column with iteration number into the catalog
    AI.CatData = insertCol(AI.CatData, repmat(Args.Iteration,1,NumSrc)', Inf, 'ITER', {''});
    % if a PSF is given, do not change it 
    if isempty(Args.PSF)
        AI = imProc.psf.populatePSF(AI,'CropByQuantile',false);
        Result.PSF = AI.PSF;
    else
        AI.PSF = Args.PSF;
        Result.PSF = Args.PSF;
    end
    % make PSF photometry
    [AI, Res] = imProc.sources.psfFitPhot(AI);  % produces PSFs shifted to RoundX, RoundY, so there is no need to Recenter     
    % construct and inject sources
    [CubePSF, XY] = imUtil.art.createSourceCube(Res.ShiftedPSF, [Res.RoundY Res.RoundX], Res.Flux, 'Recenter', false);
    ImageSrc = imUtil.art.addSources(repmat(0,size(AI.Image)),CubePSF,XY,'Oversample',[],'Subtract',false);
    ImageSrcBack = imUtil.art.addBackground(ImageSrc, AI.Back, 'Subtract', false);    
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
        DiffImage(Ind) = AI.Back(Ind); % need to be tested and improved to operate only on a 3x3 pixel core 
    end
    % 
    Result.NSrc    = NumSrc;
    Result.Cat     = AI.CatData;
    Result.Src     = ImageSrc;
    Result.SrcBack = ImageSrcBack;
    Result.Diff    = DiffImage;    
end

