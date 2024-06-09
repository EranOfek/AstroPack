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
    
    % high-latitude (tenuous) field:
    fprintf('LAST subimage from a high-latitude field 346+79:\n');
    tic;
    AI1(1)  = AstroImage('~/matlab/data/TestImages/unitTest/LAST_346+79_crop10.fits');    
    Res1(1) = FitRestoreSubtract(AI1, 'VarMethod', 'LogHist', 'Threshold', 5);
    AC1(1)   = Res1(1).Cat;
    
    for It = 2:10
        AI1(It)  = AstroImage({Res1(It-1).Diff});
        Res1(It) = FitRestoreSubtract(AI1(It), 'PSF', Res1(1).PSF, 'VarMethod', 'LogHist', 'Threshold', 5, 'Iteration',It);
        AC1(It) = Res1(It).Cat;
    end
    
    AI1(1).CatData = merge(AC1); % NB: AC1(1).Table is not updated 
        
    toc; tic;
    % low-latitude (dense) field:
    fprintf('LAST subimage from a low-latitude field 275-16:\n');
    AI2(1)  = AstroImage('~/matlab/data/TestImages/unitTest/LAST_275-16_crop22.fits');
    Res2(1) = FitRestoreSubtract(AI2(1),'VarMethod','LogHist');
    AC2(1)   = Res2(1).Cat;
        
    for It = 2:10
        AI2(It)  = AstroImage({Res2(It-1).Diff});
        Res2(It) = FitRestoreSubtract(AI2(It), 'PSF', Res2(1).PSF, 'VarMethod', 'LogHist', 'Threshold', 5, 'Iteration',It);
        AC2(It) = Res2(It).Cat;
    end
    
    AI2(1).CatData = merge(AC2); % NB: AC2(1).Table is not updated 
    
    toc;
    io.msgLog(LogLevel.Test, 'imUtil.art.unitTest passed');
    Result = true;
end

%%% internal functions

function Result = FitRestoreSubtract(AI, Args)

    arguments
       AI
       Args.PSF       = [];
       Args.Threshold = 5;
       Args.VarMethod = 'LogHist';
       Args.SubtractBack = false;
       Args.Iteration = 1;
    end
    % find sources
    if strcmpi(Args.VarMethod,'loghist')
        AI = imProc.sources.findMeasureSources(AI,'Threshold', Args.Threshold);
    elseif strcmpi(Args.VarMethod,'median')
        AI = imProc.sources.findMeasureSources(AI,'Threshold', Args.Threshold,'BackPar',...
        {'BackFun',@median,'BackFunPar',{'all'},'VarFun',@imUtil.background.rvar,'SubSizeXY','full'});     
    else
        error('not supported VarMethod ');
    end
    fprintf('New objects at iter. %d: %d\n',Args.Iteration,height(AI.Table));
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
    X1Y1 = [Res.Y Res.X]; % note the order! 
    % construct and inject sources
    [CubePSF, XY] = imUtil.art.createSourceCube(Res.ShiftedPSF, X1Y1, Res.Flux, 'Recenter', false);
    % 
    Image0 = repmat(0,size(AI.Image));
    ImageSrc = imUtil.art.addSources(Image0,CubePSF,XY,'Oversample',[],'Subtract',false);
    ImageSrcBack = imUtil.art.addBackground(ImageSrc, AI.Back, 'Subtract', false);    
    % make a difference image    
    if Args.SubtractBack % do we ever need to remove the background before the next iteration? 
        DiffImage    = AI.Image - ImageSrcBack; 
    else
        DiffImage    = AI.Image - ImageSrc;
    end
    % exclude pixels with Mask > 0
    DiffImageMasked = DiffImage .* (AI.Mask == 0);            
    % exclude pixels with reconstructed source PSFs 
    DiffImageMaskedPSF = DiffImageMasked .* (ImageSrc == 0); 
    % 
    Result.Cat     = AI.CatData;
    Result.Src     = ImageSrc;
    Result.SrcBack = ImageSrcBack;
    Result.Diff    = DiffImage;
    Result.DiffMask= DiffImageMasked;
    Result.DiffPSF = DiffImageMaskedPSF;
end

