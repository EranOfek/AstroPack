function Result = unitTest()
    % imUtil.art.unitTest
    % Example: imUtil.art.unitTest
    io.msgLog(LogLevel.Test, 'imUtil.art.unitTest test started');
    
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
    
    AI = AstroImage('~/matlab/data/TestImages/unitTest/LAST_subimage.fits');
    AI = imProc.sources.findMeasureSources(AI,'Threshold', 5);
    AI = imProc.psf.populatePSF(AI);
    [AI, Res] = imProc.sources.psfFitPhot(AI);  % produces PSFs shifted to RoundX, RoundY, so there is no need to Recenter 
    X1Y1 = [Res.Y Res.X]; % note the order! 
    
    [CubePSF, XY] = imUtil.art.createSourceCube(Res.ShiftedPSF, X1Y1, Res.Flux, 'Recenter', false);
    
    Image0 = repmat(0,size(AI.Image));
    ImageSrc = imUtil.art.addSources(Image0,CubePSF,XY,'Oversample',[],'Subtract',false);
    ImageSrcBack = imUtil.art.addBackground(ImageSrc, AI.Back, 'Subtract', false);
    DiffImage    = AI.Image - ImageSrcBack;
    DiffImageMasked = DiffImage .* (AI.Mask == 0);
    
    % trying another iteration with the difference image    
    AI2 = AstroImage({DiffImageMasked});
    AI2 = imProc.sources.findMeasureSources(AI2,'Threshold', 3,'BackPar',...
        {'BackFun',@median,'BackFunPar',{'all'},'VarFun',@imUtil.background.rvar,'SubSizeXY','full'}); 
    AI2 = imProc.psf.populatePSF(AI2);
    [AI2, Res2] = imProc.sources.psfFitPhot(AI2); 
    
    X1Y1 = [Res2.Y Res2.X];
    [CubePSF, XY] = imUtil.art.createSourceCube(Res2.ShiftedPSF, X1Y1, Res2.Flux, 'Recenter', false);
    ImageSrc2 = imUtil.art.addSources(Image0,CubePSF,XY,'Oversample',[],'Subtract',false);
    ImageSrcBack2 = imUtil.art.addBackground(ImageSrc2, AI2.Back, 'Subtract', false);
    DiffImage2    = AI2.Image - ImageSrcBack2;
    DiffImageMasked2 = DiffImage2 .* (AI2.Mask == 0);
    
    io.msgLog(LogLevel.Test, 'imUtil.art.unitTest test passed');
    Result = true;
end

