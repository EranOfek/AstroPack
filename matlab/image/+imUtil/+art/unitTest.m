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
    ImageSrc = imUtil.art.injectSources(Image0,CubePSF,XY,'Oversample',[],'Subtract',false);
    
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
    [AI, Res] = imProc.sources.psfFitPhot(AI);
    X1Y1 = [Res.Y Res.X]; % note the order! 
    
    [CubePSF, XY] = imUtil.art.createSourceCube(Res.ShiftedPSF, X1Y1, Res.Flux, 'Recenter', false, ...
        'RecenterMethod','fft', 'PositivePSF', true);
    
    Image0 = repmat(0,size(AI.Image));
    ImageSrc = imUtil.art.injectSources(Image0,CubePSF,XY,'Oversample',[],'Subtract',false);
    ImageSrcBack = imUtil.art.addBackground(ImageSrc, AI.Back, 'Subtract', false);
    DiffImage    = AI.Image - ImageSrcBack;
    
    io.msgLog(LogLevel.Test, 'imUtil.art.unitTest test passed');
    Result = true;
end

