function Result = unitTest()
    % imUtil.art.unitTest
    % Example: imUtil.art.unitTest
    io.msgLog(LogLevel.Test, 'imUtil.art.unitTest test started');
    
    % create an artificial sky image from object coordinates and PSFs
    Nsrc = 1000; Nx = 2000; Ny = Nx; MaxSrcFlux = 1000;
    
    Flux = MaxSrcFlux .* rand(Nsrc,1);
    X1Y1 = Nx.* rand(Nsrc,2);
    for i = 1:Nsrc; PSF(:,:,i) = imUtil.kernel2.gauss([4 4 0],[24 24]) + 1e-2*rand(24,24); end
    
    % create a list of shifted and resampled fluxed PSF stamps
    [CubePSF, XY] = imUtil.art.createSourceCube(PSF, X1Y1, Flux, 'Recenter', true, 'Oversample', 3, 'PositivePSF', true);
    
    % create an empty image with some small noise
    Image0 = rand(Nx,Ny);
    
    % fill the image with sources
    ImageSrc = imUtil.art.injectSources(Image0,CubePSF,XY);
    
    % add background with some spatial variations
    Back = 0.05 .* MaxSrcFlux + rand(Nx,Ny);    
    ImageSrcBack = imUtil.art.addBackground(ImageSrc, Back, 'Subtract', false);
    
    % add noise
    Image = imUtil.art.addNoise(ImageSrcBack,'normal');
    
%     Res = imUtil.sources.findSources(Image);
    
    io.msgLog(LogLevel.Test, 'imUtil.art.unitTest test passed');
    Result = true;
end

