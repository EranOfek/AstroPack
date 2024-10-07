function Result = unitTest()
    % imProc.art.unitTest
    % Example: imUtil.art.unitTest
    % Author : A.M. Krasilshchikov (2024 Jun)     
    io.msgLog(LogLevel.Test, 'imProc.art.unitTest started');
    
    %
    cprintf('blue','Source injection test:\n');
    Nsrc = 100;
    AI0  = AstroImage('~/matlab/data/TestImages/unitTest/LAST_346+79_crop10.fits');     
    AI0  = imProc.sources.findMeasureSources(AI0,'Threshold', 10, 'PsfFunPar',{[0.1; 1.0; 1.5]});
    AI0  = imProc.psf.populatePSF(AI0,'CropByQuantile',false);
    AI0  = imProc.sources.psfFitPhot(AI0); 
    PSF  = AI0.PSF;
    Cat  = 1700.*rand(Nsrc,2);
    Flux = 10.*mean(AI0.Back,'all').*rand(Nsrc,1);
    
    fprintf('Original sources: %d\n',height(AI0.CatData.Catalog))
    
    % NB! X and Y coordinates in the Cat should be transposed! 
    [AI, InjectedCat] = imProc.art.injectSources(AI0, Cat, PSF, Flux, 'PositivePSF', true,'CreateNewObj',true,'UpdateCat',true);     
    
    fprintf('Added sources: %d\n',height(InjectedCat.Catalog))
    fprintf('Total sources: %d\n',height(AI.CatData.Catalog))
    %
    
    cprintf('blue','Sky image simulation test:\n');
    % simulate an image (by default -- based on LAST source statistics in the field 275-16)
    [SimAI, SimCat] = imProc.art.simulateSkyImage('WriteFiles',false);
    % extract the sources with mextractor 
    [SimAI, SourceLess] = imProc.sources.mextractor(SimAI,'Verbose',true,...
        'WriteDs9Regions',true,'FindWithEmpiricalPSF',true,...
        'RedNoiseFactor',1.3);
    % compare the input and output catalogs
    SimCat.sortrows('Y1');
    SimAI.CatData.sortrows('Y1');
    %
    io.msgLog(LogLevel.Test, 'imUtil.art.unitTest passed');
    Result = true;
end
