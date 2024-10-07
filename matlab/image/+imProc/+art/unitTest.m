function Result = unitTest()
    % imProc.art.unitTest
    % Example: imUtil.art.unitTest
    % Author : A.M. Krasilshchikov (2024 Jun)     
    io.msgLog(LogLevel.Test, 'imProc.art.unitTest started');
    
    %
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
    
    
    
    %
    io.msgLog(LogLevel.Test, 'imUtil.art.unitTest passed');
    Result = true;
end
