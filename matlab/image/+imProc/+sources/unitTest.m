function Result = unitTest
    % unitTest for imProc.sources

    %io.msgStyle(LogLevel.Test, '@start', 'imProc.sources test started')

    DataSampleDir = tools.os.getTestDataDir;
    PWD = pwd;
    cd(DataSampleDir);


    % addMagCols
    % AC=AstroCatalog('LAST.01.02.01_20230115.170832.639_clear_050+09_001_001_024_sci_coadd_Cat_001.fits');
    % AC2 = AC.copy();
    % NamesFlux = AC.ColNames(contains(AC.ColNames, 'FLUX_'));
    % NamesFluxErr = AC.ColNames(contains(AC.ColNames, 'FLUXERR_'));
    % [NamesBoth,IndFlux,IndFluxErr] = intersect(erase(NamesFlux,'FLUX_'),erase(NamesFluxErr,'FLUXERR_'));
    % MagNames = cellstr("MAG_"+NamesBoth);
    % MagNamesNew = strrep(MagNames,'MAG_','MAG2_');
    % MagErrNames = cellstr("MAGERR_"+NamesBoth);
    % MagErrNamesNew = strrep(MagErrNames,'MAGERR_','MAGERR2_');
    % AC2 = imProc.sources.addMagCols(AC2,"FluxCols",NamesFlux(IndFlux),"FluxErrCols",NamesFluxErr(IndFluxErr),"MagStr",MagNames,"MagErrStr",MagErrNames);
    % AC2 = imProc.sources.addMagCols(AC2,"FluxCols",NamesFlux(IndFlux),"FluxErrCols",NamesFluxErr(IndFluxErr),"MagStr",MagNamesNew,"MagErrStr",MagErrNamesNew);
    % 
    % assert(all(getCol(AC2, MagNames)==getCol(AC2,MagNamesNew),'all') ,'error in addMagCols');
    % 
    % AC2 = imProc.sources.addMagCols(AC2,"FluxCols",NamesFlux(IndFlux),"FluxErrCols",NamesFluxErr(IndFluxErr),"MagStr",MagNames,"MagErrStr",MagErrNames,"IsLuptitude",false);
    
    % testing multi-iteration PSF photometry 
    
    AI  = [AstroImage('LAST_346+79_crop10.fits') AstroImage('LAST_275-16_crop22.fits')];  
%     AI.Mask = % fill in the masks 

    [AI, SourceLess] = imProc.sources.mextractor(AI,'Verbose',true,'FindWithEmpiricalPSF',true);
    
    ds9(AI(1).Image,1); ds9(SourceLess(1).Image,2)
    ds9(AI(2).Image,3); ds9(SourceLess(2).Image,4)
        
    cd(PWD)

    Result = true;
end