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
%     

    if 1==0
        AI = AstroImage({'LAST_346+79_crop10.fits', 'LAST_275-16_crop22.fits'});
    %     AI = AstroImage({'LAST_275-16_crop22.fits'}); 
        
        cd(PWD)
        
        AI0 = AI.copy;
        
        tic
        
        AI0 = imProc.background.background(AI0);  
        AI0 = imProc.sources.findMeasureSources(AI0);
        AI0 = imProc.psf.populatePSF(AI0);
        AI0 = imProc.sources.psfFitPhot(AI0);
        fprintf('%d sources \n',height(AI0(1).CatData.Catalog));
        fprintf('%d sources \n',height(AI0(2).CatData.Catalog));
        
        toc
            
        tic
        
        [AI, SourceLess] = imProc.sources.mextractor(AI,'Threshold',[30 10 5],'MomRadius',[4 6 6],'FitRadius',[2 2 2],...
            'Verbose',true, 'WriteDs9Regions',true,'FindWithEmpiricalPSF',true,'SaveSourcelessImage',true,'RedNoiseFactor',1.3); 
    
    %     [AI, SourceLess] = imProc.sources.mextractor(AI,'Threshold',[30 10 5],'MomRadius',[4 6 6],'FitRadius',[3 3 3],...
    %         'Verbose',false, 'WriteDs9Regions',false,'FindWithEmpiricalPSF',true,'SaveSourcelessImage',false,'RedNoiseFactor',1.3);
    % 
        % NB: 'RedNoiseFactor' = 1.3 -- a number of spurious sources are still found, while some of the obvious sources are not revealed 
    
        toc 
        
    %     compare the multi-iteration results with those from usual
    %     single-iteration source search and PSF-photometry:
    % 
    %     SI = imProc.sources.findMeasureSources(SI);                                               
    %     [SI] = imProc.psf.populatePSF(SI, 'Method', 'new');
    %     [SI, ResPSF] = imProc.sources.psfFitPhot(SI, 'CreateNewObj',false);
    % 
    %     figure(1); clf
    %     subplot(2,1,1); histogram(AI(1).CatData.Catalog(:,29));
    %     hold on; histogram(SI(1).CatData.Catalog(:,33)); 
    %     title('Tenuous field'); xlabel Mag; ylabel('N_{obj}')
    %     subplot(2,1,2); histogram(AI(2).CatData.Catalog(:,29));
    %     hold on; histogram(SI(2).CatData.Catalog(:,33)); xlim([10 20])
    %     title('Dense field'); xlabel Mag; ylabel('N_{obj}')
        
        ds9(AI(1).Image,1); ds9.load_region('~/346+79_it1.reg'); ds9.load_region('~/346+79_it2.reg'); ds9.load_region('~/346+79_it3.reg')
        ds9(SourceLess(1).Image,2) 
        ds9(AI(2).Image,3); ds9.load_region('~/275-16_it1.reg'); ds9.load_region('~/275-16_it2.reg'); ds9.load_region('~/275-16_it3.reg')
        ds9(SourceLess(2).Image,4)     
            
        % test with LAST data from Marvin:
    
        D=db.Db; D.User = 'last_user'; D.Password = 'physics'; D.Conn; D.useDB('last');
        T=D.query("SELECT * FROM last.visit_images WHERE abs(ra-285.385) < 0.4 AND abs(dec-22.615) < 0.4 AND (midjd < 2460500) AND (midjd > 2460460);");
        Nobs = height(T);
        F  = 1:3;
        AI = pipeline.last.queryDB.loadProducts(T(F,:),'coadd','Image+','ExtraOutProduct',["Mask", "PSF", "Cat"],...
            'table2pathArgs',{'BasePath','/mnt/marvin'});
        AI = imProc.sources.mextractor(AI, 'Verbose',true, 'FindWithEmpiricalPSF',true);
    
        % 
    end
    
    Result = true;
end