function Result = unitTest
    % unitTest for imProc.sources

    %io.msgStyle(LogLevel.Test, '@start', 'imProc.sources test started')

    %% imUtil.sources.psfPhotCube / no noise no back test

    Nsrc = 1000;
    Flux = ones(1,1,Nsrc);
    P=imUtil.kernel2.gauss(2);
    DX = rand(Nsrc,1).*2 - 1;
    DY = rand(Nsrc,1).*2 - 1;
    Ps = imUtil.trans.mex.shift_lanczos3(P, DX, DY).*Flux;
    tic;[Res,Sub]=imUtil.sources.psfPhotCube(Ps, 'PSF',P);T1=toc;
    tic;[Res,Sub]=psfPhotCube(Ps, 'PSF',P);T2=toc;
    fprintf(' New psfPhotCube is x %f faster\n',T1./T2);

    DiffX = DX - Res.DX;
    DiffY = DY - Res.DY;

    if abs(mean(DiffX))>0.001 || abs(mean(DiffY))>0.001
        error('Problem with imUtil.sources.psfPhotCube / no noise tes - mean DX,DY');
    end

    fprintf('Summary of : imUtil.sources.psfPhotCube / no noise no back test\n');
    fprintf('   Mean DiffX = %f\n',mean(DiffX));
    fprintf('   Mean DiffY = %f\n',mean(DiffY));
    fprintf('   Std DiffX = %f\n',std(DiffX));
    fprintf('   Std DiffY = %f\n',std(DiffX));
    fprintf('   Mean flux ratio (Real/Measured): %f\n', mean(squeeze(Flux)./Res.Flux));
    fprintf('   Std flux ratio (Real/Measured): %f\n', std(squeeze(Flux)./Res.Flux));

    %% imUtil.sources.psfPhotCube / with noise and back test
    %%
    FitRadius = 3;
    SigmaW = 2;
    P = imUtil.kernel2.gauss(SigmaW);
    Nsrc = 1000;
    FluxBase = 850;
    BackBase = 100;
    Flux = zeros(1,1,Nsrc) + FluxBase;
    
    ExpSN_Inf = sqrt(sum((P.*FluxBase).^2./(P.*FluxBase + BackBase),'all'))
    
    [Ny,Nx] = size(P);
    Xc = Nx.*0.5 + 0.5;
    Yc = Ny.*0.5 + 0.5;
    [X,Y] = meshgrid(1:Nx,1:Ny);
    R2 = (X - Xc).^2 + (Y - Yc).^2;
    Flag = R2 < FitRadius.^2;
    
    ExpSN = sqrt(sum(((P.*FluxBase).^2 ./ (P.*FluxBase + BackBase)) .* Flag, 'all'));


    Back = BackBase .* ones(Nsrc,1);
    
    DX = rand(Nsrc,1).*2 - 1;
    DY = rand(Nsrc,1).*2 - 1;
    
    Ps = imUtil.trans.mex.shift_lanczos3(P, DX, DY).*Flux + BackBase;
    Ps = poissrnd(Ps);
    
    Nsim=10;
    tic;for Isim=1:Nsim, [Res,Sub]=imUtil.sources.psfPhotCube(Ps, 'PSF',P, 'Back',Back, 'Std',sqrt(Back), 'FitRadius',FitRadius, 'UseMex',false); end, T1=toc;
    tic;for Isim=1:Nsim, [Res,Sub] = imUtil.sources.psfPhotCube_NEW(Ps, 'PSF', P, 'Back', Back, 'Std', sqrt(Back), 'Method', '1D', 'FitRadius',FitRadius, 'SmallStep',0.01, 'MaxStep',0.2, 'MaxIter',8, 'ConvThresh',1e-3);end, T2=toc;
    fprintf(' New psfPhotCube is x %f faster\n',T1./T2);


    DiffX = DX - Res.DX;
    DiffY = DY - Res.DY;
    
    fprintf('Summary of : psfPhotCube / with noise and back test\n');
    fprintf('   Mean DiffX = %f\n',mean(DiffX));
    fprintf('   Mean DiffY = %f\n',mean(DiffY));
    fprintf('   Expected noise in astrometry: %f\n', SigmaW./ExpSN);
    fprintf('   Std DiffX = %f\n',std(DiffX));
    fprintf('   Std DiffY = %f\n',std(DiffY));
    fprintf('   Mean flux ratio (Real/Measured): %f\n', mean(squeeze(Flux)./Res.Flux));
    fprintf('   Std flux ratio (Real/Measured): %f\n', std(squeeze(Flux)./Res.Flux));
    fprintf('   Mean S/N = %f (expected with FitRadius = %f / full radius %f)\n', mean(Res.SNm), ExpSN, ExpSN_Inf);
    fprintf('   Std S/N = %f\n',std(Res.SNm));




    %%

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