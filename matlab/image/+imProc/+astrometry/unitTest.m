function Result = unitTest()
    % unitTest for +imProc.astrometry
    % Example: imProc.astrometry.unitTest
   
    
    DataSampleDir = tools.os.getTestDataDir;
    PWD = pwd;
    cd(DataSampleDir);
    
    
    % astrometryCore (with catalog data)
    RA     = 1;     % rad
    Dec    = 1;     % rad
    Radius = 1000;  % arcsec
    % get catalog
    Cont   = true;
    try
        Ref    = catsHTM.cone_search('GAIAEDR3', RA, Dec, Radius, 'OutType','AstroCatalog');
    catch
        io.msgStyle(LogLevel.Test, '@passed', 'imProc.astrometry failed probably because GAIA catalog is not installed - skip problem');
        Cont = false;
    end
    if Cont
        
        % work on semi-simulated catalog:
        
        % project catalog with pix scale of 1"/pix
        Scale = 3600.*180./pi;  % ./PixScale ["/pix]
        Cat    = imProc.trans.projection(Ref, RA, Dec, Scale, 'TAN', 'Coo0Units','rad');
        Cat = queryRange(Cat, AstroCatalog.DefNamesMag, [11 19]);
        % plot(Cat.Catalog(:,31),Cat.Catalog(:,32),'.')
        Cat.Catalog(:,31) = Cat.Catalog(:,31) + 10;
        Cat.Catalog(:,32) = Cat.Catalog(:,32) + 20;

        Result = imProc.astrometry.astrometryCore(Cat, 'RA', RA, 'Dec', Dec, 'CooUnits','rad');
    
        % astrometryCore (with real data)
        %cd /home/eran/matlab/images
        %AI = AstroImage('PTF_201211203837_i_p_scie_t091230_u014655064_f02_p100037_c02.fits');
        %AI.crop([500 1500 1500 2500]);   % image saved as FITS_Cropped.fits
    end
    % test on real image
    AI = AstroImage('PTF_Cropped.fits');

    % detect sources in image
    imProc.background.background(AI,'VarFun','fromback');
    imProc.sources.findMeasureSources(AI);
    %ds9(AI)
    %ds9.plot(AI.CatData.Catalog(:,1:2))

    RAD = 180./pi;
    %CatG = catsHTM.cone_search('GAIAEDR3', 149.1026601./RAD, 69.4547688./RAD, 1400, 'OutType','AstroCatalog');
    if Cont
        [Result, AstrometricCat] = imProc.astrometry.astrometryCore(AI.CatData, 'RA',149.1026601, 'Dec',69.4547688+0.1, 'CatColNamesMag','MAG_CONV_2');
        % save AstrometricCat_PTF_Cropped.mat AstrometricCat
    end
    
    % perform offline testing - i.e., GAIA catalog is not available via
    % catsHTM
    load AstrometricCat_PTF_Cropped.mat   % from some reason CooType = 'deg'?!
    
    Result = imProc.astrometry.astrometryCore(AI.CatData, 'Scale',1.014, 'RA',149.1026601, 'Dec',69.4547688, 'CatColNamesMag','MAG_CONV_2','CatName',AstrometricCat);
    [Result,~,AI.CatData] = imProc.astrometry.astrometryCore(AI.CatData, 'Scale',1.01, 'RA',149.1026601, 'Dec',69.4547688, 'CatColNamesMag','MAG_CONV_2','CatName',AstrometricCat);
    
    % test bad initial conditions
    if Cont
        [Result, AstrometricCat] = imProc.astrometry.astrometryCore(AI.CatData, 'RA',149.1026601, 'Dec',69.4547688+0.1, 'CatColNamesMag','MAG_CONV_2');
    end
    % re use existing AstrometricCat
    [Result] = imProc.astrometry.astrometryCore(AI.CatData, 'RA',149.1026601, 'Dec',69.4547688+0.1, 'CatColNamesMag','MAG_CONV_2', 'CatName',AstrometricCat);
    % automatically find CatRadius
    [Result] = imProc.astrometry.astrometryCore(AI.CatData, 'RA',149.1026601, 'Dec',69.4547688+0.1, 'CatColNamesMag','MAG_CONV_2', 'CatName',AstrometricCat, 'CatRadius',[]);
    % work on two catalogs
    if Cont
        [Result, AstrometricCat] = imProc.astrometry.astrometryCore([AI.CatData, AI.CatData], 'RA',149.1026601, 'Dec',69.4547688+0.1, 'CatColNamesMag','MAG_CONV_2');
    end
    
    % timing
    %tic;
    %for I=1:1:30
    %    [Result, AstrometricCat] = imProc.astrometry.astrometryCore(AI.CatData, 'RA',149.1026601, 'Dec',69.4547688+0.1, 'CatColNamesMag','MAG_CONV_2');
    %end
    %toc
    
    
    % astrometryCheck  
    R = imProc.astrometry.astrometryCheck(AI.CatData, 'WCS',Result.WCS, 'CatName',AstrometricCat)
    
    % astrometryRefine
    % 1. why RR.ResFit rms are (seems) in arcsec??? or else, while in Result they are
    % in deg
    % TODO: go over code - note that the match is done in RA/Dec - compare
    % to Core
    RR = imProc.astrometry.astrometryRefine(AI.CatData, 'WCS',Result.WCS, 'CatName',AstrometricCat, 'RA',149.1026601, 'Dec',69.4547688);
    
    
    
    
    AI = AstroImage('PTF_201411204943_i_p_scie_t115144_u023050379_f02_p100037_c02.fits');
    W=AstroWCS.header2wcs(AI.HeaderData);
    
    
    cd(PWD);
    io.msgStyle(LogLevel.Test, '@passed', 'imProc.astrometry test passed')
    Result = true;
    
    
end
   