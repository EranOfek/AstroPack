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
    LocalDisk   = false;
    try
        Ref    = catsHTM.cone_search('GAIAEDR3', RA, Dec, Radius, 'OutType','AstroCatalog');
    catch
        io.msgStyle(LogLevel.Test, '@failed', 'imProc.astrometry failed probably because GAIA catalog is not installed - skip problem');
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
           
    % test on real image distributed with AstroPack
    AI = AstroImage('PTF_Cropped.fits');
    % detect sources in image
    imProc.background.background(AI,'VarFun','fromback');
    imProc.sources.findMeasureSources(AI);
    %ds9(AI)
    %ds9.plot(AI.CatData.Catalog(:,1:2))
    RAD = 180./pi;
    %CatG = catsHTM.cone_search('GAIAEDR3', 149.1026601./RAD, 69.4547688./RAD, 1400, 'OutType','AstroCatalog');
    
    if ~LocalDisk
        % if GAIA-EDR3 catalog is available locally
        [Result, AI.CatData, AstrometricCat] = imProc.astrometry.astrometryCore(AI.CatData, 'RA',149.1026601, 'Dec',69.4547688+0.1, 'CatColNamesMag','MAG_CONV_2');
        AI = AI.propagateWCS;
        
        % test with bad initial conditions
        [Result, AstrometricCat] = imProc.astrometry.astrometryCore(AI.CatData, 'RA',149.1026601, 'Dec',69.4547688+0.1, 'CatColNamesMag','MAG_CONV_2');
        
        [Result, AI, AstrometricCat] = imProc.astrometry.astrometryCore(AI, 'RA',149.1026601, 'Dec',69.4547688, 'CatColNamesMag','MAG_CONV_2');
        
        % save AstrometricCat_PTF_Cropped.mat AstrometricCat
    else
        % GAIA catalog is not available locally
        io.files.load1('AstrometricCat_PTF_Cropped.mat');   % from some reason CooType = 'deg'?!
        
        Tran = Tran2D('poly3');
        Tran.symPoly;
        JD = AI.julday;
          
        [Result] = imProc.astrometry.astrometryCore(AI, 'Scale',1.014, 'RA',149.1026601, 'Dec',69.4547688, 'CatColNamesMag','MAG_CONV_2','CatName',AstrometricCat, 'Tran',Tran, 'EpochOut',JD);
        
        tic;
        for I=1:1:10
        [Result, AI] = imProc.astrometry.astrometryCore(AI, 'Scale',1.014, 'RA',149.1026601, 'Dec',69.4547688, 'CatColNamesMag','MAG_CONV_2','CatName',AstrometricCat, 'Tran',Tran, 'EpochOut',JD);
        end
        toc        
        
        % test with bad initial conditions
        [Result, AI] = imProc.astrometry.astrometryCore(AI.CatData, 'RA',149.1026601, 'Dec',69.4547688+0.1, 'CatColNamesMag','MAG_CONV_2', 'CatName',AstrometricCat);
        
    end
    
    % perform offline testing - i.e., GAIA catalog is not available via
    % catsHTM
    Out = imProc.astrometry.addCoordinates2catalog(AI,'WCS',Result.WCS,'UpdateCoo',true);
        
    [SucessFlag, QualitySummary] = imProc.astrometry.assessAstrometricQuality(Result.ResFit)
    
    % astrometryCheck  
    R = imProc.astrometry.astrometryCheck(AI.CatData, 'WCS',Result.WCS, 'CatName',AstrometricCat)
        
    % astrometryRefine
    % 1. why RR.ResFit rms are (seems) in arcsec??? or else, while in Result they are
    % in deg
    % TODO: go over code - note that the match is done in RA/Dec - compare
    % to Core
    
    Tran = Tran2D('poly4');
    Tran.symPoly;
    JD = AI.julday;
          
    tic;  % 4.7s -> 4.5s -> 3.8s
    for I=1:1:100
    [RR, AI] = imProc.astrometry.astrometryRefine(AI, 'WCS',[], 'CatName',AstrometricCat, 'RA',149.1026601, 'Dec',69.4547688,'Tran',Tran,'EpochOut',JD);
    end
    toc
    
    AI = AstroImage('PTF_Cropped.fits');
    % detect sources in image
    imProc.background.background(AI,'VarFun','fromback');
    imProc.sources.findMeasureSources(AI);
    
    [Result, AI] = imProc.astrometry.astrometryCore(AI, 'Scale',1.01, 'RA',149.1026601, 'Dec',69.4547688, 'CatColNamesMag','MAG_CONV_2','CatName',AstrometricCat);
    
    AI.WCS = AstroWCS.header2wcs(AI.HeaderData);
    RR = imProc.astrometry.astrometryRefine(AI.CatData, 'WCS',AI.WCS, 'RA',149.1026601, 'Dec',69.4547688);
    
    
    AI = AstroImage('PTF_201411204943_i_p_scie_t115144_u023050379_f02_p100037_c02.fits');
    W=AstroWCS.header2wcs(AI.HeaderData);
        
    % test on LAST images
    cd /data/euler/archive/LAST/2020/08/22/proc
    AI = AstroImage('LAST.0.1_20200823.002733.839_clear__sci_proc_im_000.fits');  % M42
    AI = AstroImage('LAST.0.1_20200823.002412.958_clear__sci_proc_im_000.fits');  % NGC253
    AI = AstroImage('LAST.0.1_20200823.000105.682_clear__sci_proc_im_000.fits');  % empty field       
    
    % break into sub images
    %[SI, InfoCCDSEC] = imProc.image.image2subimages(AI,[1024 1024],'OverlapXY',[64 64]);
    tic;
    [SI, InfoCCDSEC] = imProc.image.image2subimages(AI,[1600 1600],'OverlapXY',[64 64]);
    toc
    
    tic;
    imProc.background.background(SI, 'SubSizeXY',[128 128]);
    toc
    
    PSF_Sigma = [0.1; 1.0; 1.2; 2.5];
    tic;
    imProc.sources.findMeasureSources(SI, 'PsfFunPar', {PSF_Sigma}, 'RemoveBadSources',true);
    toc
    
    
    %[Result, Flag] = imProc.sources.classifySources(SI(16), 'SigmaPSF',PSF_Sigma);
    
    % need a step for flagging non-point sources (for astrometry)
    
    RA  = AI.HeaderData.Key.RA;
    Dec = AI.HeaderData.Key.DEC;
        
    %RA  = '01:21:39.560';
    %Dec = '+15:12:25.70';
    %RA  = celestial.coo.convertdms(RA,'SH','d');
    %Dec = celestial.coo.convertdms(Dec,'SD','d');
    
    Tran = Tran2D('poly3');
    
    JD = julday(AI);
    
    Flip = [1 1; 1 -1; -1 -1; -1 1];
    
    %RA =11.583;
    %Dec = -25.672;
    
    %[Result, NewSI32,AC] = imProc.astrometry.astrometryCore(SI16, 'Scale',1.25, 'RA',RA, 'Dec',Dec, 'CatColNamesMag','MAG_CONV_2', 'Tran',Tran, 'EpochOut',JD, 'Flip',Flip);
    
    tic;
    [Result, NewSI32,AC] = imProc.astrometry.astrometryCore(SI(16), 'Scale',1.25, 'RA',RA, 'Dec',Dec, 'CatColNamesMag','MAG_CONV_2', 'Tran',Tran, 'EpochOut',JD, 'Flip',Flip);
    toc
    
    [Result, NewSI32] = imProc.astrometry.astrometryRefine(NewSI32, 'Scale',1.25, 'RA',RA, 'Dec',Dec, 'CatColNamesMag','MAG_CONV_2', 'EpochOut',JD);

    tic;
    for I=1:1:54
    [Result, NewSI32] = imProc.astrometry.astrometryRefine(NewSI32, 'Scale',1.25, 'RA',RA, 'Dec',Dec, 'CatColNamesMag','MAG_CONV_2', 'Tran',Tran, 'EpochOut',JD,'CatName',AC);
    end
    toc
       
    % astrometrySubImages
    tic;
    [ResultFit, ResultObj, AstrometricCat] = imProc.astrometry.astrometrySubImages(SI, 'Scale',1.25,'CCDSEC', InfoCCDSEC.EdgesCCDSEC, 'RA',RA,'Dec',Dec, 'EpochOut',JD, 'Tran',Tran,'CreateNewObj',false);
    toc
        
    % astrometrySubImages w/AstrometricCat as input
    tic;
    [ResultFit, ResultObj] = imProc.astrometry.astrometrySubImages(SI, 'Scale',1.25,'CCDSEC', InfoCCDSEC.EdgesCCDSEC, 'RA',RA,'Dec',Dec, 'EpochOut',JD, 'Tran',Tran, 'CatName',AstrometricCat,'CreateNewObj',false);
    toc        
    
    cd(PWD);
    io.msgStyle(LogLevel.Test, '@passed', 'imProc.astrometry test passed')
    Result = true;
       
end
   