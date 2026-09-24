
function Result = unitTest()
    % unitTest for AstroPSF
    % Author: Eran Ofek, A.M. Krassilchtchikov (Oct 2023)
    % Example: Result = AstroPSF.unitTest

    %io.msgStyle(LogLevel.Test, '@start', 'AstroPSF test started');       
    
    % synthetic kernels:
    
    AP = AstroPSF('Synthetic','gauss','GaussSigma',[3 4 0],'StampSize',[15 19]);    
    AP = AstroPSF('Synthetic','lorentzian','LorentzianGamma',2);
    AP = AstroPSF('Synthetic','cosbell','CosbellRadii',[6 10],'StampSize',21);
    AP = AstroPSF('Synthetic','moffat','MoffatAlphaBeta',[0.7 3]);
    
    % getPSF
    AP = AstroPSF;
    P = imUtil.kernel2.gauss;
    AP.DataPSF = P;
    if ~all(AP.getPSF('ReNorm',0)==P)
        error('Problem with set/get PSF');
    end
    
    % stamp padding 
    P = AP.getPSF('StampSize',[21 21]);
    P = AP.getPSF('StampSize',[21 21],'fftshift','fftshift');

    % curve of growth and radial profile
    AP = AstroPSF;
    AP.DataPSF = imUtil.kernel2.gauss;
    [R, V] = AP.radialProfile

    % @FIX - @Eran
    [Result, RadHalfCumSum, RadHalfPeak] = curve_of_growth(AP);

    % images2cube
    AP = AstroPSF;
    AP.DataPSF = imUtil.kernel2.gauss;
    AP(2).DataPSF = imUtil.kernel2.gauss;
    [Cube, CubeVar] = images2cube(AP)

    % moments
    AP = AstroPSF;
    AP.DataPSF = imUtil.kernel2.gauss;
    AP(2).DataPSF = imUtil.kernel2.gauss;
    [M1,M2,Aper] = moment2(AP,'moment2Args',{'Momradius',4,'Annulus',[3, 4]});

    % fwhm
    imUtil.psf.pseudoFWHM(AP(1).Data)
    imUtil.psf.quantileRadius(AP(1).Data,'Level',0.9)
    
%     [FWHM_CumSum, FWHM_Flux] = fwhm(AP(1));
    [FWHM_CumSum, FWHM_Flux] = fwhm(AP);    
    
    % Newly developed properties:
    
    cprintf('blue','%s\n', 'Test some new multi-D properties:');    
    
    AP = AstroPSF;
    P0 = imUtil.kernel2.gauss([2 2 0],[7 7]);
    AP.StampSize = size(P0);
    
%     X = 1:AP.StampSize(1); Y = 1:AP.StampSize(2);
    
    T1 = [0.2  0.3 0;  -0.1 0.9 0; 0 0 1]';  % the last column shall read: 0 0 1
    T2 = [-0.5 0.4 0;   1 0.5   0; 0 0 1]';
    P1 = imwarp(P0, affine2d(T1));
    P2 = imwarp(P0, affine2d(T2));
    
    P1 = imresize(P1,AP.StampSize);
    P2 = imresize(P2,AP.StampSize);
    
    Lam = [2000 4000 6000]; % 3 wavelengths for 3 PSFs 
    Rad = [0 2 4];          % 3 radial positions
    
    AP.DimVals{1} = Lam;
    AP.DimVals{2} = Rad;
    
    AP.DataPSF = zeros( AP.StampSize(1), AP.StampSize(2), ...
        max(size(AP.DimVals{1},2),1), max(size(AP.DimVals{2},2),1), ...
        max(size(AP.DimVals{3},2),1), max(size(AP.DimVals{4},2),1), ...
        max(size(AP.DimVals{5},2),1) );
    
    % put 3 PSFs at 3 radial postions:
    
    AP.DataPSF(:,:,1,1) = P0;
    AP.DataPSF(:,:,1,2) = P1;
    AP.DataPSF(:,:,1,3) = P2;
    
    % put 3 PSFs at 3 frequencies:
    
    AP.DataPSF(:,:,1,1) = P0;
    AP.DataPSF(:,:,2,1) = P1;
    AP.DataPSF(:,:,3,1) = P2;
    
    Ps1 = AP.getPSF;
    Ps2 = AP.getPSF('PsfArgs',{'Wave',2200});
    Ps3 = AP.getPSF('PsfArgs',{'PosX',2.5});
    Ps4 = AP.getPSF('PsfArgs',{'Wave',2200,'PosX',2.5});
    Ps5 = AP.getPSF('PsfArgs',{'Wave',2200,'PosX',2.5},'InterpMethod','linear');
    Ps6 = AP.getPSF('PsfArgs',{'Wave',5000});
    Ps7 = AP.getPSF('PsfArgs',{'Wave',5000},'InterpMethod','linear');
    
% [PSF, Var] = P.weightPSF('Flux',Val, 'Wave',[5000 5500 6000],'Spec',[0.5 1 0.5])
% AstroPSF = P.repopPSF('Wave',[5000 5500 6000],'WaveWeight',[0.5 1 0.5])
% ValPerPSF = fwhm(P)
% [ValX, ValY] = P.fwhm(Method=[], 'Flux',Val, 'Wave',[5000])

    % load the ULTRASAT PSFs as AP.DataPSF
    I = Installer;
%     PSF_db = sprintf('%s%s%g%s',tools.os.getAstroPackPath,'/../data/ULTRASAT/PSF/ULTRASATlabPSF5.mat');
    PSF_db = sprintf('%s%s%g%s',I.getDataDir('ULTRASAT_PSF'),'/ULTRASATlabPSF5.mat');
%     ReadDB = struct2cell ( io.files.load1(PSF_db) ); % PSF data at the chosen spatial resolution
    ReadDB = struct2cell ( io.files.load_check(PSF_db) ); % PSF data at the chosen spatial resolution
    PSFdata = ReadDB{2}; 
    
    AP.DataPSF = PSFdata;
    AP.DimVals{1} = 2000:100:11000;
    AP.DimVals{2} = linspace(0,10,25);
%     AP.StampSize = size(AP.DataPSF,[1 2]);
    
    Pg1 = AP.getPSF;
    Pg2 = AP.getPSF('PsfArgs',{'Wave',2210});
    Pg3 = AP.getPSF('PsfArgs',{'Wave',3550,'PosX',5.5});
    Pg3 = AP.getPSF('PsfArgs',{'Wave',3550,'PosX',5.5},'InterpMethod','linear');
    
    Pg4 = AP.getPSF('PsfArgs',{'Wave',[2210 3400 6800]}); % get a cube of PSFs at different wavelengths
    
    Pg5 = AP.getPSF('Oversampling',5,'PsfArgs',{'Wave',2500,'PosX',7}); % rescale the output stamp 
    
    Pw1 = AP.specWeightedPSF;
    Pw2 = AP.specWeightedPSF('Pos',{'PosX',2});
    Pw3 = AP.specWeightedPSF('Pos',{'PosX',6},'Wave',[2000 3000 4000 5000],'Spec',[0.5 1 1 0.3]);
    
    Sp = AstroSpec.blackBody(2000:11000,3500);
    Pw4 = AP.specWeightedPSF('Pos',{'PosX',6},'Wave',Sp.Wave,'Spec',Sp.Flux');
    
    % multiple interpolation methods:
    Pg6 = AP.getPSF('PsfArgs',{'Wave',3550,'PosX',5.5},'InterpMethod',{'linear','nearest'});
    Pg7 = AP.getPSF('PsfArgs',{'Wave',3550,'PosX',5.5},'InterpMethod','linear');
    Pg8 = AP.getPSF('PsfArgs',{'Wave',3550,'PosX',5.5},'InterpMethod','nearest');
    sum((Pg6-Pg7)^2,'all')
    sum((Pg6-Pg8)^2,'all')
    
    % PSF fitting:
    fit = AP.fitFunPSF('CreateNewObj',true,'PsfArgs',{'Wave',3550,'PosX',5.5,'InterpMethod','linear'});
    
    % full field to stamp:
    stamp = AP.full2stamp('CreateNewObj',true,'PsfArgs',{'Wave',3550,'PosX',5.5,'InterpMethod','linear'});
    
    % curve of growth:
    cgw1 = AP.curve_of_growth;
    cgw2 = AP.curve_of_growth('PsfArgs',{'Wave',3200});
    cgw3 = AP.curve_of_growth('PsfArgs',{'Wave',3550,'PosX',5.5,'InterpMethod','linear'});
    
    % surface plotting
    AP.surface;
    AP.surface('PsfArgs',{'Wave',3550,'PosX',5.5,'InterpMethod','linear'});
    
    % suppress edges
    AP.suppressEdges;
    
    % normalize the stamps
    AP.normPSF;
    
    % images to cube:
    Cube  = AP.images2cube;
    Cube2 = AP.images2cube('PsfArgs',{'Wave',5300});
    
    % fwhm:
    FWHM  = AP.fwhm
    FWHM2 = AP.fwhm('PsfArgs',{'PosX',4.4})
    
    % moment2:
    M2  = AP.moment2
    M21 = AP.moment2('PsfArgs',{'Wave',5300});
    
    % populate some properties:
    AP.FWHM = imUtil.psf.pseudoFWHM(AP.getPSF);
    AP.FluxContainmentRadius = imUtil.psf.quantileRadius(AP.getPSF,'Level',0.99);
    AP
    
    % plot radial profile:  
    AP2(1) = AstroPSF; AP2(1).DataPSF = Pg2; 
    AP2(2) = AstroPSF; AP2(2).DataPSF = Pg3;
    AP2.plotRadialProfile;
    
    %io.msgStyle(LogLevel.Test, '@passed', 'AstroPSF test passed');                          
    Result = true;
end
