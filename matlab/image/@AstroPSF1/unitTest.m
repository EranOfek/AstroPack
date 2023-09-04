function Result = unitTest()
    % unitTest for AstroPSF
    % Example: Result = AstroPSF.unitTest

    io.msgStyle(LogLevel.Test, '@start', 'AstroPSF test started');                                      

    % getPSF
    AP = AstroPSF1;
    P = imUtil.kernel2.gauss;
    AP.DataPSF = P;
    if ~all(AP.getPSF==P)
        error('Problem with set/get PSF');
    end

    % curve of growth
    AP = AstroPSF1;
    AP.DataPSF = imUtil.kernel2.gauss;

    % @FIX - @Eran
    [Result, RadHalfCumSum, RadHalfPeak] = curve_of_growth(AP);

    % images2cube
    AP = AstroPSF1;
    AP.DataPSF = imUtil.kernel2.gauss;
    AP(2).DataPSF = imUtil.kernel2.gauss;
    [Cube, CubeVar] = images2cube(AP)

    % moments
    AP = AstroPSF1;
    AP.DataPSF = imUtil.kernel2.gauss;
    AP(2).DataPSF = imUtil.kernel2.gauss;
    [M1,M2,Aper] = moment2(AP,'moment2Args',{'Momradius',4,'Annulus',[3, 4]});

    % fwhm
    imUtil.psf.pseudoFWHM(AP(1).Data)
    imUtil.psf.containment(AP(1).Data,'Level',0.9)
    
%     [FWHM_CumSum, FWHM_Flux] = fwhm(AP(1));
    [FWHM_CumSum, FWHM_Flux] = fwhm(AP);    
    
    % extended data structures 
    
    AP = AstroPSF1;
    P0 = imUtil.kernel2.gauss([2 2 0],[7 7]);
    AP.StampSize = size(P0);
    
    X = 1:AP.StampSize(1); Y = 1:AP.StampSize(2);
    
    T1 = [0.2  0.3 0;  -0.1 0.9 0; 0 0 1]';  % the last column shall read: 0 0 1
    T2 = [-0.5 0.4 0;   1 0.5   0; 0 0 1]';
    P1 = imwarp(P0, affine2d(T1));
    P2 = imwarp(P0, affine2d(T2));
    
    P1 = imresize(P1,AP.StampSize);
    P2 = imresize(P2,AP.StampSize);
    
    Lam = [2000 4000 6000]; % 3 wavelengths for 3 PSFs 
    Rad = [0 2 4];          % 3 radial positions
    
    AP.DimAxes{1} = Lam;
    AP.DimAxes{2} = Rad;
    
    AP.DataPSF = zeros( AP.StampSize(1), AP.StampSize(2), ...
        max(size(AP.DimAxes{1},2),1), max(size(AP.DimAxes{2},2),1), ...
        max(size(AP.DimAxes{3},2),1), max(size(AP.DimAxes{4},2),1), ...
        max(size(AP.DimAxes{5},2),1) );
    
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
    PSF_db = sprintf('%s%s%g%s',tools.os.getAstroPackPath,'/../data/ULTRASAT/PSF/ULTRASATlabPSF5.mat');
    ReadDB = struct2cell ( io.files.load1(PSF_db) ); % PSF data at the chosen spatial resolution
    PSFdata = ReadDB{2}; 
    
    AP.DataPSF = PSFdata;
    AP.DimAxes{1} = 2000:100:11000;
    AP.DimAxes{2} = linspace(0,10,25);
    AP.StampSize = size(AP.DataPSF,[1 2]);
    
    Pg1 = AP.getPSF;
    Pg2 = AP.getPSF('PsfArgs',{'Wave',2210});
    Pg3 = AP.getPSF('PsfArgs',{'Wave',3550,'PosX',5.5});
    Pg3 = AP.getPSF('PsfArgs',{'Wave',3550,'PosX',5.5},'InterpMethod','linear');
    
    Pg4 = AP.getPSF('PsfArgs',{'Wave',[2210 3400 6800]}); % get a cube of PSFs at different wavelengths
    
    Pw1 = AP.weightPSF;
    Pw2 = AP.weightPSF('Pos',{'PosX',2});
    Pw3 = AP.weightPSF('Pos',{'PosX',6},'Wave',[2000 3000 4000 5000],'Spec',[0.5 1 1 0.3]);
    
    Sp = AstroSpec.blackBody(2000:11000,3500);
    Pw4 = AP.weightPSF('Pos',{'PosX',6},'Wave',Sp.Wave,'Spec',Sp.Flux');
    
    pause

    % 
    
% %     AP.DimAxis2{1} = Lam; 
% %     AP.DimAxis2{6} = Rad;           
% %     
% %     AP.DataPSF2 = zeros( AP.StampSize(1), AP.StampSize(2), ...
% %                          max(size(AP.DimAxis2{1},2),1), max(size(AP.DimAxis2{2},2),1), ...
% %                          max(size(AP.DimAxis2{3},2),1), max(size(AP.DimAxis2{4},2),1), ...
% %                          max(size(AP.DimAxis2{5},2),1), max(size(AP.DimAxis2{6},2),1), ...
% %                          max(size(AP.DimAxis2{7},2),1), max(size(AP.DimAxis2{8},2),1), ...
% %                          max(size(AP.DimAxis2{9},2),1), max(size(AP.DimAxis2{10},2),1) );
% %     
% %     % put 3 PSFs at 3 radial postions and interpolate: 
% %                      
% %     AP.DataPSF2(:,:,1,1) = P0;
% %     AP.DataPSF2(:,:,1,2) = P1;
% %     AP.DataPSF2(:,:,1,3) = P2;
% %     
% %     Radius = 3.0; lab1 = sprintf('%s%d','spatial interpolation at r=', Radius);
% %     
% %     SubArray = squeeze( AP.DataPSF2 );
% %     PSF = interpn(X,Y,Lam,Rad,SubArray,X,Y,Lam,Radius);
% %     
% %     figure(1)
% %     subplot(2,2,1); imagesc(AP.DataPSF2(:,:,1,1)); title '1';
% %     subplot(2,2,2); imagesc(AP.DataPSF2(:,:,1,2)); title '2';
% %     subplot(2,2,3); imagesc(AP.DataPSF2(:,:,1,3)); title '3';
% %     subplot(2,2,4); imagesc(PSF(:,:,1)); title(lab1);     
% %     
% %     % put 3 PSFs at 3 frequencies and make a spectrum-weighted PSF:
% %     
% %     AP.DataPSF2(:,:,1,1) = P0;
% %     AP.DataPSF2(:,:,2,1) = P1;
% %     AP.DataPSF2(:,:,3,1) = P2;
% %     
% %     PSFdata = AP.DataPSF2(:,:,:,:);
% %     RadSrc  = Rad(1);
% %     Temp = 3000; lab2 = sprintf('%s%d','spec. weighting, T=' , Temp);
% %     SpecSrc(1,:) = AstroSpec.blackBody(Lam',Temp).Flux;
% %     
% %     PSF2 = imUtil.psf.specWeight( SpecSrc, RadSrc, PSFdata, 'Rad', Rad );
% % 
% %     figure(2)
% %     subplot(2,2,1); imagesc(AP.DataPSF2(:,:,1,1)); title '1';
% %     subplot(2,2,2); imagesc(AP.DataPSF2(:,:,1,2)); title '2';
% %     subplot(2,2,3); imagesc(AP.DataPSF2(:,:,1,3)); title '3';
% %     subplot(2,2,4); imagesc(PSF2(:,:,1)); title(lab2);
% %     
% %     % calculate pseudo FWHM and containtemnt radius
% %     
% %     % the function acts only on the first 2 dimentions of the cube, 
% %     % i.e. takes AP.DataPSF2(:,:,1,1,1,1,1,1) or AP.DataPSF2(:,:,1,1)
% %     % for AP.DataPSF2(:,:,1,2) or AP.DataPSF2(:,:,1,3) the result will be different! 
% %     
% %     AP.FWHM = imUtil.psf.pseudoFWHM(AP.DataPSF2);
% %     AP.ContainmentR = imUtil.psf.containment(AP.DataPSF2(:,:,1,1),'Level',0.99);
% %     
%     % extended data structures (temporarily in DataPSF3, later will be moved to DataPSF)
%     
%     AP.DimAxis3{1} = Lam; 
%     AP.DimAxis3{2} = Rad;           
%     
%     AP.DataPSF3 = zeros( AP.StampSize(1), AP.StampSize(2), ...
%                          max(size(AP.DimAxis3{1},2),1), max(size(AP.DimAxis3{2},2),1), ...
%                          max(size(AP.DimAxis3{3},2),1), max(size(AP.DimAxis3{4},2),1), ...
%                          max(size(AP.DimAxis3{5},2),1) );
%     
%     % put 3 PSFs at 3 radial postions and interpolate: 
%                      
%     AP.DataPSF3(:,:,1,1) = P0;
%     AP.DataPSF3(:,:,1,2) = P1;
%     AP.DataPSF3(:,:,1,3) = P2;
%     
%     Radius = 3.0; lab1 = sprintf('%s%d','spatial interpolation at r=', Radius);
%     
%     SubArray = squeeze( AP.DataPSF3 );
%     PSF = interpn(X,Y,Lam,Rad,SubArray,X,Y,Lam,Radius);
%     
%     figure(3)
%     subplot(2,2,1); imagesc(AP.DataPSF3(:,:,1,1)); title '1';
%     subplot(2,2,2); imagesc(AP.DataPSF3(:,:,1,2)); title '2';
%     subplot(2,2,3); imagesc(AP.DataPSF3(:,:,1,3)); title '3';
%     subplot(2,2,4); imagesc(PSF(:,:,1)); title(lab1);     
%     
%     % put 3 PSFs at 3 frequencies and make a spectrum-weighted PSF:
%     
%     AP.DataPSF3(:,:,1,1) = P0;
%     AP.DataPSF3(:,:,2,1) = P1;
%     AP.DataPSF3(:,:,3,1) = P2;
%     
%     PSFdata = AP.DataPSF3(:,:,:,:);
%     RadSrc  = Rad(1);
%     Temp = 3000; lab2 = sprintf('%s%d','spec. weighting, T=' , Temp);
%     SpecSrc(1,:) = AstroSpec.blackBody(Lam',Temp).Flux;
%     
%     PSF2 = imUtil.psf.specWeight( SpecSrc, RadSrc, PSFdata, 'Rad', Rad );
% 
%     figure(4)
%     subplot(2,2,1); imagesc(AP.DataPSF3(:,:,1,1)); title '1';
%     subplot(2,2,2); imagesc(AP.DataPSF3(:,:,1,2)); title '2';
%     subplot(2,2,3); imagesc(AP.DataPSF3(:,:,1,3)); title '3';
%     subplot(2,2,4); imagesc(PSF2(:,:,1)); title(lab2);
    
    % calculate pseudo FWHM and containtemnt radius
    
    % the function acts only on the first 2 dimentions of the cube, 
    % i.e. takes AP.DataPSF2(:,:,1,1,1,1,1,1) or AP.DataPSF2(:,:,1,1)
    % for AP.DataPSF2(:,:,1,2) or AP.DataPSF2(:,:,1,3) the result will be different! 
    
    AP.FWHM = imUtil.psf.pseudoFWHM(AP.DataPSF3);
    AP.ContainmentR = imUtil.psf.containment(AP.DataPSF3(:,:,1,1),'Level',0.99);
    
    %
    io.msgStyle(LogLevel.Test, '@passed', 'AstroPSF test passed');                          
    Result = true;
end
