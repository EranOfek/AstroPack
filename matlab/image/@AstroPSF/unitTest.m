function Result = unitTest()
    % unitTest for AstroPSF
    % Example: Result = AstroPSF.unitTest

    io.msgStyle(LogLevel.Test, '@start', 'AstroPSF test started');                                      

    % getPSF
    AP = AstroPSF;
    P = imUtil.kernel2.gauss;
    AP.DataPSF = P;
    if ~all(AP.getPSF==P)
        error('Problem with set/get PSF');
    end

    % curve of growth
    AP = AstroPSF;
    AP.DataPSF = imUtil.kernel2.gauss;

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
    imUtil.psf.containment(AP(1).Data,'Level',0.9)
    
%     [FWHM_CumSum, FWHM_Flux] = fwhm(AP(1));
    [FWHM_CumSum, FWHM_Flux] = fwhm(AP);    
    
    % extended data structures (currently in DataPSF2)
    
    AP = AstroPSF;
    P0 = imUtil.kernel2.gauss;
    AP.StampSize = size(P0);
    
    T1 = [1.2  0.3 0;  -0.1 0.9 0; 0 0 1]';  % the last column shall read: 0 0 1
    T2 = [-0.5 0.4 0;   1 0.5   0; 0 0 1]';
    P1 = imwarp(P0, affine2d(T1));
    P2 = imwarp(P0, affine2d(T2));
    
    
    
    %
    io.msgStyle(LogLevel.Test, '@passed', 'AstroPSF test passed');                          
    Result = true;
end
