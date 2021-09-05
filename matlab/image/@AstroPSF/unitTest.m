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
    [M1,M2,Aper] = moment2(AP);

    % fwhm
    [FWHM_CumSum, FWHM_Flux] = fwhm(AP);

    io.msgStyle(LogLevel.Test, '@passed', 'AstroPSF test passed');                          
    Result = true;
end
