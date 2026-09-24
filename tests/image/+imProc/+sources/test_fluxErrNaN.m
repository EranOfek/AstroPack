function tests = test_fluxErrNaN
    % Issue #1135: non-positive aperture flux must yield NaN (never
    % negative) FLUXERR_APER and MAGERR_APER, in both the source finder
    % (imUtil.sources.find_measure_sources) and imProc.sources.forcedPhotNew.
    tests = functiontests(localfunctions);
end

%% Fixture

function setup(testCase)
    % Synthetic flat field + a few real sources; low detection threshold
    % makes the finder pick noise peaks whose annulus-subtracted aperture
    % flux is negative.
    rng(7);
    Back = 300;
    Im   = Back + sqrt(Back).*randn(1024, 1024);
    [XX,YY] = meshgrid(-7:7);
    G = exp(-(XX.^2+YY.^2)/(2*2^2));  G = G./sum(G(:));
    for S = 1:20
        X0 = randi([50 970]); Y0 = randi([50 970]);
        Im(Y0-7:Y0+7, X0-7:X0+7) = Im(Y0-7:Y0+7, X0-7:X0+7) + 3000.*G;
    end
    AI = AstroImage({Im});
    AI = imProc.background.backVar(AI);
    AI.PSFData.Data = G;    % populatePSF returns empty on this synthetic field

    % minimal TAN WCS - forcedPhotNew calls xy2sky unconditionally
    W = AstroWCS();
    W.ProjType  = 'TAN';  W.ProjClass = 'ZENITHAL';
    W.CooName   = {'RA','DEC'};  W.CTYPE = {'RA---TAN','DEC---TAN'};  W.CUNIT = {'deg','deg'};
    W.CD(1,1)   = 1.25/3600;  W.CD(2,2) = 1.25/3600;
    W.CRVAL     = [180 0];    W.CRPIX   = [512 512];
    W.populate_projMeta;  W.Success = true;
    AI.WCS = W;

    testCase.TestData.AI = AI;
end

%% Tests

function testFinderErrors(testCase)
    AI = imProc.sources.findMeasureSources(testCase.TestData.AI.copy, ...
             'Threshold',2, 'MagType','mag');
    C  = AI.CatData;
    F  = C.getCol({'FLUX_APER_1','FLUX_APER_2','FLUX_APER_3'});
    FE = C.getCol({'FLUXERR_APER_1','FLUXERR_APER_2','FLUXERR_APER_3'});
    ME = C.getCol({'MAGERR_APER_1','MAGERR_APER_2','MAGERR_APER_3'});
    verifyGreaterThan(testCase, nnz(F<=0), 0, 'test needs non-positive fluxes');
    verifyTrue(testCase, all(isnan(FE(F<=0))), 'FLUXERR not NaN for F<=0');
    verifyFalse(testCase, any(FE(:)<0),        'negative FLUXERR present');
    verifyTrue(testCase, all(isnan(ME(F<=0))), 'MAGERR not NaN for F<=0');
    verifyFalse(testCase, any(ME(:)<0),        'negative MAGERR present');
end

function testForcedPhotErrors(testCase)
    rng(11);
    Xf = 30 + 960.*rand(60,1);  Yf = 30 + 960.*rand(60,1);
    R = imProc.sources.forcedPhotNew(testCase.TestData.AI.copy, ...
            'Coo',[Xf Yf], 'CooUnits','pix', ...
            'Moving',false, 'AddRefStarsDist',0, 'PopulateWCS',false, ...
            'ReadColFromHeader',false, 'MagType','mag', 'OutputType','AstroCatalog', ...
            'ColCell',{'X','Y','FLAGS','BACK_ANNULUS','STD_ANNULUS', ...
                       'FLUX_APER_1','FLUX_APER_2','FLUX_APER_3', ...
                       'FLUXERR_APER_1','FLUXERR_APER_2','FLUXERR_APER_3', ...
                       'MAG_APER_1','MAG_APER_2','MAG_APER_3', ...
                       'MAGERR_APER_1','MAGERR_APER_2','MAGERR_APER_3'});
    F  = R.getCol({'FLUX_APER_1','FLUX_APER_2','FLUX_APER_3'});
    FE = R.getCol({'FLUXERR_APER_1','FLUXERR_APER_2','FLUXERR_APER_3'});
    ME = R.getCol({'MAGERR_APER_1','MAGERR_APER_2','MAGERR_APER_3'});
    verifyGreaterThan(testCase, nnz(F<=0), 0, 'test needs non-positive fluxes');
    verifyTrue(testCase, all(isnan(FE(F<=0))), 'FLUXERR not NaN for F<=0');
    verifyFalse(testCase, any(FE(:)<0),        'negative FLUXERR present');
    verifyTrue(testCase, all(isnan(ME(F<=0))), 'MAGERR not NaN for F<=0');
    verifyFalse(testCase, any(ME(:)<0),        'negative MAGERR present');
end
