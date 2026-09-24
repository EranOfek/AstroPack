function tests = test_magTypeInstrumental
    % Unit tests for the instrumental flux->magnitude convention (issue #1161).
    %
    % Covers the 'MagType' option added to the source-extraction chain, the
    % PhotCalibTrans.nanFillMagCols helper used for crops whose photometric
    % calibration did not run, and the MatchedSources MagType stamp that
    % records which convention a saved product holds.
    %
    % The point of the change is that a non-positive flux must produce NaN
    % under MagType='mag' instead of the finite-but-meaningless asinh value a
    % luptitude returns; the tests below assert exactly that at each level.

    tests = functiontests(localfunctions);
end

%% convert-level dispatch

function testMagnitudeIsNaNForNonPositiveFlux(testCase)
    % convert.magnitude returns NaN for flux <= 0, convert.luptitude does not.

    Flux  = [1000; 10; 0; -5];
    Flux0 = 10.^(0.4.*25);

    Lup = convert.luptitude(Flux, Flux0);
    Mag = convert.magnitude(Flux, Flux0);

    testCase.verifyTrue(all(isfinite(Lup)), ...
        'convert.luptitude should stay finite for non-positive flux.');
    testCase.verifyTrue(all(isnan(Mag(3:4))), ...
        'convert.magnitude should be NaN for zero/negative flux.');
    % The two conventions converge at the bright end and diverge as the flux
    % approaches the softening scale - which is the whole subject of #1161.
    testCase.verifyEqual(Mag(1), Lup(1), 'AbsTol', 1e-5, ...
        'At 1000 counts luptitude and magnitude must agree to <0.01 mmag.');
    testCase.verifyEqual(Mag(2) - Lup(2), 0.0107, 'AbsTol', 5e-4, ...
        'At 10 counts the luptitude is ~11 mmag brighter than the magnitude.');
end

%% Extraction chain

function testFindMeasureSourcesMagType(testCase)
    % Forced measurements on blank sky give negative aperture fluxes; those
    % must be finite under 'lup' and NaN under 'mag'.

    rng(7);
    Im   = randn(256,256).*3 + 100;
    Im(60,60) = 3000;
    ColCell   = {'XPEAK','YPEAK','FLUX_APER','MAG_APER','FLUXERR_APER','MAGERR_APER'};
    ForcedXY  = [(30:10:220).', (30:10:220).'];
    % Background deliberately set above the image level so that the forced
    % aperture fluxes come out negative.
    CommonArgs = {'ColCell',ColCell, 'Threshold',5, 'OutType','AstroCatalog', ...
                  'ForcedList',ForcedXY, 'OnlyForced',true, ...
                  'BackIm',ones(256,256).*110, 'VarIm',ones(256,256).*9};

    CatLup = imUtil.sources.find_measure_sources(Im, CommonArgs{:}, 'MagType','lup');
    CatMag = imUtil.sources.find_measure_sources(Im, CommonArgs{:}, 'MagType','mag');

    Flux = CatLup.getCol('FLUX_APER_1');
    Neg  = ~(Flux>0);

    MagLup = CatLup.getCol('MAG_APER_1');
    MagMag = CatMag.getCol('MAG_APER_1');

    testCase.assumeTrue(any(Neg), ...
        'Test fixture failed to produce any non-positive aperture flux.');
    testCase.verifyTrue(all(isfinite(MagLup(Neg))), ...
        'MagType=''lup'' should give finite MAG_APER for non-positive flux.');
    testCase.verifyTrue(all(isnan(MagMag(Neg))), ...
        'MagType=''mag'' should give NaN MAG_APER for non-positive flux.');
    testCase.verifyEqual(CatLup.getCol('FLUX_APER_1'), CatMag.getCol('FLUX_APER_1'), ...
        'MagType must not change the measured fluxes.');
end

function testPsfPhotCubeMagType(testCase)
    % Same contract for the PSF photometry that produces MAG_PSF.

    PSF   = imUtil.kernel2.gauss(1.5.*ones(4,1));
    Cube  = imUtil.trans.shift_fft(PSF, [0.4;0.7;-1.1;0.6], [0.7;-0.2;-0.9;-0.6]);
    Cube  = Cube.*permute([100 110 -50 300],[1 3 2]) + randn(15,15);

    ResLup = imUtil.sources.psfPhotCube(Cube, 'PSF',PSF(:,:,1), 'MagType','lup');
    ResMag = imUtil.sources.psfPhotCube(Cube, 'PSF',PSF(:,:,1), 'MagType','mag');

    Neg = ~(ResLup.Flux>0);
    testCase.assumeTrue(any(Neg), 'Fixture produced no negative PSF flux.');
    testCase.verifyTrue(all(isfinite(ResLup.Mag(Neg))));
    testCase.verifyTrue(all(isnan(ResMag.Mag(Neg))));
    testCase.verifyEqual(ResLup.Flux, ResMag.Flux, ...
        'MagType must not change the fitted fluxes.');
end

function testMagTypeRejectsInvalidValue(testCase)
    % The option is validated by mustBeMember at every level.

    testCase.verifyError( ...
        @() imUtil.sources.find_measure_sources(rand(64,64), 'MagType','xxx'), ...
        ?MException);
end

%% MAGERR follows MAG

function testMagErrIsNaNWhereMagIsNaN(testCase)
    % A magnitude error must not survive where the magnitude itself is NaN -
    % the error columns are divided by the flux, so under MagType='mag' they
    % would otherwise be finite-and-negative next to a NaN magnitude.

    rng(7);
    Im = randn(256,256).*3 + 100;
    Im(60,60) = 3000;
    ColCell  = {'XPEAK','YPEAK','FLUX_APER','MAG_APER','FLUXERR_APER','MAGERR_APER'};
    ForcedXY = [(30:10:220).', (30:10:220).'];
    CommonArgs = {'ColCell',ColCell, 'Threshold',5, 'OutType','AstroCatalog', ...
                  'ForcedList',ForcedXY, 'OnlyForced',true, ...
                  'BackIm',ones(256,256).*110, 'VarIm',ones(256,256).*9};

    CatLup = imUtil.sources.find_measure_sources(Im, CommonArgs{:}, 'MagType','lup');
    CatMag = imUtil.sources.find_measure_sources(Im, CommonArgs{:}, 'MagType','mag');

    Flux   = CatLup.getCol('FLUX_APER_1');
    Neg    = ~(Flux>0);
    MagMag = CatMag.getCol('MAG_APER_1');
    ErrMag = CatMag.getCol('MAGERR_APER_1');
    ErrLup = CatLup.getCol('MAGERR_APER_1');

    testCase.assumeTrue(any(Neg), 'Fixture produced no non-positive aperture flux.');
    testCase.verifyEqual(isnan(MagMag), isnan(ErrMag), ...
        'MAGERR must be NaN exactly where MAG is NaN.');
    testCase.verifyFalse(any(ErrMag<0), ...
        'MagType=''mag'' must not leave a negative magnitude error.');
    testCase.verifyEqual(ErrLup(~Neg), ErrMag(~Neg), ...
        'Where the flux is positive the two conventions give the same MAGERR.');
end

%% NaN-fill of uncalibrated magnitude columns

function testNanFillMagCols(testCase)
    % Every MAG_*/MAGERR_* column is replaced in place by NaN; fluxes are
    % untouched and FLUX_XYPEAK does not spawn a magnitude column.

    Nrow = 5;
    Cat  = AstroCatalog({[ (1:Nrow).', (1:Nrow).', rand(Nrow,1).*100, rand(Nrow,1), ...
                           rand(Nrow,1).*10, rand(Nrow,1).*50, rand(Nrow,1).*1000 ]}, ...
                        'ColNames',{'X','Y','FLUX_APER_1','FLUXERR_APER_1', ...
                                    'MAG_APER_1','FLUX_XYPEAK','FLUX_PSF'});
    FluxBefore = Cat.getCol('FLUX_APER_1');
    NcolBefore = numel(Cat.ColNames);

    Cat = PhotCalibTrans.nanFillMagCols(Cat);

    testCase.verifyTrue(all(isnan(Cat.getCol('MAG_APER_1'))), ...
        'MAG_APER_1 should be NaN-filled.');
    testCase.verifyTrue(any(strcmp(Cat.ColNames,'MAGERR_APER_1')), ...
        'MAGERR_APER_1 should be created (FLUXERR_APER_1 exists).');
    testCase.verifyFalse(any(strcmp(Cat.ColNames,'MAG_XYPEAK')), ...
        'FLUX_XYPEAK is a pixel peak value and must be skipped.');
    testCase.verifyEqual(Cat.getCol('FLUX_APER_1'), FluxBefore, ...
        'Fluxes must not be modified.');
    testCase.verifyEqual(numel(Cat.ColNames), NcolBefore + 2, ...
        'MAG_APER_1 is replaced in place; only MAGERR_APER_1 and MAG_PSF are new.');
end

function testNanFillMagColsEmptyCatalog(testCase)
    % An empty catalog is a no-op, not an error (crops with no sources).

    Cat = AstroCatalog;
    testCase.verifyWarningFree(@() PhotCalibTrans.nanFillMagCols(Cat));
end

%% RedoUpIter bright-source branch (aperPhotCube positional-argument fix)

function testRedoUpIterUsesRequestedAperRadii(testCase)
    % imUtil.sources.aperPhotCube takes (Cube, X, Y, Args). The RedoUpIter
    % branch used to omit X and Y, so 'AperRad' was swallowed as X and the
    % radius vector as Y, leaving AperRad at its own default [2,4,5]: wrong
    % radii, a garbage sub-pixel shift, and a column-count mismatch that threw
    % whenever numel(AperRadius) ~= 3.

    rng(11);
    N    = 256;
    Im   = randn(N,N).*sqrt(200) + 200;
    Nsrc = 40;
    Xt   = 30 + rand(Nsrc,1).*(N-60);
    Yt   = 30 + rand(Nsrc,1).*(N-60);
    Ft   = 10.^(2.5 + rand(Nsrc,1).*2);
    [XX,YY] = meshgrid(1:N,1:N);
    for I = 1:Nsrc
        Im = Im + Ft(I)./(2.*pi.*1.7.^2).*exp(-((XX-Xt(I)).^2+(YY-Yt(I)).^2)./(2.*1.7.^2));
    end

    ColCell = {'XPEAK','YPEAK','X1','Y1','SN','BACK_IM','VAR_IM', ...
               'FLUX_APER','FLUXERR_APER','MAG_APER','MAGERR_APER','FLUX_XYPEAK'};
    AperRad = [3 5 6 7];   % four radii - the count that used to throw

    Result = imProc.sources.multiIterExtractor(AstroImage({Im}), ...
                'Threshold',[100 20 4], 'ColCell',ColCell, ...
                'AperRadius',AperRad, 'RedoUpIter',1);

    Cols     = Result.CatData.ColNames;
    FluxCols = Cols(startsWith(Cols,'FLUX_APER'));
    testCase.verifyEqual(numel(FluxCols), numel(AperRad), ...
        'One FLUX_APER column per requested aperture radius.');

    % The radii are really used: a larger aperture must collect more flux.
    F1 = Result.CatData.getCol('FLUX_APER_1');
    F4 = Result.CatData.getCol('FLUX_APER_4');
    Good = isfinite(F1) & isfinite(F4) & F1>0;
    testCase.assumeTrue(any(Good), 'No usable sources in the fixture.');
    testCase.verifyGreaterThan(median(F4(Good)./F1(Good)), 1, ...
        'Flux must grow with aperture radius.');
end

%% Blank PT_* keywords on an uncalibrated image

function testUncalibratedHeaderCarriesBlankPTKeys(testCase)
    % An uncalibrated (empty) PhotCalibTrans still writes the PT_* key set, so
    % a consumer can tell "calibration did not run" from "no sources found".
    % The fit-result keys carry NaN, which the mex writers serialize as blank
    % (FITS undefined) cards and getVal reads back as NaN.

    Header = AstroHeader;
    PC     = PhotCalibTrans;            % empty TransModel = uncalibrated

    Header = PC.photCalibTransToHeader(Header);

    PTKeys = Header.Data(startsWith(Header.Data(:,1),'PT_'), 1);
    testCase.verifyNotEmpty(PTKeys, 'PT_* keywords should be written.');
    testCase.verifyTrue(any(strcmp(PTKeys,'PT_ZP')), ...
        'PT_ZP is a core calibration result and must always be present.');

    % Every fit-result key is NaN/undefined; the remaining PT_* keys are
    % configuration constants (reference model, spectra source) and stay set.
    for Key = {'PT_RMS','PT_CHI2','PT_DOF','PT_NCALIB','PT_ZP'}
        testCase.verifyTrue(isnan(Header.getVal(Key{1})), ...
            sprintf('%s should be undefined for an uncalibrated image.', Key{1}));
    end
end

%% MatchedSources provenance stamp

function testMatchedSourcesMagTypeStampRoundTrip(testCase)
    % The stamp survives a write1/read round trip as an HDF5 root attribute,
    % without being loaded into Data (which would corrupt Nsrc/Nepoch).

    FileName = [tempname '.hdf5'];
    Cleanup  = onCleanup(@() delete(FileName)); %#ok<NASGU>

    MS = MatchedSources;
    MS.addMatrix({rand(20,100), rand(20,100)}, {'MAG_APER_3','FLUX_APER_3'});
    MS.JD      = (1:20).';
    MS.MagType = 'mag';
    MS.write1(FileName);

    MSread = MatchedSources.read(FileName);

    testCase.verifyEqual(MSread.MagType, 'mag');
    testCase.verifyEqual(MSread.Nsrc, 100, 'The stamp must not land in Data.');
    testCase.verifyEqual(MSread.Nepoch, 20);
    testCase.verifyEqual(sort(MSread.Fields), {'FLUX_APER_3'; 'MAG_APER_3'});
end

function testMatchedSourcesUnstampedFileStillReads(testCase)
    % Products written before the attribute existed must still read cleanly.

    FileName = [tempname '.hdf5'];
    Cleanup  = onCleanup(@() delete(FileName)); %#ok<NASGU>

    MS = MatchedSources;
    MS.addMatrix(rand(20,100), 'MAG_APER_3');
    MS.JD = (1:20).';
    MS.write1(FileName);   % MagType is '' -> no attribute written

    MSread = MatchedSources.read(FileName);

    testCase.verifyEmpty(MSread.MagType, ...
        'An unstamped product should read back with an empty MagType.');
    testCase.verifyEqual(MSread.Nsrc, 100);
end

function testMatchedSourcesMagTypeRejectsInvalidValue(testCase)
    MS = MatchedSources;
    testCase.verifyError(@() setMagType(MS, 'xxx'), ?MException);
end

%% Helpers

function setMagType(MS, Val)
    % Assign MagType so the property set-validation can be exercised in a
    % function handle (verifyError needs a callable).
    MS.MagType = Val;
end
