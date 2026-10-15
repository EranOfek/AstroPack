function tests = test_read_ZTF_cat
    % Unit tests for VO.ZTF.read_ZTF_cat and its file-name / flag helpers.
    %
    % Two kinds of test. The synthetic ones build a psfcat and a sexcat in a
    % temporary folder, carrying the quirks of the real files - vector
    % aperture columns, a FLUX_APER one element SHORT of MAG_APER,
    % SExtractor's +/-99 magnitudes, absolute flux errors, and the
    % photometric keywords in the binary-table header - so they run anywhere.
    % The rest read the real ZTF archive at /bigdata3/projects/ZTF (mounted
    % on dana's workstation and on archimedes) and are skipped when it is
    % not reachable. Set ZTF_DATA_DIR to point them elsewhere.

    tests = functiontests(localfunctions);
end

%% Fixture helpers

function Dir = ztfDataDir()
    % Directory of the real ZTF per-epoch catalogs, or '' when unreachable.
    Dir = getenv('ZTF_DATA_DIR');
    if isempty(Dir) || ~isfolder(Dir)
        Dir = '/bigdata3/projects/ZTF/HS2209+1914';
    end
    if ~isfolder(Dir)
        Dir = '';
    end
end

function assumeZtfData(testCase)
    % Skip (not fail) when the ZTF archive is not mounted on this machine.
    testCase.assumeTrue(~isempty(ztfDataDir()), ...
        'Skipping: the ZTF archive is not reachable (set ZTF_DATA_DIR).');
end

function Stem = synthStem()
    Stem = 'ztf_20180606421482_000596_zr_c10_o_q3';
end

function File = writeSynthSexcat(Dir, RAoffset)
    % Six sources: 1-4 clean, 5 flagged saturated+truncated, 6 unmeasurable.
    if nargin < 2
        RAoffset = 0;
    end
    File = fullfile(Dir, [synthStem(), '_sexcat.fits']);
    N   = 6;
    RA  = 332.90 + (0:N-1)'.*0.001 + RAoffset;
    Dec = 19.50  + (0:N-1)'.*0.001;
    Mag = repmat([-10 -10.6 -11 -11.3 -11.5 -11.55], N, 1) - (0:N-1)';
    Mag(6,:) = 99;                               % SExtractor: not measurable
    EFlux = repmat([20 30 40 50 60 70], N, 1);   % ABSOLUTE counts
    Flags = [0; 0; 0; 0; 4+8; 0];
    Apcor = [-1.269 -0.6444 -0.3384 -0.1182 -0.0265 -0.0070];

    Fp = matlab.io.fits.createFile(File);
    matlab.io.fits.createImg(Fp, 'byte_img', []);
    matlab.io.fits.writeKey(Fp, 'ORIGIN', 'SExtractor');
    Ttype = {'NUMBER','FLAGS','XWIN_IMAGE','YWIN_IMAGE','ALPHAWIN_J2000','DELTAWIN_J2000', ...
             'MAG_APER','MAGERR_APER','FLUXERR_APER','FLUX_APER','CLASS_STAR','FWHM_IMAGE', ...
             'BACKGROUND','X2WIN_IMAGE','Y2WIN_IMAGE','XYWIN_IMAGE','ELONGATION'};
    Tform = {'1J','1J','1E','1E','1D','1D','6E','6E','6E','5E','1E','1E','1E','1D','1D','1D','1E'};
    matlab.io.fits.createTbl(Fp, 'binary', N, Ttype, Tform, repmat({''},1,numel(Ttype)), 'OBJECTS');
    matlab.io.fits.writeKey(Fp, 'MAGZP',    26.2664, 'zero point');
    matlab.io.fits.writeKey(Fp, 'MAGZPRMS', 0.0213,  'zero point rms');
    matlab.io.fits.writeKey(Fp, 'CLRCOEFF', 0.0937,  'colour coefficient');
    matlab.io.fits.writeKey(Fp, 'OBSMJD',   58275.4215, 'MJD');
    matlab.io.fits.writeKey(Fp, 'FIXAPERS', '2,3,4,6,10,14');
    for Ia = 1:6
        matlab.io.fits.writeKey(Fp, sprintf('APCOR%d', Ia),   Apcor(Ia), 'psf-aper');
        matlab.io.fits.writeKey(Fp, sprintf('APCORUN%d', Ia), 0.001,     'uncertainty');
    end
    matlab.io.fits.writeCol(Fp,  1, 1, int32((1:N)'));
    matlab.io.fits.writeCol(Fp,  2, 1, int32(Flags));
    matlab.io.fits.writeCol(Fp,  3, 1, single((101:100+N)'));
    matlab.io.fits.writeCol(Fp,  4, 1, single((201:200+N)'));
    matlab.io.fits.writeCol(Fp,  5, 1, RA);
    matlab.io.fits.writeCol(Fp,  6, 1, Dec);
    matlab.io.fits.writeCol(Fp,  7, 1, single(Mag));
    matlab.io.fits.writeCol(Fp,  8, 1, single(ones(N,6).*0.01));
    matlab.io.fits.writeCol(Fp,  9, 1, single(EFlux));
    matlab.io.fits.writeCol(Fp, 10, 1, single(10.^(-0.4.*Mag(:,1:5))));
    matlab.io.fits.writeCol(Fp, 11, 1, single(ones(N,1).*0.98));
    matlab.io.fits.writeCol(Fp, 12, 1, single(ones(N,1).*2.5));
    matlab.io.fits.writeCol(Fp, 13, 1, single(ones(N,1).*300));
    matlab.io.fits.writeCol(Fp, 14, 1, ones(N,1).*1.1);
    matlab.io.fits.writeCol(Fp, 15, 1, ones(N,1).*1.2);
    matlab.io.fits.writeCol(Fp, 16, 1, ones(N,1).*0.1);
    matlab.io.fits.writeCol(Fp, 17, 1, single(ones(N,1).*1.05));
    matlab.io.fits.closeFile(Fp);
end

function File = writeSynthPsfcat(Dir)
    % Five sources on the same positions as the synthetic sexcat.
    File = fullfile(Dir, [synthStem(), '_psfcat.fits']);
    N    = 5;
    RA   = 332.90 + (0:N-1)'.*0.001;
    Dec  = 19.50  + (0:N-1)'.*0.001;
    Flux = (10000:-1000:6000)';

    Fp = matlab.io.fits.createFile(File);
    matlab.io.fits.createImg(Fp, 'byte_img', []);
    matlab.io.fits.writeKey(Fp, 'MAGZP',    26.2664, 'zero point');
    matlab.io.fits.writeKey(Fp, 'MAGZPRMS', 0.0213,  'zero point rms');
    matlab.io.fits.writeKey(Fp, 'CLRCOEFF', 0.0937,  'colour coefficient');
    matlab.io.fits.writeKey(Fp, 'OBSMJD',   58275.4215, 'MJD');
    Ttype = {'sourceid','xpos','ypos','ra','dec','flux','sigflux','mag','sigmag', ...
             'snr','chi','sharp','flags'};
    Tform = {'1J','1E','1E','1D','1D','1E','1E','1E','1E','1E','1E','1E','1J'};
    matlab.io.fits.createTbl(Fp, 'binary', N, Ttype, Tform, repmat({''},1,numel(Ttype)), 'PSFCAT');
    matlab.io.fits.writeCol(Fp,  1, 1, int32((1:N)'));
    matlab.io.fits.writeCol(Fp,  2, 1, single((101:100+N)'));
    matlab.io.fits.writeCol(Fp,  3, 1, single((201:200+N)'));
    matlab.io.fits.writeCol(Fp,  4, 1, RA);
    matlab.io.fits.writeCol(Fp,  5, 1, Dec);
    matlab.io.fits.writeCol(Fp,  6, 1, single(Flux));
    matlab.io.fits.writeCol(Fp,  7, 1, single(Flux./100));
    matlab.io.fits.writeCol(Fp,  8, 1, single(-2.5.*log10(Flux)));
    matlab.io.fits.writeCol(Fp,  9, 1, single(ones(N,1).*0.011));
    matlab.io.fits.writeCol(Fp, 10, 1, single(ones(N,1).*100));
    matlab.io.fits.writeCol(Fp, 11, 1, single(ones(N,1).*1.02));
    matlab.io.fits.writeCol(Fp, 12, 1, single(ones(N,1).*0.1));
    matlab.io.fits.writeCol(Fp, 13, 1, int32(zeros(N,1)));
    matlab.io.fits.closeFile(Fp);
end

%% Test Functions - file names

function testFilenameParsesPsfcat(testCase)
    % Every field of a psfcat name, including the derived readout channel.
    Prop = VO.ZTF.ztf_filename2prop('ztf_20180606421482_000596_zr_c10_o_q3_psfcat.fits');
    testCase.verifyEqual(Prop.FileType, 'psfcat', 'FileType misparsed.');
    testCase.verifyEqual(Prop.Field,  596,  'Field misparsed.');
    testCase.verifyEqual(Prop.Filter, 'zr', 'Filter misparsed.');
    testCase.verifyEqual(Prop.CCDID,  10,   'CCDID misparsed.');
    testCase.verifyEqual(Prop.QuadID, 3,    'QuadID misparsed.');
    testCase.verifyEqual(Prop.RCID,   38,   'RCID should be 4*(CCDID-1)+QuadID-1.');
    testCase.verifyEqual(Prop.Year,   2018, 'Year misparsed.');
end

function testFilenameParsesOtherProducts(testCase)
    % sexcat and sciimg used to raise 'Unsupported ztf file type'; the
    % per-epoch products differ only in the last token of the name.
    Sex = VO.ZTF.ztf_filename2prop('ztf_20180606421482_000596_zr_c10_o_q3_sexcat.fits');
    testCase.verifyEqual(Sex.FileType, 'sexcat', 'sexcat should parse.');
    testCase.verifyEqual(Sex.Field, 596, 'Field misparsed for a sexcat.');

    Img = VO.ZTF.ztf_filename2prop('/some/dir/ztf_20180606421482_000596_zg_c01_o_q1_sciimg.fits');
    testCase.verifyEqual(Img.FileType, 'sciimg', 'sciimg should parse.');
    testCase.verifyEqual(Img.Filter, 'zg', 'Filter misparsed with a directory prefix.');
    testCase.verifyEqual(Img.RCID, 0, 'RCID of CCD 1 quadrant 1 is 0.');
end

function testFilenameRoundTrip(testCase)
    % prop2filename is the inverse, and names the sibling of the exposure.
    Name = 'ztf_20180606421482_000596_zr_c10_o_q3_psfcat.fits';
    Prop = VO.ZTF.ztf_filename2prop(Name);
    testCase.verifyEqual(VO.ZTF.ztf_prop2filename(Prop), Name, 'Round trip failed.');
    testCase.verifyEqual(VO.ZTF.ztf_prop2filename(Prop, 'sexcat'), ...
        strrep(Name, 'psfcat', 'sexcat'), 'Sibling name is wrong.');
end

function testPixScale(testCase)
    % Plate scale and quadrant size, used for CCDSEC and aperture radii.
    [Scale, Naxis] = VO.ZTF.pixScale();
    testCase.verifyEqual(Scale, 1.0126, 'AbsTol', 1e-6, 'Plate scale changed.');
    testCase.verifyEqual(Naxis, [3072 3080], 'Quadrant size changed.');
end

%% Test Functions - flags

function testFlagTranslation(testCase)
    % Only the two SExtractor conditions with a LAST equivalent translate.
    BD = BitDictionary('BitMask.Image.Default');
    [~, SatDec]  = BD.name2bit('Saturated');
    [~, EdgeDec] = BD.name2bit('NearEdge');

    [Mask, Map] = VO.ZTF.flags2LastMask([0; 1; 2; 4; 8; 4+8; 16; -1]);
    testCase.verifyEqual(double(Mask(1)), 0, 'Clean source must stay clean.');
    testCase.verifyEqual(double(Mask(2)), 0, 'Neighbour-biased is not translated.');
    testCase.verifyEqual(double(Mask(3)), 0, 'Deblended is not translated.');
    testCase.verifyEqual(double(Mask(4)), double(SatDec), 'Bit 4 is Saturated.');
    testCase.verifyEqual(double(Mask(5)), double(EdgeDec), 'Bit 8 is NearEdge.');
    testCase.verifyEqual(double(Mask(6)), double(bitor(uint32(SatDec), uint32(EdgeDec))), ...
        'Both bits should combine.');
    testCase.verifyEqual(double(Mask(7)), 0, 'Corrupted aperture data is not translated.');
    testCase.verifyEqual(double(Mask(8)), 0, 'A negative flag value is invalid.');
    testCase.verifyEqual(Map.Nbad, 1, 'The invalid value should be counted.');
end

%% Test Functions - synthetic catalogs

function testReadSexcatSynthetic(testCase)
    % Aperture expansion, the recovered sixth flux, relative errors and SN.
    Dir = tempname;  mkdir(Dir);
    Cleanup = onCleanup(@() rmdir(Dir, 's')); %#ok<NASGU>
    File = writeSynthSexcat(Dir);

    [AI, Info] = VO.ZTF.read_ZTF_cat(File, 'AirMass', 1.32);

    testCase.verifyEqual(Info.Nsrc, 4, 'Flagged and unmeasurable rows must be screened.');
    testCase.verifyEqual(Info.NsrcIn, 6, 'Input count is wrong.');

    Cat = AI.CatData;
    testCase.verifyTrue(all(ismember({'FLUX_APER_1','FLUX_APER_6','FLUXERR_APER_6','SN', ...
        'ZTF_MAG_APER','SEX_FLAGS','FLAGS','BACK_IM'}, Cat.ColNames)), ...
        'A LAST-like column is missing.');
    testCase.verifyEqual(numel(Cat.ColNames), numel(Cat.ColUnits), ...
        'ColUnits must have one entry per column.');

    Flux6 = Cat.getCol('FLUX_APER_6');
    testCase.verifyTrue(all(isfinite(Flux6) & Flux6 > 0), ...
        'Aperture 6 must exist although ZTF stores only 5 fluxes.');

    % error written as 70 counts on the flux of magnitude -11.55
    testCase.verifyEqual(Cat.getCol('FLUXERR_APER_6'), ...
        70./10.^(0.4.*(11.55 + (0:3)')), 'RelTol', 1e-4, ...
        'FLUXERR must be converted to a relative error.');
    testCase.verifyEqual(Cat.getCol('SN'), 1./Cat.getCol('FLUXERR_APER_5'), ...
        'RelTol', 1e-10, 'SN must be 1/FLUXERR of the chosen aperture.');
    testCase.verifyEqual(Cat.getCol('ZTF_MAG_APER'), ...
        -11.5 - (0:3)' + 26.2664 - 0.0265, 'AbsTol', 1e-3, ...
        'ZTF_MAG_APER must be MAG_APER + MAGZP + APCOR.');

    testCase.verifyEqual(Info.FixApers, [2 3 4 6 10 14], 'AbsTol', 1e-9, ...
        'FIXAPERS misparsed.');
    testCase.verifyEqual(Info.ApRadiusPix, [1 1.5 2 3 5 7], 'AbsTol', 1e-9, ...
        'FIXAPERS are diameters; radii are half of them.');
    testCase.verifyEqual(AI.HeaderData.getVal('APERRAD5'), 5, 'AbsTol', 1e-9, ...
        'Aperture radii must reach the header.');
    testCase.verifyEqual(AI.HeaderData.getVal('AIRMASS'), 1.32, 'AbsTol', 1e-9, ...
        'AIRMASS must reach the header.');
    testCase.verifyEqual(AI.HeaderData.getVal('CCDSEC'), '[1 3072 1 3080]', ...
        'CCDSEC must describe the readout quadrant.');
end

function testReadSexcatKeepFlagged(testCase)
    % With KeepFlagged the flagged row survives and its bits are translated.
    Dir = tempname;  mkdir(Dir);
    Cleanup = onCleanup(@() rmdir(Dir, 's')); %#ok<NASGU>
    File = writeSynthSexcat(Dir);

    [AI, Info] = VO.ZTF.read_ZTF_cat(File, 'KeepFlagged', true, 'AirMass', 1.32);

    testCase.verifyEqual(Info.Nsrc, 5, ...
        'Only the unmeasurable row should go when flags are kept.');
    BD = BitDictionary('BitMask.Image.Default');
    [~, SatDec]  = BD.name2bit('Saturated');
    [~, EdgeDec] = BD.name2bit('NearEdge');
    Flags = AI.CatData.getCol('FLAGS');
    testCase.verifyEqual(sum(Flags > 0), 1, 'Exactly one row is flagged.');
    testCase.verifyEqual(double(max(Flags)), ...
        double(bitor(uint32(SatDec), uint32(EdgeDec))), 'Wrong LAST bits.');
    testCase.verifyEqual(max(AI.CatData.getCol('SEX_FLAGS')), 12, ...
        'The untranslated original must travel alongside.');
end

function testReadPsfcatSynthetic(testCase)
    % PSF columns, the relative error and the zero-pointed magnitude.
    Dir = tempname;  mkdir(Dir);
    Cleanup = onCleanup(@() rmdir(Dir, 's')); %#ok<NASGU>
    File = writeSynthPsfcat(Dir);

    [Cat, Info] = VO.ZTF.read_ZTF_cat(File, 'AirMass', 1.5, 'OutType', 'AstroCatalog');

    testCase.verifyTrue(isa(Cat, 'AstroCatalog'), 'OutType AstroCatalog ignored.');
    testCase.verifyEqual(Info.Nsrc, 5, 'All synthetic sources are clean.');
    testCase.verifyEqual(Cat.getCol('FLUXERR_PSF'), ones(5,1)./100, 'RelTol', 1e-5, ...
        'FLUXERR_PSF must be sigflux/flux.');
    testCase.verifyEqual(Cat.getCol('ZTF_MAG_PSF'), ...
        -2.5.*log10((10000:-1000:6000)') + 26.2664, 'AbsTol', 1e-3, ...
        'ZTF_MAG_PSF must be the instrumental magnitude plus MAGZP.');
    testCase.verifyEqual(Info.ObsJD, 58275.4215 + 2400000.5, 'AbsTol', 1e-6, ...
        'ObsJD must come from OBSMJD.');
end

function testReadBothSynthetic(testCase)
    % 'both' keeps one row per cross-matched source, with both flux types.
    Dir = tempname;  mkdir(Dir);
    Cleanup = onCleanup(@() rmdir(Dir, 's')); %#ok<NASGU>
    writeSynthSexcat(Dir);
    writeSynthPsfcat(Dir);
    File = fullfile(Dir, [synthStem(), '_sexcat.fits']);

    [AI, Info] = VO.ZTF.read_ZTF_cat(File, 'CatType', 'both', 'AirMass', 1.32);

    testCase.verifyEqual(Info.Nmatched, 4, 'All four clean sources should match.');
    testCase.verifyTrue(all(ismember({'FLUX_APER_5','FLUX_PSF','DIST_PSF_APER'}, ...
        AI.CatData.ColNames)), 'Merged catalog must carry both flux types.');
    testCase.verifyEqual(numel(AI.CatData.ColNames), numel(AI.CatData.ColUnits), ...
        'ColUnits must have one entry per column.');
    testCase.verifyLessThan(max(AI.CatData.getCol('DIST_PSF_APER')), 0.1, ...
        'Matched separations should be far below the match radius.');
end

function testReadBothNoCounterpart(testCase)
    % Shifted positions: the merge must come back empty, not wrong.
    Dir = tempname;  mkdir(Dir);
    Cleanup = onCleanup(@() rmdir(Dir, 's')); %#ok<NASGU>
    writeSynthSexcat(Dir, 0.01);
    writeSynthPsfcat(Dir);
    File = fullfile(Dir, [synthStem(), '_sexcat.fits']);

    [~, Info] = VO.ZTF.read_ZTF_cat(File, 'CatType', 'both', 'AirMass', 1.32);
    testCase.verifyEqual(Info.Nmatched, 0, 'Nothing should match after the shift.');
end

%% Test Functions - the real archive (skipped when not mounted)

function testRealPsfcat(testCase)
    % A known file of the ZTF archive: source count and zero point.
    assumeZtfData(testCase);
    File = fullfile(ztfDataDir(), 'ztf_20180606421482_000596_zr_c10_o_q3_psfcat.fits');
    testCase.assumeTrue(isfile(File), 'Skipping: reference psfcat is missing.');

    [AI, Info] = VO.ZTF.read_ZTF_cat(File, 'AirMass', 1.32);
    testCase.verifyEqual(Info.NsrcIn, 8524, 'Row count of the reference psfcat changed.');
    testCase.verifyEqual(Info.Nsrc, 7717, 'Flag screening of the reference psfcat changed.');
    testCase.verifyEqual(Info.MAGZP, 26.2664, 'AbsTol', 1e-4, 'MAGZP misread.');
    testCase.verifyTrue(all(AI.CatData.getCol('FLUXERR_PSF') > 0), ...
        'Relative errors must be positive.');
end

function testRealSexcatApertures(testCase)
    % The strongest check there is: on REAL data the sixth flux, rebuilt
    % from the magnitudes, agrees with the five ZTF stores to float32.
    assumeZtfData(testCase);
    File = fullfile(ztfDataDir(), 'ztf_20180606421482_000596_zr_c10_o_q3_sexcat.fits');
    testCase.assumeTrue(isfile(File), 'Skipping: reference sexcat is missing.');

    [AI, Info] = VO.ZTF.read_ZTF_cat(File, 'AirMass', 1.32);
    testCase.verifyEqual(Info.FixApers, [2 3 4 6 10 14], 'AbsTol', 1e-9, ...
        'Aperture diameters of the reference sexcat changed.');
    testCase.verifyLessThan(Info.MaxFluxRelDiff, 1e-5, ...
        'Flux rebuilt from MAG_APER must match ZTF''s own FLUX_APER.');
    testCase.verifyEqual(Info.APCOR(5), -0.0265, 'AbsTol', 1e-3, ...
        'APCOR5 of the reference sexcat changed.');
    testCase.verifyEqual(Info.Nsrc, 5408, 'Screening of the reference sexcat changed.');
    testCase.verifyTrue(all(isfinite(AI.CatData.getCol('FLUX_APER_6'))), ...
        'The sixth aperture must be finite for every kept row.');
end

function testRealBothMatchFraction(testCase)
    % The two real source lists are different, and most sexcat sources have
    % a psfcat counterpart within an arcsecond.
    assumeZtfData(testCase);
    Sex = fullfile(ztfDataDir(), 'ztf_20180606421482_000596_zr_c10_o_q3_sexcat.fits');
    Psf = fullfile(ztfDataDir(), 'ztf_20180606421482_000596_zr_c10_o_q3_psfcat.fits');
    testCase.assumeTrue(isfile(Sex) && isfile(Psf), ...
        'Skipping: a reference catalog is missing.');

    [AI, Info] = VO.ZTF.read_ZTF_cat(Sex, 'CatType', 'both', 'AirMass', 1.32);
    testCase.verifyGreaterThan(Info.MatchFrac, 0.85, ...
        'Most sexcat sources should match a psfcat source.');
    testCase.verifyLessThan(median(AI.CatData.getCol('DIST_PSF_APER')), 0.3, ...
        'Median psfcat-sexcat separation should be well under an arcsecond.');
    testCase.verifyTrue(all(ismember({'FLUX_PSF','FLUX_APER_5'}, AI.CatData.ColNames)), ...
        'Merged real catalog must carry both flux types.');
end
