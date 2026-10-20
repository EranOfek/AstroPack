function tests = test_PhotCalibTrans_ccdsec_provenance
    % Unit tests for the provenance of PhotCalibTrans.CCDSEC.
    %
    % CCDSEC reports the image section the METADATA gave - calibrate and
    % photCalibTransFromHeader read it from the header keyword. calcAperCorr
    % needs a section to normalize its position-dependent fit onto [-1,1] and
    % falls back to the LAST per-crop default when there is none, but it must
    % not write that fallback onto the property: a default stored there is
    % indistinguishable from a section that really came from a header, which
    % is exactly how a ZTF run was reported as having CCDSEC [1 1716 1 1716].
    % The fallback is recorded inside the per-aperture fit struct instead, and
    % announced in the log.
    % Author : D. Kovaleva (Oct 2026)
    % Example: runtests('tests/image/test_PhotCalibTrans_ccdsec_provenance.m')
    tests = functiontests(localfunctions);
end

%% Test Functions

function testFallbackIsNotWrittenOntoTheProperty(TestCase)
    % The regression: a positional fit on a catalog whose metadata carried no
    % CCDSEC must leave the property empty.
    PC  = PhotCalibTrans;
    Cat = localSyntheticCat();
    verifyEmpty(TestCase, PC.CCDSEC, 'A fresh object must have no CCDSEC.');
    PC  = PC.calcAperCorr(Cat, 'Positional', true, 'MinSN', 5, 'CalcCorrType', 'flux');
    verifyEmpty(TestCase, PC.CCDSEC, ...
        'calcAperCorr wrote a fallback section onto the CCDSEC property.');
end

function testFitRecordsTheSectionItUsed(TestCase)
    % The normalization is not lost - it travels inside the fit struct, which
    % is what applyAperCorr evaluates through.
    PC  = PhotCalibTrans;
    PC  = PC.calcAperCorr(localSyntheticCat(), 'Positional', true, 'MinSN', 5, 'CalcCorrType', 'flux');
    Idx = find(~cellfun(@isempty, PC.AperCorrPositional), 1);
    verifyNotEmpty(TestCase, Idx, 'No positional fit was stored.');
    Fit = PC.AperCorrPositional{Idx};
    verifyTrue(TestCase, isfield(Fit, 'CCDSEC'), ...
        'The positional fit does not record the section it was normalized on.');
    verifyEqual(TestCase, Fit.CCDSEC, [1 1716 1 1716], ...
        'The recorded section is not the documented fallback.');
end

function testAnExistingSectionIsKeptAndUsed(TestCase)
    % A section that did come from the metadata is neither replaced nor lost.
    PC        = PhotCalibTrans;
    PC.CCDSEC = [1 3072 1 3080];            % one ZTF readout quadrant
    PC        = PC.calcAperCorr(localSyntheticCat(), 'Positional', true, 'MinSN', 5, 'CalcCorrType', 'flux');
    verifyEqual(TestCase, PC.CCDSEC, [1 3072 1 3080], ...
        'An existing CCDSEC must survive calcAperCorr.');
    Idx = find(~cellfun(@isempty, PC.AperCorrPositional), 1);
    verifyEqual(TestCase, PC.AperCorrPositional{Idx}.CCDSEC, [1 3072 1 3080], ...
        'The fit must normalize on the section the metadata gave.');
end

function testExplicitSectionDoesNotTouchTheProperty(TestCase)
    % 'PosCCDSEC' says how to normalize this fit; it is not a claim about the
    % metadata, so it must not land on the property either.
    PC = PhotCalibTrans;
    PC = PC.calcAperCorr(localSyntheticCat(), 'Positional', true, 'MinSN', 5, 'CalcCorrType', 'flux', ...
                         'PosCCDSEC', [1 2048 1 2048]);
    verifyEmpty(TestCase, PC.CCDSEC, ...
        'An explicit PosCCDSEC was written onto the CCDSEC property.');
    Idx = find(~cellfun(@isempty, PC.AperCorrPositional), 1);
    verifyEqual(TestCase, PC.AperCorrPositional{Idx}.CCDSEC, [1 2048 1 2048], ...
        'The explicit section did not reach the fit.');
end

function testScalarRunLeavesThePropertyEmpty(TestCase)
    % Without a positional fit there is nothing to normalize and nothing to
    % warn about, and the property stays empty as well.
    PC = PhotCalibTrans;
    PC = PC.calcAperCorr(localSyntheticCat(), 'Positional', false, 'MinSN', 5, 'CalcCorrType', 'flux');
    verifyEmpty(TestCase, PC.CCDSEC, ...
        'A scalar aperture-correction run must not set CCDSEC.');
    verifyNotEmpty(TestCase, PC.AperCorr, ...
        'The scalar corrections should still have been computed.');
end

%% Helper

function Cat = localSyntheticCat()
    % A catalog with three apertures whose offsets from the reference vary
    % linearly across a 3072 x 3080 detector - enough for the positional fit
    % to run. Fluxes are noiseless: these tests are about where the
    % normalization is recorded, not about the fitted values. The tests ask
    % for CalcCorrType 'flux' so no calibrated-magnitude columns are needed.
    rng(42);
    N = 400;
    X = 1 + 3071*rand(N,1);
    Y = 1 + 3079*rand(N,1);
    Fref = 1e5 * ones(N,1);                       % reference aperture
    % aperture 1 loses flux towards +X, aperture 2 towards +Y
    F1 = Fref .* 10.^(-0.4*(0.20 + 0.05*(X/3072)));
    F2 = Fref .* 10.^(-0.4*(0.08 + 0.03*(Y/3080)));
    Rel = 0.002 * ones(N,1);                      % FLUXERR is relative (dF/F)
    Cat = AstroCatalog({[X, Y, F1, Rel, F2, Rel, Fref, Rel, 1./Rel, zeros(N,1)]}, ...
        'ColNames', {'X','Y','FLUX_APER_1','FLUXERR_APER_1', ...
                     'FLUX_APER_2','FLUXERR_APER_2', ...
                     'FLUX_APER_3','FLUXERR_APER_3','SN','FLAGS'}, ...
        'ColUnits', {'pix','pix','count','','count','','count','','',''});
end
