function T = fieldCalibrators(Args)
    % Expected number of photometric calibrators per crop, per LAST field.
    %   Loads the committed table last_field_calibrators.csv, which sits next to
    %   this function and was built by pipeline.last.quality.specDensityMap from
    %   the GAIADR3spec HTM index (2026 Oct 7).
    %
    %   The spectral-calibrator density is a property of the POINTING, not of
    %   the night (issue #1381): Gaia DR3 published sampled XP spectra for a
    %   selected subset of sources, and whole regions hold ~10% of the spectra
    %   per Gaia star that the typical sky holds. A field there cannot reach the
    %   calibrator floor of the photometric fit however good the night is, so
    %   its crops fall back to the reduced (Norm-only) mode by construction.
    %   Keeping the table lets a report say so instead of reporting a fault.
    %
    %   The table is read once and cached, so repeated calls are free.
    %
    %   One row per field of the 1784-field LAST tiling
    %   (celestial.grid.tile_the_sky(88,30)), whose row index is the FIELDID
    %   carried by the data products:
    %     Index           - FIELDID
    %     RA, Dec         - field centre [deg]
    %     GalLat          - galactic latitude [deg]
    %     MaxAlt          - culmination altitude at the site, lat 30.053 [deg]
    %     Visible         - reaches 30 deg altitude (1320 of the 1784 fields)
    %     SpecDens        - GAIADR3spec surface density [deg^-2]
    %     ExpectedPerCrop - SpecDens * 0.357 deg^2 (one crop) * 0.65 (survival)
    %     Below9, Below16 - expected below the bare minimum for the 8-parameter
    %                   fit (1 field) and below twice the parameter count
    %                   (54 fields). All 54 lie in the observable sky.
    %
    %   ExpectedPerCrop is an expectation within a factor of about 1.5: the
    %   0.65 survival fraction is the median PT_NCALI / (spectra in the crop)
    %   on 2026-10-14 without the CR_DeltaHT cut (issue #1414), and runs from
    %   0.55 at FWHM 3" to 0.69 above 6". Density median is 453 deg^-2
    %   all-sky, 403 over the observable sky.
    %
    % Input  : * ...,key,val,...
    %            'FileName' - Table to read. Default '' uses
    %                   last_field_calibrators.csv next to this function.
    %            'Reload' - Re-read the file instead of using the cache.
    %                   Default is false.
    % Output : - A table with one row per field of the 1784-field LAST tiling,
    %            whose row index is the FIELDID of the data products:
    %            Index, RA, Dec, GalLat, MaxAlt, Visible, SpecDens,
    %            ExpectedPerCrop, Below9, Below16. Empty when the file is
    %            missing, so a caller can degrade rather than fail.
    % Author : D. Kovaleva (Oct 2026)
    % Example: F = pipeline.last.quality.fieldCalibrators();
    %          F.Index(F.Below16 & F.Visible)   % starved fields that are observable
    %          F.ExpectedPerCrop(F.Index == 977)
    %          % to regenerate the file itself:
    %          T = pipeline.last.quality.specDensityMap('OutFile','/tmp/map.png');

    arguments
        Args.FileName char   = ''
        Args.Reload logical  = false
    end

    persistent Cache CachedFile

    FileName = Args.FileName;
    if isempty(FileName)
        % next to this function, so the table travels with the code
        FileName = fullfile(fileparts(mfilename('fullpath')), ...
                            'last_field_calibrators.csv');
    end

    if Args.Reload || isempty(Cache) || ~strcmp(CachedFile, FileName)
        if isfile(FileName)
            Cache = readtable(FileName);
            % Logical columns read back as 0/1 doubles.
            for Ic = {'Visible','Below9','Below16'}
                if ismember(Ic{1}, Cache.Properties.VariableNames)
                    Cache.(Ic{1}) = logical(Cache.(Ic{1}));
                end
            end
        else
            Cache = table();
        end
        CachedFile = FileName;
    end

    T = Cache;
end
