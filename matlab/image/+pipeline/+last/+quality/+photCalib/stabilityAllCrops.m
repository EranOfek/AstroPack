function [MS, Info] = stabilityAllCrops(Args)
    % Build a per-crop MatchedSources for every crop of a field (all epochs).
    % Description: Runs stabilityN3's load+match step once PER CROP over the
    %              same set of visits, so each crop gets its own MatchedSources
    %              (epochs cross-matched within that crop). Returns a 1xNcrop
    %              cell of MatchedSources - the per-crop analogue of the single
    %              MS stabilityN3 returns. stabilityN3 is called with
    %              'Plot',false so no per-crop figures are drawn.
    %
    %              The per-crop glob is built by substituting the crop number
    %              into a pattern: by default
    %                LAST*_<Field>_*_<crop>_sci_coadd_Cat_1.fits
    %              where <crop> is the 3-digit crop index (the last number
    %              before _sci_coadd). Override the whole glob via
    %              'PatternTemplate' (must contain a %d / %03d for the crop) for
    %              non-standard layouts.
    %
    %              Crops with no matching files (or that otherwise fail to
    %              build) are skipped: their cell stays [] and Info marks them
    %              Ok=false with the reason - the run does not abort.
    % Input  : * ...,key,val,...
    %            'DataPath'        - Directory holding the calibrated per-crop
    %                                _Cat_1.fits (required).
    %            'Field'           - Field token matched as _<Field>_ in the
    %                                filename (e.g. '1716.c', '1679'). Default
    %                                '1716.c'. Ignored when PatternTemplate set.
    %            'Crops'           - Crop numbers to build. Default 1:24.
    %            'PatternTemplate' - sprintf template for the per-crop glob;
    %                                must contain one integer conversion for the
    %                                crop (e.g. 'LAST*_1679_*_%03d_sci_coadd_Cat_1.fits').
    %                                Default '' (build from Field).
    %            'StabArgs'        - Cell of extra name-value pairs forwarded to
    %                                stabilityN3 (e.g. {'NEpochsCap',Inf,
    %                                'Mags',{'MAGAB__APER_3'},'MinEpochs',5}).
    %                                Default {}. ('DataPath','Pattern','Plot'
    %                                are set by this function and must not be
    %                                passed here.)
    %            'Verbose'         - Per-crop progress. Default true.
    % Output : - MS   - 1xNcrop cell; MS{i} is the MatchedSources for Crops(i),
    %                   or [] if that crop had no files / failed. Pass MS{i} to
    %                   plotPhotStabilityMap for a single crop, or the whole
    %                   cell to pool every crop in native pixel coordinates.
    %          - Info - 1xNcrop struct array: .CropId, .NEpochs, .NSrc, .Ok,
    %                   .ErrorMessage ('' on success).
    % Author : D. Kovaleva (Jul 2026)
    % See also: stabilityN3, plotPhotStabilityMap.
    % Example:
    %   % --- All 24 crops of field 1716.c, all epochs (no per-crop plots):
    %   [MS, Info] = pipeline.last.quality.photCalib.stabilityAllCrops( ...
    %       'DataPath', PerCropDir, 'Field', '1716.c', ...
    %       'StabArgs', {'NEpochsCap', Inf});
    %
    %   % --- Then map one crop, or pool all crops, vs native X/Y:
    %   pipeline.last.quality.photCalib.plotPhotStabilityMap(MS{10});   % crop 10
    %   pipeline.last.quality.photCalib.plotPhotStabilityMap(MS);       % all crops pooled
    %
    %   % --- Different field, subset of crops, custom mags:
    %   MS = pipeline.last.quality.photCalib.stabilityAllCrops( ...
    %       'DataPath', Dir1679, 'Field', '1679', 'Crops', [5 10 15 20], ...
    %       'StabArgs', {'Mags', {'MAGAB__APER_3','MAG_APER_3'}, 'NEpochsCap', Inf});
    %
    %   % --- Non-standard layout: give the whole glob template explicitly:
    %   MS = pipeline.last.quality.photCalib.stabilityAllCrops( ...
    %       'DataPath', Dir, 'PatternTemplate', 'LAST*_1679_*_%03d_sci_coadd_Cat_1.fits');
    arguments
        Args.DataPath        (1,:) char
        Args.Field           (1,:) char   = '1716.c'
        Args.Crops           (1,:) double = 1:24
        Args.PatternTemplate (1,:) char   = ''
        Args.StabArgs              cell   = {}
        Args.Verbose         (1,1) logical = true
    end

    if isempty(Args.DataPath)
        error('pipeline:last:quality:photCalib:stabilityAllCrops:NoDataPath', ...
              'DataPath is required.');
    end

    Ncrop = numel(Args.Crops);
    MS    = cell(1, Ncrop);
    Info  = repmat(struct('CropId', NaN, 'NEpochs', 0, 'NSrc', 0, ...
                          'Ok', false, 'ErrorMessage', ''), 1, Ncrop);

    for I = 1:Ncrop
        C = Args.Crops(I);
        Info(I).CropId = C;

        if isempty(Args.PatternTemplate)
            Pattern = sprintf('LAST*_%s_*_%03d_sci_coadd_Cat_1.fits', Args.Field, C);
        else
            Pattern = sprintf(Args.PatternTemplate, C);
        end

        if Args.Verbose
            fprintf('[crop %2d/%d] Pattern "%s"\n', I, Ncrop, Pattern);
        end

        try
            M = pipeline.last.quality.photCalib.stabilityN3( ...
                    'DataPath', Args.DataPath, 'Pattern', Pattern, ...
                    'Plot', false, Args.StabArgs{:});
            MS{I}           = M;
            Info(I).Ok      = true;
            % Report shape from the first Data field, if any.
            Fn = fieldnames(M.Data);
            if ~isempty(Fn)
                Sz = size(M.Data.(Fn{1}));
                Info(I).NEpochs = Sz(1);
                Info(I).NSrc    = Sz(2);
            end
            if Args.Verbose
                fprintf('           -> %d epochs x %d sources\n', ...
                        Info(I).NEpochs, Info(I).NSrc);
            end
        catch ME
            Info(I).ErrorMessage = ME.message;
            if Args.Verbose
                fprintf('           -> SKIPPED: %s\n', ME.message);
            end
        end
    end

    if Args.Verbose
        fprintf('stabilityAllCrops: %d/%d crops built\n', sum([Info.Ok]), Ncrop);
    end
end
