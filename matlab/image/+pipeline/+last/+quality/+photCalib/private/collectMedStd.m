function S = collectMedStd(MS, Args)
    % Per-source median(X) and std(Y) over epochs from a MatchedSources.
    % Description: For one MatchedSources object, returns the per-source
    %              median of a reference quantity (XField) and the per-source
    %              epoch-to-epoch scatter (std) of a measured quantity
    %              (YField). Bad epochs can be masked out, faint sources cut
    %              on XField, and an optional cos-latitude scaling applied to
    %              the scatter (for RA-type angular quantities). Shared
    %              stability-statistics helper.
    % Input  : - MS - a single MatchedSources object.
    %          * ...,key,val,...
    %            'YField'        - Data field whose epoch std is returned.
    %                              Required.
    %            'XField'        - Reference Data field whose epoch median is
    %                              returned (and on which the faint cut
    %                              acts). Required.
    %            'MinEpochs'     - Drop sources with fewer finite Y epochs.
    %                              0 = no cut. Default 0.
    %            'BadEpochMask'  - Logical [Nepoch x Nsrc] mask of epochs to
    %                              NaN out before reducing. Default [].
    %            'MaxX'          - Faint cut: XField values above this are
    %                              NaN'd out. Default inf.
    %            'ScaleCosField' - Data field (degrees) whose per-source
    %                              median cosine multiplies Std. Default ''.
    % Output : - S - struct with row vectors Med (per-source median XField)
    %            and Std (per-source std YField). Empty when a field is
    %            missing.
    % Author : photCalib package refactor (2026-05)
    % Example: S = collectMedStd(MS, 'YField','RA', 'XField','MAG_PSF', ...
    %                            'ScaleCosField','Dec');

    arguments
        MS                 MatchedSources
        Args.YField        {mustBeTextScalar}
        Args.XField        {mustBeTextScalar}
        Args.MinEpochs     (1,1) double = 0
        Args.BadEpochMask               = []
        Args.MaxX          (1,1) double = inf
        Args.ScaleCosField {mustBeText} = ''
    end

    S = struct('Med', [], 'Std', []);
    if ~isfield(MS.Data, Args.YField) || ~isfield(MS.Data, Args.XField)
        return;
    end

    Y = MS.Data.(Args.YField);
    X = MS.Data.(Args.XField);

    if ~isempty(Args.BadEpochMask)
        Y(Args.BadEpochMask) = NaN;
        X(Args.BadEpochMask) = NaN;
    end

    BadX = false(size(X));
    if isfinite(Args.MaxX)
        BadX = X > Args.MaxX;
        Y(BadX) = NaN;
        X(BadX) = NaN;
    end

    ScaleField = char(Args.ScaleCosField);
    HaveScale  = ~isempty(ScaleField) && isfield(MS.Data, ScaleField);
    if HaveScale
        Z = MS.Data.(ScaleField);
        if ~isempty(Args.BadEpochMask); Z(Args.BadEpochMask) = NaN; end
        Z(BadX) = NaN;
    end

    if Args.MinEpochs > 0
        Good = sum(~isnan(Y), 1) >= Args.MinEpochs;
        Y = Y(:, Good);
        X = X(:, Good);
        if HaveScale; Z = Z(:, Good); end
    end

    S.Med = median(X, 1, 'omitnan');
    S.Std = std(Y, 0, 1, 'omitnan');
    if HaveScale
        S.Std = S.Std .* cosd(median(Z, 1, 'omitnan'));
    end
end
