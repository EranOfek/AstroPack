function Mask = flagBadEpochs(MS, BadFlags, Args)
    % Per-epoch bad-source mask from FLAGS bits of a MatchedSources.
    % Description: Builds a logical [Nepoch x Nsrc] mask that is true wherever
    %              the FLAGS field of the MatchedSources carries any of the
    %              named bad bits. Used to NaN out flagged epochs before
    %              computing stability statistics. Unifies the FLAGS-masking
    %              logic previously duplicated across the stability plotters.
    % Input  : - MS       - a single MatchedSources object.
    %          - BadFlags - flag name(s) (char/string/cell) to mask.
    %                       Default {} (mask is all false).
    %          * ...,key,val,...
    %            'SizeRefField' - Data field whose size sets the mask shape
    %                             when FLAGS is absent. Default '' (use FLAGS,
    %                             else the first Data field).
    % Output : - Mask - logical [Nepoch x Nsrc] (all false if no FLAGS field
    %            or no BadFlags requested).
    % Author : photCalib package refactor (2026-05)
    % Example: Mask = flagBadEpochs(MS, {'Saturated','NearEdge'});

    arguments
        MS                MatchedSources
        BadFlags                       = {}
        Args.SizeRefField {mustBeText} = ''
    end

    if ischar(BadFlags) || isstring(BadFlags)
        BadFlags = cellstr(BadFlags);
    end

    % Determine mask shape
    RefField = char(Args.SizeRefField);
    if ~isempty(RefField) && isfield(MS.Data, RefField)
        Mask = false(size(MS.Data.(RefField)));
    elseif isfield(MS.Data, 'FLAGS')
        Mask = false(size(MS.Data.FLAGS));
    else
        F = fieldnames(MS.Data);
        if isempty(F)
            Mask = false(0);
            return;
        end
        Mask = false(size(MS.Data.(F{1})));
    end

    if isempty(BadFlags) || ~isfield(MS.Data, 'FLAGS')
        return;
    end

    FlagMat = MS.Data.FLAGS;
    FlagMat(isnan(FlagMat)) = 0;
    try
        BD = BitDictionary;
        for Ifl = 1:numel(BadFlags)
            [~, ~, BitDec] = BD.name2bit(BadFlags{Ifl});
            Mask = Mask | (bitand(uint32(FlagMat), uint32(BitDec)) > 0);
        end
    catch
        % FLAGS bit lookup failed — leave the mask as-is (no epochs masked).
    end
end
