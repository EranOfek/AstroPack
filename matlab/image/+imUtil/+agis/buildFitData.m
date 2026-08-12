function Data = buildFitData(FieldStruct, JD, Args)
    % Build the validated data container used by the imUtil.agis fitting engine.
    % Input  : - FieldStruct : struct whose fields are observation matrices,
    %            each sized [Nepoch x Nsrc] (e.g., X, Y, Mag, Color, HA,
    %            ParAng, AirMass). Field names are user-defined; only X and Y
    %            are required by this function (individual terms may require
    %            others, e.g., Color, ParAng, AirMass).
    %          - JD : [Nepoch x 1] vector of Julian dates, one per epoch.
    %          * ...,key,val,...
    %            'RefEpoch'      - Reference epoch (JD) for the time origin.
    %                              Default: NaN -> median(JD).
    %            'ColorBinEdges' - Bin edges (mag) used to assign each source
    %                              to a color bin (Data.ColorBin).
    %                              Default: -0.5:0.5:3.5
    %            'ColorField'    - Name of the field in FieldStruct holding
    %                              color (used to build Data.ColorBin).
    %                              Default: 'Color'
    %            'ParAngField'   - Name of the field holding parallactic
    %                              angle. Default: 'ParAng'
    %            'ParAngUnits'   - 'deg' | 'rad'. Default: 'deg'
    %            'AirMassField'  - Name of the field holding airmass
    %                              (== sec z). Default: 'AirMass'
    %            'RequiredFields'- Cell array of field names that must be
    %                              present in FieldStruct.
    %                              Default: {'X','Y'}
    % Output : - Data : struct containing all fields of FieldStruct plus:
    %            .Nepoch, .Nsrc   - array dimensions
    %            .JD              - [Nepoch x 1]
    %            .RefEpoch        - scalar JD used as the time origin
    %            .T               - [Nepoch x 1] = JD - RefEpoch
    %            .SecZ            - [Nepoch x 1] or [Nepoch x Nsrc], from AirMassField (if present)
    %            .ParAngRad       - same shape as ParAngField (if present), in radians
    %            .ColorBin        - [1 x Nsrc] integer bin id per source (if ColorField present)
    %            .FracYear        - [Nepoch x 1] fraction of the calendar year for each JD
    % Author : N. Segev / imUtil.agis rewrite
    % Example: Data = imUtil.agis.buildFitData(S, JD);
    %          Data = imUtil.agis.buildFitData(S, JD, 'ColorBinEdges', -1:0.5:4);

    arguments
        FieldStruct (1,1) struct
        JD (:,1) double
        Args.RefEpoch (1,1) double            = NaN
        Args.ColorBinEdges (1,:) double       = -0.5:0.5:3.5
        Args.ColorField char                  = 'Color'
        Args.ParAngField char                 = 'ParAng'
        Args.ParAngUnits char {mustBeMember(Args.ParAngUnits,{'deg','rad'})} = 'deg'
        Args.AirMassField char                = 'AirMass'
        Args.RequiredFields cell              = {'X','Y'}
    end

    % --- validate required fields exist
    for Ifld = 1:numel(Args.RequiredFields)
        if ~isfield(FieldStruct, Args.RequiredFields{Ifld})
            error('imUtil:agis:buildFitData:missingField', ...
                'Required field "%s" not found in FieldStruct.', Args.RequiredFields{Ifld});
        end
    end

    [Nepoch, Nsrc] = size(FieldStruct.X);

    if numel(JD) ~= Nepoch
        error('imUtil:agis:buildFitData:sizeMismatch', ...
            'numel(JD) (%d) does not match Nepoch inferred from X (%d).', numel(JD), Nepoch);
    end

    % --- validate every field in FieldStruct has consistent [Nepoch x Nsrc] shape
    FN = fieldnames(FieldStruct);
    for Ifld = 1:numel(FN)
        Val = FieldStruct.(FN{Ifld});
        if ~isequal(size(Val), [Nepoch, Nsrc])
            error('imUtil:agis:buildFitData:sizeMismatch', ...
                'Field "%s" has size [%d x %d], expected [%d x %d].', ...
                FN{Ifld}, size(Val,1), size(Val,2), Nepoch, Nsrc);
        end
    end

    % --- start Data as a copy of FieldStruct
    Data = FieldStruct;
    Data.Nepoch = Nepoch;
    Data.Nsrc   = Nsrc;
    Data.JD     = JD;

    % --- reference epoch / time axis
    if isnan(Args.RefEpoch)
        Data.RefEpoch = median(JD, 'omitnan');
    else
        Data.RefEpoch = Args.RefEpoch;
    end
    Data.T = JD - Data.RefEpoch;

    % --- derived: airmass / sec(z)
    if isfield(FieldStruct, Args.AirMassField)
        Data.SecZ = FieldStruct.(Args.AirMassField);
    end

    % --- derived: parallactic angle in radians
    if isfield(FieldStruct, Args.ParAngField)
        if strcmp(Args.ParAngUnits, 'deg')
            Data.ParAngRad = deg2rad(FieldStruct.(Args.ParAngField));
        else
            Data.ParAngRad = FieldStruct.(Args.ParAngField);
        end
    end

    % --- derived: per-source color bin id
    if isfield(FieldStruct, Args.ColorField)
        MedColor = median(FieldStruct.(Args.ColorField), 1, 'omitnan');   % [1 x Nsrc]
        [~, ~, BinId] = histcounts(MedColor, Args.ColorBinEdges);
        BinId(BinId == 0) = 1;   % clip out-of-range sources into the first bin
        Data.ColorBin = BinId;  % [1 x Nsrc]
    end

    % --- derived: fractional year (for annual-effect terms)
    Dt = datetime(JD, 'convertfrom', 'juliandate');
    Yr = year(Dt);
    StartOfYear = datetime(Yr, 1, 1);
    EndOfYear   = datetime(Yr + 1, 1, 1);
    Data.FracYear = days(Dt - StartOfYear) ./ days(EndOfYear - StartOfYear);
end
