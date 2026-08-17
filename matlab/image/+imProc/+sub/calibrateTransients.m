function Obj = calibrateTransients(Obj, Args)
    %{
    Calibrate transient candidates using absolute photometry of the New image.

    Input   : - An AstroDiff object in which New, Ref and CatData are
                populated.
              * ...,key,val,...
                'ConfigFile' - Path to JSON configuration file. Fields in
                       the file override corresponding Args fields.
                       Default is ''.

                'FlagCol' - Name of the transient-filter flag column.
                       Candidates with this column value < 1 are treated as
                       transients.
                       Default is 'FLAGS_TRANSIENT'.

                'ScoreCol' - Existing column to update with the local S
                       statistic from the re-subtraction.
                       Default is 'SCORE'.

                'ScorrCol' - Existing column to update with Scorr from the
                       re-subtraction.
                       Default is 'S_CORR'.

                'FluxCol' - Existing column to update with PSF flux
                       measured on the local re-subtraction.
                       Default is 'FLUX_PSF'.

                'FluxErrCol' - Existing column to update with PSF flux
                       uncertainty.
                       Default is 'FLUXERR_PSF'.

                'MagCol' - Existing column to update with PSF magnitude.
                       Default is 'MAG_PSF'.

                'MagErrCol' - Existing column to update with PSF magnitude
                       uncertainty.
                       Default is 'MAGERR_PSF'.

                'CropHalfSize' - Half-size of the local New/Ref cutout used
                       for re-subtraction.
                       Default is 100.

                'BoxHalfSize' - Half-size of the box used to search for the
                       local maximum S pixel around the candidate.
                       Default is 2.

                'MinAperCorrStars' - Minimum number of aperture-correction
                       stars required for accepting the absolute photometry.
                       Default is 10.

                'PreferredAperCorrCol' - Preferred aperture-correction
                       column used to verify the photometric solution.
                       Default is "MAG_PSF".

                'N_AV_EXPTIME' - Number of averaged New-image exposures.
                       Used to convert the per-exposure calibration to the
                       coadd zeropoint.
                       Default is 20.

    Output  : - The input AstroDiff object, with existing CatData columns
                updated only for candidates where FLAGS_TRANSIENT < 1.
                No new columns are inserted.

    Author  : Ruslan Konno
    Example : AD = imProc.sub.calibrateTransients(AD);
    %}

    arguments
        Obj AstroDiff

        % General
        Args.ConfigFile char = ''

        % Candidate selection
        Args.FlagCol char = 'FLAGS_TRANSIENT'

        % Existing output columns
        Args.ScoreCol char = ''
        Args.ScorrCol char = ''

        Args.FluxCol char = 'FLUX_PSF'
        Args.FluxErrCol char = 'FLUXERR_PSF'
        Args.MagCol char = 'MAG_PSF'
        Args.MagErrCol char = 'MAGERR_PSF'

        Args.ZPCol char = 'ZP'
        Args.NZPCol char = 'N_ZP'
        Args.RZPCol char = 'R_ZP'

        % Local subtraction
        Args.CropHalfSize double = 100
        Args.StampHalfSize double = 25
        Args.BoxHalfSize double = 2

        % Absolute photometry
        Args.MinAperCorrStars double = 10
        Args.PreferredAperCorrCol string = "MAG_PSF"
        Args.N_AV_EXPTIME double = 20
        Args.R_AV_EXPTIME double = 20
    end

    Nobj = numel(Obj);

    for Iobj=Nobj:-1:1

        CandCat = Obj(Iobj).CatData;
        NumCand = size(CandCat.Catalog, 1);

        % Skip empty catalogs
        if NumCand < 1
            continue
        end

        if ~CandCat.isColumn(Args.FlagCol)
            continue
        end

        Flags = CandCat.getCol(Args.FlagCol);
        Transients = (Flags < 1.0);

        if ~any(Transients)
            continue
        end

        TranInd = find(Transients);

        % Candidate positions in the registered subtraction/New grid.
        [X, Y] = CandCat.getXY();

        AbsPhotOK_New = isAbsPhotOK(Obj(Iobj).PC_New, ...
            'MinAperCorrStars', Args.MinAperCorrStars, ...
            'PreferredAperCorrCol', Args.PreferredAperCorrCol);
        
        AbsPhotOK_Ref = isAbsPhotOK(Obj(Iobj).PC_Ref, ...
            'MinAperCorrStars', Args.MinAperCorrStars, ...
            'PreferredAperCorrCol', Args.PreferredAperCorrCol);
        
        if ~AbsPhotOK_New || ~AbsPhotOK_Ref
            continue
        end

        % Read existing columns. Columns that do not already exist are
        % ignored and will not be created.
        Out = getExistingColumns(CandCat, Args);

        for Itran=numel(TranInd):-1:1

            Irow = TranInd(Itran);

            X_New = X(Irow);
            Y_New = Y(Irow);

            if ~isfinite(X_New) || ~isfinite(Y_New)
                continue
            end

            try
                WorkAD = AstroZOGY(Obj(Iobj).New.copy, Obj(Iobj).Ref.copy);
            
                Result = photTransientFromCalibratedImages( ...
                    WorkAD, X_New, Y_New, Obj(Iobj).PC_New, Obj(Iobj).PC_Ref, Args);
            
                Out = updateExistingColumns(Out, Irow, Result);
            
            catch ME
                warning('calibrateTransients:TransientPhotFailed', ...
                    'Photometry failed for Obj(%d), row %d: %s', ...
                    Iobj, Irow, ME.message);
                continue
            end
        end

        CandCat = replaceExistingColumns(CandCat, Out);
        Obj(Iobj).CatData = CandCat;
    end
end

function Result = photTransientFromCalibratedImages(AD, X_New, Y_New, PC_New, PC_Ref, Args)
%{
    Re-subtract a local cutout and measure forced PSF photometry.

    Input   : - AstroDiff object.
              - Target X position on the registered New-image grid.
              - Target Y position on the registered New-image grid.
              - Photometric calibration object for the New image.
              - Logical flag indicating whether the New-image calibration
                passed quality checks.
              - Args structure.

    Output  : - Structure with local subtraction statistics and PSF
                photometry results.

    Description : This function follows the same local re-subtraction logic
                  used in the forced absolute photometry script. The New
                  image PH_ZP is replaced by the position-dependent absolute
                  zeropoint if the calibration is valid. The Ref image is
                  not recalibrated here.

    Author  : Ruslan Konno
    %}

    Result = makeNanResult();

    ZP_New = PC_New.evaluateZP('X', X_New, 'Y', Y_New) ...
        + 2.5.*log10(Args.N_AV_EXPTIME);

    ZP_Ref = PC_Ref.evaluateZP('X', X_New, 'Y', Y_New) ...
        + 2.5.*log10(Args.R_AV_EXPTIME);

    setHeaderKey(AD.New.HeaderData, 'PH_ZP', ZP_New);
    setHeaderKey(AD.Ref.HeaderData, 'PH_ZP', ZP_Ref);

    AD.estimateFnFr;

    [Ny_New, Nx_New] = getAstroImageSize(AD.New);

    [XMin, XMax, YMin, YMax, X0Crop, Y0Crop] = ...
        makeEdgeSafeCropBox(X_New, Y_New, Nx_New, Ny_New, Args.CropHalfSize);

    AD.New = AD.New.crop([XMin XMax YMin YMax]);
    AD.Ref = AD.Ref.crop([XMin XMax YMin YMax]);

    X0 = round(X0Crop);
    Y0 = round(Y0Crop);

    AD.subtractionD;
    AD.subtractionS;
    AD.subtractionScorr;

    [S, S_MaxX, S_MaxY] = getLocalMaxS(AD.S, X0, Y0, Args.BoxHalfSize);

    Scorr = getImageValueAtXY(AD.Scorr, S_MaxX, S_MaxY);

    try
        PSFSize = floor(size(AD.PSFData.getPSF, 2)./2);

        [Cube, ~, ~, ~, ~] = imUtil.cut.image2cutouts( ...
            AD.Dbs, X0, Y0, PSFSize);

        [Result, ~] = imUtil.sources.psfPhotCube(Cube, ...
            'PSF', AD.PSFData.getPSF, ...
            'ZP', AD.HeaderData.getVal('PH_ZP'));

        Result.FluxErr = sqrt(abs(Result.Flux));
        Result.MagErr = 1.086 ./ Result.FluxErr;

    catch ME
        warning('calibrateTransients:PSFPhotFailed', ...
            'PSF forced photometry failed: %s', ME.message);

        Result = makeNanResult();
    end

    Result.S = S;
    Result.Scorr = Scorr;
    Result.ZP_New = ZP_New;
    Result.ZP_Ref = ZP_Ref;

end

function Out = getExistingColumns(CandCat, Args)
    %{
    Read existing output columns from CatData.
    %}

    Out = struct();

    Out.Score   = getExistingColumn(CandCat, Args.ScoreCol);
    Out.Scorr   = getExistingColumn(CandCat, Args.ScorrCol);

    Out.Flux    = getExistingColumn(CandCat, Args.FluxCol);
    Out.FluxErr = getExistingColumn(CandCat, Args.FluxErrCol);
    Out.Mag     = getExistingColumn(CandCat, Args.MagCol);
    Out.MagErr  = getExistingColumn(CandCat, Args.MagErrCol);

    Out.ZP      = getExistingColumn(CandCat, Args.ZPCol);
    Out.N_ZP    = getExistingColumn(CandCat, Args.NZPCol);
    Out.R_ZP    = getExistingColumn(CandCat, Args.RZPCol);
end

function Col = getExistingColumn(CandCat, ColName)
    %{
    Read a single existing column.

    Input   : - AstroCatalog.
              - Column name.

    Output  : - Structure with fields Name, Exists and Data.

    Author  : Ruslan Konno
    %}

    Col = struct();
    Col.Name = ColName;
    Col.Exists = false;
    Col.Data = [];

    if strlength(string(ColName)) == 0
        return
    end

    if ~CandCat.isColumn(ColName)
        return
    end

    Col.Data = CandCat.getCol(ColName);
    Col.Exists = true;
end

function Out = updateExistingColumns(Out, Irow, Result)
    %{
    Update existing output columns for one candidate row.
    %}

    Out = updateOneColumn(Out, 'Score',   Irow, Result, 'S');
    Out = updateOneColumn(Out, 'Scorr',   Irow, Result, 'Scorr');

    Out = updateOneColumn(Out, 'Flux',    Irow, Result, 'Flux');
    Out = updateOneColumn(Out, 'FluxErr', Irow, Result, 'FluxErr');
    Out = updateOneColumn(Out, 'Mag',     Irow, Result, 'Mag');
    Out = updateOneColumn(Out, 'MagErr',  Irow, Result, 'MagErr');

    Out = updateOneColumn(Out, 'ZP',   Irow, Result, 'ZP_New');
    Out = updateOneColumn(Out, 'N_ZP', Irow, Result, 'ZP_New');
    Out = updateOneColumn(Out, 'R_ZP', Irow, Result, 'ZP_Ref');
end

function Out = updateOneColumn(Out, OutField, Irow, Result, ResultField)
    %{
    Update one output column if both the column and result field exist.

    Input   : - Output-column structure.
              - Name of the output-column field.
              - Candidate row index.
              - Result structure.
              - Name of the result field.

    Output  : - Updated output-column structure.

    Author  : Ruslan Konno
    %}

    if ~Out.(OutField).Exists
        return
    end

    if ~isfield(Result, ResultField)
        return
    end

    Out.(OutField).Data(Irow) = Result.(ResultField);
end

function CandCat = replaceExistingColumns(CandCat, Out)
    %{
    Replace existing CatData columns.

    Input   : - AstroCatalog.
              - Output-column structure.

    Output  : - AstroCatalog with selected existing columns replaced.

    Description : This function does not insert columns. It only replaces
                  columns that were found by getExistingColumns.

    Author  : Ruslan Konno
    %}

    Fields = fieldnames(Out);

    for IField=numel(Fields):-1:1

        Field = Fields{IField};

        if ~Out.(Field).Exists
            continue
        end

        CandCat = CandCat.replaceCol(Out.(Field).Data, Out.(Field).Name);
    end
end

function AbsPhotOK = isAbsPhotOK(PC, Args)
    %{
    Check whether an absolute photometric calibration is usable.

    Input   : - Photometric calibration object.
              * ...,key,val,...
                'MinAperCorrStars' - Minimum number of aperture-correction
                       stars.
                       Default is 10.

                'PreferredAperCorrCol' - Preferred aperture-correction
                       column.
                       Default is "MAG_PSF".

    Output  : - Logical flag indicating whether the calibration is usable.

    Author  : Ruslan Konno
    %}

    arguments
        PC
        Args.MinAperCorrStars (1,1) double = 10
        Args.PreferredAperCorrCol (1,1) string = "MAG_PSF"
    end

    AbsPhotOK = false;

    if isempty(PC)
        return
    end

    if ~isprop(PC, 'CalFound') || ~PC.CalFound
        return
    end

    if ~isprop(PC, 'AperCorrNStars') || ...
            ~isfinite(PC.AperCorrNStars) || ...
            PC.AperCorrNStars < Args.MinAperCorrStars
        return
    end

    if ~isprop(PC, 'AperCorr') || isempty(PC.AperCorr)
        return
    end

    if ~isprop(PC, 'AperCorrColNames') || isempty(PC.AperCorrColNames)
        return
    end

    AperCorr = PC.AperCorr;
    ColNames = string(PC.AperCorrColNames);

    ICol = find(ColNames == Args.PreferredAperCorrCol, 1);

    if isempty(ICol)
        ICol = find(contains(ColNames, "PSF"), 1);
    end

    if isempty(ICol)
        return
    end

    AbsPhotOK = isfinite(AperCorr(ICol));
end

function [S, S_MaxX, S_MaxY] = getLocalMaxS(SImage, X0, Y0, BoxHalfSize)
    %{
    Get maximum S value in a local box around a candidate.

    Input   : - S-statistic image.
              - Candidate X position in the cropped image.
              - Candidate Y position in the cropped image.
              - Half-size of local search box.

    Output  : - Maximum S value.
              - X position of maximum S.
              - Y position of maximum S.

    Author  : Ruslan Konno
    %}

    S = nan;
    S_MaxX = nan;
    S_MaxY = nan;

    if isempty(SImage)
        return
    end

    if Y0 < 1 || Y0 > size(SImage, 1) || X0 < 1 || X0 > size(SImage, 2)
        return
    end

    XInd = max(1, X0 - BoxHalfSize):min(size(SImage, 2), X0 + BoxHalfSize);
    YInd = max(1, Y0 - BoxHalfSize):min(size(SImage, 1), Y0 + BoxHalfSize);

    SPatch = SImage(YInd, XInd);

    if ~any(isfinite(SPatch(:)))
        return
    end

    [S, IMax] = max(SPatch(:), [], 'omitnan');

    [ILocalY, ILocalX] = ind2sub(size(SPatch), IMax);

    S_MaxX = XInd(ILocalX);
    S_MaxY = YInd(ILocalY);
end

function Val = getImageValueAtXY(Image, X, Y)
    %{
    Get an image value at integer X,Y.

    Input   : - Image.
              - X coordinate.
              - Y coordinate.

    Output  : - Image value, or NaN if unavailable.

    Author  : Ruslan Konno
    %}

    Val = nan;

    if isempty(Image)
        return
    end

    if ~isfinite(X) || ~isfinite(Y)
        return
    end

    if Y < 1 || Y > size(Image, 1) || X < 1 || X > size(Image, 2)
        return
    end

    Val = Image(Y, X);
end

function [XMin, XMax, YMin, YMax, X0Crop, Y0Crop] = makeEdgeSafeCropBox(X, Y, Nx, Ny, HalfSize)
    %{
    Construct an edge-safe crop box around an image position.

    Input   : - X coordinate.
              - Y coordinate.
              - Image X size.
              - Image Y size.
              - Crop half-size.

    Output  : - Minimum X pixel.
              - Maximum X pixel.
              - Minimum Y pixel.
              - Maximum Y pixel.
              - Target X coordinate in the cropped image.
              - Target Y coordinate in the cropped image.

    Author  : Ruslan Konno
    %}

    XCenter = round(X);
    YCenter = round(Y);

    XMin = max(1, XCenter - HalfSize);
    XMax = min(Nx, XCenter + HalfSize);

    YMin = max(1, YCenter - HalfSize);
    YMax = min(Ny, YCenter + HalfSize);

    X0Crop = X - XMin + 1;
    Y0Crop = Y - YMin + 1;
end

function [Ny, Nx] = getAstroImageSize(AI)
    %{
    Get the image size from an AstroImage-like object.

    Input   : - AstroImage-like object.

    Output  : - Number of rows.
              - Number of columns.

    Author  : Ruslan Konno
    %}

    if isprop(AI, 'Image') && ~isempty(AI.Image)
        ImageSize = size(AI.Image);

    elseif isprop(AI, 'ImageData') && isprop(AI.ImageData, 'Image') && ~isempty(AI.ImageData.Image)
        ImageSize = size(AI.ImageData.Image);

    elseif isprop(AI, 'ImageData') && isfield(AI.ImageData, 'Image') && ~isempty(AI.ImageData.Image)
        ImageSize = size(AI.ImageData.Image);

    elseif isprop(AI, 'Data') && ~isempty(AI.Data)
        ImageSize = size(AI.Data);

    else
        error('calibrateTransients:UnknownImageSize', ...
            'Could not determine AstroImage image size.');
    end

    Ny = ImageSize(1);
    Nx = ImageSize(2);
end

function Result = makeNanResult()
    %{
    Construct a NaN photometry result.

    Output  : - Result structure with NaN photometry fields.

    Author  : Ruslan Konno
    %}

    Result = struct();

    Result.S = nan;
    Result.Scorr = nan;

    Result.Flux = nan;
    Result.FluxErr = nan;
    Result.Mag = nan;
    Result.MagErr = nan;

    Result.ZP_New = nan;
    Result.ZP_Ref = nan;
end

function setHeaderKey(HeaderData, Key, Val)
    %{
    Replace a header key with a new value.

    Input   : - HeaderData object.
              - Header key.
              - Header value.

    Author  : Ruslan Konno
    %}

    try
        HeaderData.deleteKey(Key);
    catch
    end

    HeaderData.insertKey({Key, Val});
end