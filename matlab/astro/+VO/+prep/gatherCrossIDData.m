function [Data, OutFile] = gatherCrossIDData(T, Cats_cone, Args)
    % Materialize a crossIDCatsHTM index table into a per-source data table.
    %   Given the cross-id table T produced by VO.prep.crossIDCatsHTM, gather
    %   for EACH global source (row of T) the actual column values of its
    %   matched source in each catalog. Two data sources, chosen by 'Source':
    %     'cats'    - read from the Cats_cone snapshot (in-memory AstroCatalogs, or
    %                 the local .mat paths of the CatsToDisk form) using the
    %                 Ind_<Cat> index. Fast, but limited to the columns the
    %                 cone_search returned.
    %     'pointer' - read straight from the on-disk catsHTM store using the
    %                 CellID_<Cat>/RowInCell_<Cat> pointer (via
    %                 catsHTM.gatherByPointer). Needs no Cats_cone and can fetch ANY
    %                 catalog column, at the cost of reading the HDF5 files.
    %   Catalog columns are prefixed by catalog name (e.g. PS1_RA) to avoid
    %   collisions. Rows with no match get FillValue. Optionally writes the
    %   result to a .mat and/or .csv file.
    % Input  : - T: the cross-id result (first output of crossIDCatsHTM) -
    %            either an AstroCatalog or a MATLAB table. For 'cats' it must
    %            contain the Ind_<Cat> columns; for 'pointer' the
    %            CellID_<Cat>/RowInCell_<Cat> columns (crossIDCatsHTM with
    %            AddPointer=true). For an AstroCatalog the text OriginCat column
    %            is absent (it is in Summary.OriginCat) and is simply omitted.
    %          - Cats_cone: the struct of per-catalog catalogs (second output of
    %            crossIDCatsHTM). Each field is either an AstroCatalog or a
    %            char path to a .mat holding it under variable 'Cat' (the
    %            CatsToDisk form) - both are handled. Required for Source='cats';
    %            ignored (may be omitted or []) for Source='pointer'.
    %          * ...,key,val,...
    %            'Source' - 'cats' | 'pointer' | 'auto'. 'auto' (default) picks
    %                   'cats' when Cats_cone is provided (non-empty), else 'pointer'.
    %            'CatList' - Cellstr of catalog names to gather. Default {} =
    %                   all catalogs found as Ind_<Cat> columns in T.
    %            'Columns' - Which catalog columns to pull. Default {} = ALL
    %                   columns of each catalog. A cellstr applies the same
    %                   column list to every catalog (names absent from a given
    %                   catalog are skipped). A struct with fields named after
    %                   catalogs gives a per-catalog cellstr.
    %            'ColPrefix' - Prefix each gathered column name with '<Cat>_'.
    %                   Default is true (recommended; avoids name collisions).
    %            'IncludeGlobal' - Prepend MasterID, RA, Dec, OriginCat from T.
    %                   Default is true.
    %            'FillValue' - Value for rows with no match. Default is NaN.
    %            'CatDir' - Directory holding the catsHTM files, passed through
    %                   to catsHTM.gatherByPointer for Source='pointer'.
    %                   Default '' = resolve via which() on the path.
    %            'OutFile' - Output file base/full name. If non-empty, the
    %                   gathered table is written (see 'OutFormat'). Default ''.
    %            'OutFormat' - Cellstr subset of {'mat','csv'} to write when
    %                   OutFile is set. Default is {'mat'}.
    %            'Verbose' - Print progress. Default is true.
    % Output : - Data: the gathered per-source table (Nglobal rows). Columns:
    %            [MasterID RA Dec OriginCat] then <Cat>_<Col> for every gathered
    %            catalog/column.
    %          - OutFile: cellstr of files actually written (empty if none).
    % See also: VO.prep.crossIDCatsHTM (produces the T / Cats_cone inputs),
    %           catsHTM.gatherByPointer (the Source='pointer' data reader),
    %           catsHTM.sourcePointer, catsHTM.catRowID.
    % Author : Dana Kovaleva (Jul 2026)
    % Example: % Step 1 - build the cross-id index (see VO.prep.crossIDCatsHTM):
    %          [T, Cats_cone] = VO.prep.crossIDCatsHTM(254/(180/pi), 64/(180/pi), 360);
    %          % Step 2a - from the snapshot (default when Cats_cone is given):
    %          D = VO.prep.gatherCrossIDData(T, Cats_cone, 'OutFile','~/tmp/xid_data');
    %          % Step 2b - straight from catsHTM, no Cats_cone needed, any columns:
    %          D = VO.prep.gatherCrossIDData(T, [], 'Source','pointer', ...
    %                 'Columns',struct('GAIADR3',{{'Mag_G'}}, 'PS1',{{'gPSFMag','rPSFMag'}}));

    arguments
        T
        Cats_cone                       = [];
        Args.Source                = 'auto';
        Args.CatList               = {};
        Args.Columns               = {};
        Args.ColPrefix logical     = true;
        Args.IncludeGlobal logical = true;
        Args.FillValue             = NaN;
        Args.CatDir                = '';
        Args.OutFile               = '';
        Args.OutFormat             = {'mat'};
        Args.Verbose logical       = true;
    end

    % resolve the data source ('auto' -> cats if Cats_cone given, else pointer)
    Source = validatestring(Args.Source, {'auto','cats','pointer'});
    if strcmp(Source, 'auto')
        if isempty(Cats_cone)
            Source = 'pointer';
        else
            Source = 'cats';
        end
    end
    if strcmp(Source, 'cats') && isempty(Cats_cone)
        error('gatherCrossIDData:noCats', ...
            'Source=''cats'' requires the Cats_cone struct (2nd argument).');
    end

    if ~istable(T) && ~isa(T, 'AstroCatalog')
        error('gatherCrossIDData:badTable', ...
            'T must be a MATLAB table or an AstroCatalog (crossIDCatsHTM output).');
    end
    % column accessor for either a table or an AstroCatalog. For an
    % AstroCatalog the text OriginCat column is absent (it lives in
    % Summary.OriginCat), so it is simply not gathered here.
    if istable(T)
        Nglobal  = height(T);
        VarNames = T.Properties.VariableNames;
        GetCol   = @(Name) T.(Name);
    else
        Nglobal  = size(T.Catalog, 1);
        VarNames = T.ColNames;
        GetCol   = @(Name) getCol(T, Name);
    end

    % catalogs to gather: from CatList, else every Ind_<Cat> column in T
    if isempty(Args.CatList)
        IsInd   = startsWith(VarNames, 'Ind_');
        CatList = extractAfter(VarNames(IsInd), 'Ind_');
    else
        CatList = Args.CatList;
        if ischar(CatList) || isstring(CatList)
            CatList = cellstr(CatList);
        end
    end

    % ---- global columns ---------------------------------------------------
    VarData = {};
    VarName = {};
    if Args.IncludeGlobal
        for Gc = {'MasterID','RA','Dec','OriginCat'}
            if ismember(Gc{1}, VarNames)
                VarData{end+1} = GetCol(Gc{1}); %#ok<AGROW>
                VarName{end+1} = Gc{1};         %#ok<AGROW>
            end
        end
    end

    % ---- gather each catalog ---------------------------------------------
    for Icat = 1:1:numel(CatList)
        Name = CatList{Icat};
        [Block, ColNm, Msg] = localGatherOne(Source, Name, VarNames, GetCol, ...
            Cats_cone, Args, Nglobal);
        if ~isempty(Msg)
            if Args.Verbose
                fprintf('gatherCrossIDData: %s\n', Msg);
            end
        else
            for Ic = 1:1:numel(ColNm)
                if Args.ColPrefix
                    ThisName = [Name '_' ColNm{Ic}];
                else
                    ThisName = ColNm{Ic};
                end
                VarData{end+1} = Block(:, Ic);              %#ok<AGROW>
                VarName{end+1} = ThisName;                  %#ok<AGROW>
            end
            if Args.Verbose
                fprintf('gatherCrossIDData: %s - %d columns [%s]\n', Name, numel(ColNm), Source);
            end
        end
    end

    % ensure valid, unique table variable names
    VarName  = matlab.lang.makeValidName(VarName);
    VarName  = matlab.lang.makeUniqueStrings(VarName, {}, namelengthmax);
    Data     = table(VarData{:}, 'VariableNames', VarName);

    % ---- optional file output --------------------------------------------
    OutFile = {};
    if ~isempty(Args.OutFile)
        OutFile = localWriteData(Args.OutFile, Args.OutFormat, Data, Args.Verbose);
    end
end

% ======================================================================
function [Block, ColNm, Msg] = localGatherOne(Source, Name, VarNames, GetCol, Cats_cone, Args, Nglobal)
    % Gather one catalog's columns from either the Cats_cone snapshot (Ind_<Cat>)
    % or the catsHTM store (CellID_<Cat>/RowInCell_<Cat>). Returns a non-empty
    % Msg when the catalog is skipped (missing columns / not in Cats_cone).
    Block = []; ColNm = {}; Msg = '';
    if strcmp(Source, 'cats')
        IndName = ['Ind_' Name];
        if ~ismember(IndName, VarNames)
            Msg = sprintf('no %s column in T; skipping %s', IndName, Name);
            return;
        end
        if ~isfield(Cats_cone, Name)
            Msg = sprintf('%s not in Cats_cone; skipping', Name);
            return;
        end
        Cat = localGetCat(Cats_cone.(Name));
        Ind = GetCol(IndName);
        [ColIdx, ColNm] = localResolveColumns(Cat, Args.Columns, Name);
        Block = repmat(Args.FillValue, Nglobal, numel(ColIdx));
        Ok    = ~isnan(Ind);
        if any(Ok) && ~isempty(ColIdx)
            Block(Ok, :) = Cat.Catalog(Ind(Ok), ColIdx);
        end
    else   % 'pointer'
        CidName = ['CellID_' Name];
        RicName = ['RowInCell_' Name];
        if ~ismember(CidName, VarNames) || ~ismember(RicName, VarNames)
            Msg = sprintf(['no %s/%s columns in T (run crossIDCatsHTM with ', ...
                'AddPointer); skipping %s'], CidName, RicName, Name);
            return;
        end
        Cid    = GetCol(CidName);
        Ric    = GetCol(RicName);
        Wanted = localWantedColumns(Args.Columns, Name);
        [Block, ColNm] = catsHTM.gatherByPointer(Name, Cid, Ric, ...
            'Columns', Wanted, 'CatDir', Args.CatDir, 'FillValue', Args.FillValue);
        ColNm = ColNm(:).';
    end
end

% ======================================================================
function Cat = localGetCat(Entry)
    % Return an AstroCatalog from either an object or a .mat path.
    if ischar(Entry) || isstring(Entry)
        L   = load(char(Entry));
        Cat = L.Cat;
    else
        Cat = Entry;
    end
end

% ======================================================================
function [ColIdx, ColNm] = localResolveColumns(Cat, Columns, Name)
    % Resolve the column indices/names to pull from a Cats_cone catalog object.
    AllNames = Cat.ColNames;
    Wanted   = localWantedColumns(Columns, Name);
    if isempty(Wanted)
        Wanted = AllNames;                       % all columns
    end
    % keep only columns that exist in this catalog, preserving requested order
    [Tf, Loc] = ismember(Wanted, AllNames);
    ColIdx    = Loc(Tf);
    ColNm     = AllNames(ColIdx);
    ColIdx    = ColIdx(:).';
    ColNm     = ColNm(:).';
end

% ======================================================================
function Wanted = localWantedColumns(Columns, Name)
    % The requested column list for a given catalog (cellstr, or {} = all).
    if isempty(Columns)
        Wanted = {};
    elseif isstruct(Columns)
        if isfield(Columns, Name)
            Wanted = Columns.(Name);
        else
            Wanted = {};
        end
    else
        Wanted = Columns;
    end
    if ischar(Wanted) || isstring(Wanted)
        Wanted = cellstr(Wanted);
    end
end

% ======================================================================
function Written = localWriteData(OutFile, Formats, Data, Verbose)
    % Write the gathered table to .mat and/or .csv (stem-shared).
    if ischar(Formats) || isstring(Formats)
        Formats = cellstr(Formats);
    end
    [Path, Base] = fileparts(OutFile);
    if isempty(Path)
        Path = pwd;
    end
    Stem    = fullfile(Path, Base);
    Written = {};
    if any(strcmpi(Formats, 'mat'))
        MatFile = [Stem '.mat'];
        save(MatFile, 'Data', '-v7.3');
        Written{end+1} = MatFile;
        if Verbose
            fprintf('gatherCrossIDData: wrote %s\n', MatFile);
        end
    end
    if any(strcmpi(Formats, 'csv'))
        CsvFile = [Stem '.csv'];
        writetable(Data, CsvFile);
        Written{end+1} = CsvFile;
        if Verbose
            fprintf('gatherCrossIDData: wrote %s\n', CsvFile);
        end
    end
end
