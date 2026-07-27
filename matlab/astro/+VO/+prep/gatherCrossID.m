function [Data, OutFile] = gatherCrossID(T, Cats, Args)
    % Materialize a crossIDCatsHTM index table into a per-source data table.
    %   Given the cross-id table T and the per-catalog catalogs Cats produced
    %   by VO.prep.crossIDCatsHTM, gather for EACH global source (row of T)
    %   the actual column values of its matched source in each catalog (via
    %   the Ind_<Cat> index), NaN where there is no match. Catalog columns are
    %   prefixed by catalog name (e.g. PS1_RA) to avoid collisions. Optionally
    %   writes the result to a .mat and/or .csv file.
    % Input  : - T: the cross-id result (first output of crossIDCatsHTM) -
    %            either an AstroCatalog (default output) or a MATLAB table.
    %            Must contain the Ind_<Cat> columns. For an AstroCatalog the
    %            text OriginCat column is absent (it is in Summary.OriginCat)
    %            and is simply omitted from the gathered output.
    %          - Cats: the struct of per-catalog catalogs (second output of
    %            crossIDCatsHTM). Each field is either an AstroCatalog or a
    %            char path to a .mat holding it under variable 'Cat' (the
    %            CatsToDisk form) - both are handled.
    %          * ...,key,val,...
    %            'CatList' - Cellstr of catalog names to gather. Default {} =
    %                   all catalogs found as Ind_<Cat> columns in T.
    %            'Columns' - Which catalog columns to pull. Default {} = ALL
    %                   columns of each catalog. A cellstr applies the same
    %                   column list to every catalog (names absent from a given
    %                   catalog are skipped). A struct with fields named after
    %                   catalogs gives a per-catalog cellstr.
    %            'ColPrefix' - Prefix each gathered column name with '<Cat>_'.
    %                   Default is true (recommended; avoids name collisions).
    %            'IncludeGlobal' - Prepend GlobalID, RA, Dec, OriginCat from T.
    %                   Default is true.
    %            'FillValue' - Value for rows with no match. Default is NaN.
    %            'OutFile' - Output file base/full name. If non-empty, the
    %                   gathered table is written (see 'OutFormat'). Default ''.
    %            'OutFormat' - Cellstr subset of {'mat','csv'} to write when
    %                   OutFile is set. Default is {'mat'}.
    %            'Verbose' - Print progress. Default is true.
    % Output : - Data: the gathered per-source table (Nglobal rows). Columns:
    %            [GlobalID RA Dec OriginCat] then <Cat>_<Col> for every gathered
    %            catalog/column.
    %          - OutFile: cellstr of files actually written (empty if none).
    % Author : Dana Kovaleva (Jul 2026)
    % Example: [T, Cats] = VO.prep.crossIDCatsHTM(254/(180/pi), 64/(180/pi), 360);
    %          % gather everything:
    %          D = VO.prep.gatherCrossID(T, Cats, 'OutFile','~/tmp/xid_data');
    %          % only Gaia G-mag and PS1 g/r, written to CSV:
    %          D = VO.prep.gatherCrossID(T, Cats, ...
    %                 'CatList',{'GAIADR3','PS1'}, ...
    %                 'Columns',struct('GAIADR3',{{'Mag_G'}}, 'PS1',{{'gPSFMag','rPSFMag'}}), ...
    %                 'OutFile','~/tmp/xid_phot', 'OutFormat',{'csv'});

    arguments
        T
        Cats
        Args.CatList             = {};
        Args.Columns             = {};
        Args.ColPrefix logical   = true;
        Args.IncludeGlobal logical = true;
        Args.FillValue           = NaN;
        Args.OutFile             = '';
        Args.OutFormat           = {'mat'};
        Args.Verbose logical     = true;
    end

    if ~istable(T) && ~isa(T, 'AstroCatalog')
        error('gatherCrossID:badTable', ...
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
        for Gc = {'GlobalID','RA','Dec','OriginCat'}
            if ismember(Gc{1}, VarNames)
                VarData{end+1} = GetCol(Gc{1}); %#ok<AGROW>
                VarName{end+1} = Gc{1};         %#ok<AGROW>
            end
        end
    end

    % ---- gather each catalog ---------------------------------------------
    for Icat = 1:1:numel(CatList)
        Name    = CatList{Icat};
        IndName = ['Ind_' Name];
        if ~ismember(IndName, VarNames)
            if Args.Verbose
                fprintf('gatherCrossID: no %s column in T; skipping %s\n', IndName, Name);
            end
        elseif ~isfield(Cats, Name)
            if Args.Verbose
                fprintf('gatherCrossID: %s not in Cats; skipping\n', Name);
            end
        else
            Cat  = localGetCat(Cats.(Name));
            Ind  = GetCol(IndName);
            [ColIdx, ColNm] = localResolveColumns(Cat, Args.Columns, Name);

            Block = repmat(Args.FillValue, Nglobal, numel(ColIdx));
            Ok    = ~isnan(Ind);
            if any(Ok) && ~isempty(ColIdx)
                Block(Ok, :) = Cat.Catalog(Ind(Ok), ColIdx);
            end

            for Ic = 1:1:numel(ColIdx)
                if Args.ColPrefix
                    ThisName = [Name '_' ColNm{Ic}];
                else
                    ThisName = ColNm{Ic};
                end
                VarData{end+1} = Block(:, Ic);              %#ok<AGROW>
                VarName{end+1} = ThisName;                  %#ok<AGROW>
            end
            if Args.Verbose
                fprintf('gatherCrossID: %s - %d columns, %d matched rows\n', ...
                    Name, numel(ColIdx), nnz(Ok));
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
    % Resolve the column indices/names to pull from Cat.
    AllNames = Cat.ColNames;
    if isempty(Columns)
        Wanted = AllNames;                       % all columns
    elseif isstruct(Columns)
        if isfield(Columns, Name)
            Wanted = Columns.(Name);
        else
            Wanted = AllNames;                   % no per-cat entry -> all
        end
    else
        Wanted = Columns;                        % same list for every catalog
    end
    if ischar(Wanted) || isstring(Wanted)
        Wanted = cellstr(Wanted);
    end
    % keep only columns that exist in this catalog, preserving requested order
    [Tf, Loc] = ismember(Wanted, AllNames);
    ColIdx    = Loc(Tf);
    ColNm     = AllNames(ColIdx);
    ColIdx    = ColIdx(:).';
    ColNm     = ColNm(:).';
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
            fprintf('gatherCrossID: wrote %s\n', MatFile);
        end
    end
    if any(strcmpi(Formats, 'csv'))
        CsvFile = [Stem '.csv'];
        writetable(Data, CsvFile);
        Written{end+1} = CsvFile;
        if Verbose
            fprintf('gatherCrossID: wrote %s\n', CsvFile);
        end
    end
end
