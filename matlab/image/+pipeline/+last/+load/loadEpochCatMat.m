function [Cat, Head, Info] = loadEpochCatMat(VisitDir, Args)
    % Load the epoch catalogs and headers of a LAST visit from its proc_Cat/proc_Head MAT files
    %   pipelineI can write the epoch catalogs of a visit as two MAT files
    %   instead of one proc_Cat FITS file per epoch and crop (issue #1417):
    %   *_sci_proc_Cat_1.mat (variable ProcCat) and *_sci_proc_Head_1.mat
    %   (variable ProcHead), in either the "object" form (AstroCatalog and
    %   AstroHeader arrays) or the "plain" form (struct array with fields
    %   Catalog, ColNames, ColUnits, and a cell array of N x 3 header cells),
    %   see pipeline.last.pipes.PipelineDemon/saveEpochCatMat.
    %   The output grid has one row per epoch and one column per crop,
    %   sorted by the epoch counter and the crop ID taken from the FILENAME
    %   keyword of each header (the name the proc_Cat FITS file would have).
    %   The header file is always read, since it gives the grid positions.
    % Input  : - Visit directory. Default is the current directory.
    %          * ...,key,val,...
    %            'Read' - "both" | "head". With "head" the catalog file is
    %                   not read and Cat is empty. Default is "both".
    %            'Counter' - Keep only these epoch counters. [] for all.
    %                   Default is [].
    %            'CropID' - Keep only these crop IDs. [] for all.
    %                   Default is [].
    %            'CatTemplate' - File template of the catalog MAT file.
    %                   Default is '*_sci_proc_Cat_1.mat'.
    %            'HeadTemplate' - File template of the header MAT file.
    %                   Default is '*_sci_proc_Head_1.mat'.
    % Output : - Nepoch x Ncrop AstroCatalog ([] with 'Read'="head").
    %          - Nepoch x Ncrop AstroHeader.
    %          - Structure with fields:
    %            .CatFile, .HeadFile - Full names of the MAT files.
    %            .Counter - Nepoch x 1 epoch counters of the rows.
    %            .CropID  - 1 x Ncrop crop IDs of the columns.
    %            .FileName - Nepoch x Ncrop string array of the FILENAME
    %                   keywords (names of the equivalent proc_Cat FITS files).
    % Author : Alexander Krassilchtchikov (Oct 2026)
    % Example: [Cat, Head, Info] = pipeline.last.load.loadEpochCatMat('/marvin/LAST.01.01.02/2026/10/19/proc/001037v1');
    %          [~, Head] = pipeline.last.load.loadEpochCatMat(VisitDir, 'Read',"head");

    arguments
        VisitDir             = pwd;
        Args.Read            = "both";
        Args.Counter         = [];
        Args.CropID          = [];
        Args.CatTemplate     = '*_sci_proc_Cat_1.mat';
        Args.HeadTemplate    = '*_sci_proc_Head_1.mat';
    end

    Info.HeadFile = oneFile(VisitDir, Args.HeadTemplate);
    S    = load(Info.HeadFile, 'ProcHead');
    Head = toHeader(S.ProcHead);

    % grid positions from the FILENAME keyword
    FileName = strings(size(Head));
    for I=1:1:numel(Head)
        FileName(I) = string(Head(I).getVal('FILENAME'));
    end
    AFN     = AstroFileName(FileName(:));
    Counter = reshape(str2double(string(AFN.Counter)), size(Head));
    CropID  = reshape(str2double(string(AFN.CropID)),  size(Head));
    if any(Counter ~= Counter(:,1), 'all') || any(CropID ~= CropID(1,:), 'all')
        error('loadEpochCatMat:Grid', 'The FILENAME keywords of %s do not form an epoch x crop grid', Info.HeadFile);
    end
    Info.Counter = Counter(:,1);
    Info.CropID  = CropID(1,:);

    % rows and columns to keep, sorted
    [Info.Counter, Irow] = sort(Info.Counter);
    [Info.CropID,  Icol] = sort(Info.CropID);
    if ~isempty(Args.Counter)
        Keep = ismember(Info.Counter, Args.Counter);
        Irow = Irow(Keep);
        Info.Counter = Info.Counter(Keep);
    end
    if ~isempty(Args.CropID)
        Keep = ismember(Info.CropID, Args.CropID);
        Icol = Icol(Keep);
        Info.CropID = Info.CropID(Keep);
    end
    Head = Head(Irow, Icol);
    Info.FileName = FileName(Irow, Icol);

    if strcmpi(Args.Read, "head")
        Info.CatFile = "";
        Cat = [];
    else
        Info.CatFile = oneFile(VisitDir, Args.CatTemplate);
        S   = load(Info.CatFile, 'ProcCat');
        if ~isequal(size(S.ProcCat), size(CropID))
            error('loadEpochCatMat:Size', 'The grids of %s and %s differ in size', Info.CatFile, Info.HeadFile);
        end
        Cat = toCatalog(S.ProcCat(Irow, Icol));
    end
end

% internal functions

function File = oneFile(VisitDir, Template)
    % Full name of the single file matching the template in the visit directory
    List = dir(fullfile(VisitDir, Template));
    if numel(List) ~= 1
        error('loadEpochCatMat:File', 'Found %d files %s in %s', numel(List), Template, VisitDir);
    end
    File = string(fullfile(List.folder, List.name));
end

function Head = toHeader(ProcHead)
    % AstroHeader array from the object or plain form
    if isa(ProcHead, 'AstroHeader')
        Head = ProcHead;
    else
        Head = AstroHeader(size(ProcHead));
        for I=1:1:numel(ProcHead)
            Head(I).Data = ProcHead{I};
        end
    end
end

function Cat = toCatalog(ProcCat)
    % AstroCatalog array from the object or plain form
    if isa(ProcCat, 'AstroCatalog')
        Cat = ProcCat;
    elseif isempty(ProcCat)
        Cat = AstroCatalog.empty(size(ProcCat));
    else
        Cat = AstroCatalog(size(ProcCat));
        for I=1:1:numel(ProcCat)
            Cat(I).ColNames = ProcCat(I).ColNames;
            Cat(I).ColUnits = ProcCat(I).ColUnits;
            Cat(I).Catalog  = ProcCat(I).Catalog;
        end
    end
end
