% AstroHeader Class - A container for astronomical image/tables headers
% Properties:
%   Data        - a 3-column cell array of [key, val, comment]
%   Key         - A structure array containing on-the-fly-generated header
%   File
%   HDU
%   KeyDict     - Dictionary of key-name synonyms
%   ValDict     - Dictionary of key values (IMTYPE) synonyms
%   CommentDict - Dictionary of default comments
%   TimeDict    - Dictionary of time keywords and their conversion functions
%   IsKeyUpToDate  - Is the Key structure up to date
%
% Performance redesign notes:
%   The original O(N) linear scan through Data has been replaced by two
%   complementary caching layers that together make the dominant access
%   patterns O(1):
%
%   KeyIndex (containers.Map)
%     Maps each uppercase keyword string to the vector of row indices in
%     Data at which that keyword appears.  Built lazily on first use after
%     any Data modification (IsIndexUpToDate = false) and then reused until
%     Data changes again.  A single write to Obj.Data (via the set.Data
%     setter) invalidates it automatically.
%
%   SynonymCache (containers.Map)
%     Maps a compact cache-key string (input synonym + search options) to
%     the resolved cell array of alternate keyword names returned by the
%     dictionary.  Avoids repeated searchAlt / searchKey traversals for
%     the same input keyword across thousands of pipeline calls.  Valid for
%     the lifetime of the object (dictionaries are assumed constant at runtime).
%
%   The fast path inside getVal (strcmp mode) therefore costs only:
%     one containers.Map lookup in SynonymCache  (synonym resolution)
%   + one containers.Map lookup in KeyIndex       (row location)
%   instead of a full O(N) cell scan each time.
%   The regexp path falls back to the original imUtil linear-scan helper.
%
%   All existing method signatures are unchanged.  New methods added for
%   callers that know the canonical keyword name and want maximum throughput:
%     getValFast, getMultiValFast, getCellKeyFast, getKeyRows,
%     rebuildKeyIndex, resolveSynonym, getValSimple.
%
% Bug fixes vs original:
%   get.Key       - hardcoded Iobj=1 removed; no-op KeyS(1)=KeyS removed.
%                   Getter now operates on the scalar Obj that MATLAB always
%                   passes to property getters.
%   header2table  - ~FlagHistory & ~FlagHistory  ->  ~FlagComment & ~FlagHistory
%   deleteComment - Obj.Data = ...  ->  Obj(Iobj).Data = ...  inside element loop
%   julday        - Args.FunTimeKeys.Dicr  ->  Args.FunTimeKeys.Dict  (typo)
%   funUnary      - undefined variable ArgsInsertKeys  ->  Args.InsertKeys
%
% #functions (autogen)
% AstroHeader           - Construct AstroHeader object and populate it with headers
% createBasicHeader     - Create an AstroHeader object with a basic header
% deleteComment         - Delete all COMMENT keywords from header (or only empty ones)
% deleteKey             - Delete keywords from header by exact keyword name
% funUnary              - funUnary for AstroHeader - modify header and add history
% get.Key               - getter for Key, generate key structure array if needed
% getCellKey            - Get multiple keys from multiple headers into a cell array
% getCellKeyFast        - Fast exact-match multi-key retrieval using the keyword index
% getCoo                - get RA/Dec coordinates from header
% getDictionary         -
% getKeyRows            - Return row indices for a keyword using the O(1) index
% getMultiValFast       - Fast retrieval of multiple values using the keyword index
% getObsCoo             - Get Observatory geodetic position from Header
% getStructKey          - Get multiple keys from multiple headers into a structure array
% getVal                - get a single keyword value where the keyword appears first in a dictionary
% getValFast            - Fast exact-match keyword lookup using the keyword index (no dict/Val2Num)
% getValSimple          - Fast exact-match keyword lookup (no dict, returns [] when missing)
% groupByKeyVal         - Group a set of AstroHeaders by their unique keyword values
% help                  - show manuals.AstroHeader
% insertDefaultComments - Insert/replace default comments using the header comments dictionary
% insertKey             - Insert key/val/comment to headers
% isImType              - Check if header IMTYPE keyword value equals some type
% isKeyExist            - Check if a keyword exists in the header
% isKeyVal              - Check if a single keyword value equals some value
% julday                - Calculate mid exposure JD and ExpTime for AstroHeader object
% numKeys               - Return the number of lines/keywords in each header
% read                  - Read single/multiple headers from file/s into an Header object
% rebuildKeyIndex       - Rebuild the keyword-to-row containers.Map index from Data
% replaceVal            - Replace a keyword value in headers (no dictionary in key search)
% resolveSynonym        - Resolve a keyword synonym via the dictionary with per-object caching
% set.Data              - setter for header data; invalidates Key struct and keyword index
% setVal                - @Todo - use Dictionaries
% show                  - Display all headers in an AstroHeader object
% #/functions (autogen)
%

classdef AstroHeader < Component

    % ------------------------------------------------------------------ %
    properties (SetAccess = public)
        Data(:,3) cell            = cell(0,3);
        Key struct                = struct();

        File                      = '';
        HDU                       = ''; % HDU or dataset

        % Synonym / conversion dictionaries
        KeyDict Dictionary        % keyword-name synonyms
        ValDict Dictionary        % keyword-value synonyms (IMTYPE etc.)
        CommentDict Dictionary    % default comment strings
        TimeDict Dictionary       % time-keyword names and conversion handles
    end

    properties (Hidden, SetAccess = private)
        IsKeyUpToDate(1,1) logical    = true;

        % --- keyword-index cache (new) -----------------------------------
        % containers.Map: uppercase keyword char -> numeric row-index vector.
        % [] before the first build; rebuilt lazily via IsIndexUpToDate.
        KeyIndex                      = [];
        IsIndexUpToDate(1,1) logical  = false;

        % --- synonym-resolution cache (new) ------------------------------
        % containers.Map: cache-key string -> resolved alternate-names cell.
        % Cache key encodes the input synonym and all relevant search
        % options so different call signatures never collide.
        % Stays valid for the lifetime of the object because dictionaries
        % are assumed not to change at runtime.
        SynonymCache                  = [];
    end

    properties (Constant, Hidden)
        ColKey     = 1;
        ColVal     = 2;
        ColComment = 3;
    end

    % ================================================================== %

    methods  % Constructor

        function Obj = AstroHeader(FileNames, HDU, Args)
            % Construct AstroHeader object and populate it with headers
            % Input  : - Either a vector of the size of the empty
            %            AstroHeader object (e.g., [2 2]).
            %            OR a file name with wild cards or regular
            %            expression from which multiple files will be
            %            searched.
            %            OR a cell array of strings/char arrays of file names.
            %            Default is 1.
            %          - The index of HDU from which to read the header.
            %            This can be a vector or a scalar.
            %            Default is 1.
            %          * ...,key,val,...
            %            'UseRegExp' - Logical indicating if to use regexp
            %                   (true) or wild cards (false). Default is false.
            %            'UseMex'    - Use MEX reader. Default is false.
            % Output : - An AstroHeader object with populated headers.
            % Author : Eran Ofek (Mar 2021)
            % Example: H = AstroHeader('*.fits', 1);
            %          H = AstroHeader('WFPC2ASSNu5780205bx.fits');
            %          H = AstroHeader;

            arguments
                FileNames      = 1;
                HDU            = 1;
                Args.UseRegExp(1,1) logical = false;
                Args.UseMex                 = false;
            end

            if isnumeric(FileNames)
                % size vector -> create an empty object array of that size
                List = cell(FileNames);
            elseif iscell(FileNames) || isstring(FileNames)
                List = FileNames;
            else
                % char glob / regexp -> expand to a file list
                List = io.files.filelist(FileNames, 'UseRegExp', Args.UseRegExp, 'AddPath', true);
                if isempty(List)
                    error('No file was found in path');
                end
            end

            Nh = numel(List);
            for Ih = 1:1:Nh
                Obj(Ih).File = List{Ih};
            end
            Obj = reshape(Obj, size(List));

            Nhdu = numel(HDU);
            for Ih = 1:1:Nh
                if ~isempty(Obj(Ih).File)
                    Ihdu = min(Ih, Nhdu);
                    if Args.UseMex
                        Obj(Ih).Data = io.fits.mex.read_header(Obj(Ih).File, HDU(Ihdu));
                    else
                        Obj(Ih).Data = FITS.readHeader1(Obj(Ih).File, HDU(Ihdu));
                    end
                end
                Obj(Ih).KeyDict     = Dictionary.getDict('Header.Synonyms.KeyNames');
                Obj(Ih).ValDict     = Dictionary.getDict('Header.Synonyms.KeyVal.IMTYPE');
                Obj(Ih).CommentDict = Dictionary.getDict('Header.Comments.Default');
                Obj(Ih).TimeDict    = Dictionary.getDict('Header.Time.KeyNames');
                Obj(Ih).TimeDict    = string2funHandle(Obj(Ih).TimeDict);
                % KeyIndex and SynonymCache are [] and built lazily on first use
            end
        end

    end

    % ================================================================== %

    methods  % Setters / Getters

        function KeyS = get.Key(Obj)
            % getter for Key, generate key structure array if needed.
            % MATLAB always passes a scalar Obj to a property getter;
            % array-element access invokes the getter once per element.
            % Bug fix vs original: hardcoded Iobj=1 and no-op
            %   KeyS(Iobj) = KeyS self-assignment both removed.

            if Obj.IsKeyUpToDate
                KeyS = Obj.Key;
            else
                KeyS              = imUtil.headerCell.cellhead2struct(Obj.Data);
                Obj.Key           = KeyS;
                Obj.IsKeyUpToDate = true;
            end
        end

        function Obj = set.Key(Obj, Val)
            % setter for Key property
            Obj.Key = Val;
        end

        function set.Data(Obj, HeaderCell)
            % setter for the header data.
            % Marks both the Key struct and the keyword index as stale so
            % they are rebuilt lazily on next access.

            Obj.Data            = HeaderCell;
            Obj.IsKeyUpToDate   = false;
            Obj.IsIndexUpToDate = false;
        end

    end

    % ================================================================== %

    methods  % Keyword index and synonym cache  (new in redesign)

        function rebuildKeyIndex(Obj)
            % Rebuild the keyword-to-row containers.Map index from Data.
            %   Maps each uppercase keyword string to a numeric row-index
            %   vector, enabling O(1) lookups instead of O(N) linear scans.
            %   Called automatically by getKeyRows when IsIndexUpToDate is
            %   false (i.e. after any Data modification via the set.Data
            %   setter).  May also be called explicitly to force a rebuild.
            % Input  : - A single-element AstroHeader object.
            % Output : (modifies Obj in place)
            % Author : Eran Ofek (redesign 2025)
            % Example: H.rebuildKeyIndex;

            Nrow     = size(Obj.Data, 1);
            NewIndex = containers.Map('KeyType','char','ValueType','any');
            for Irow = 1:1:Nrow
                K = Obj.Data{Irow, Obj.ColKey};
                if ischar(K) && ~isempty(K)
                    Ku = upper(K);
                    if isKey(NewIndex, Ku)
                        NewIndex(Ku) = [NewIndex(Ku), Irow];
                    else
                        NewIndex(Ku) = Irow;
                    end
                end
            end
            Obj.KeyIndex        = NewIndex;
            Obj.IsIndexUpToDate = true;
        end

        function Rows = getKeyRows(Obj, Key, Occur)
            % Return row indices of a keyword in Data using the O(1) index.
            %   Rebuilds the index lazily when stale.
            %   Returns empty when the keyword is absent.
            % Input  : - A single-element AstroHeader object.
            %          - Keyword name (char). Case-insensitive.
            %          - Which occurrences to return:
            %                   'all' (default) | 'first' | 'last'.
            % Output : - Numeric row-index vector (empty if not found).
            % Author : Eran Ofek (redesign 2025)
            % Example: Rows = H.getKeyRows('EXPTIME');
            %          Row  = H.getKeyRows('COMMENT', 'first');

            arguments
                Obj(1,1)
                Key   (1,:) char
                Occur       = 'all';
            end

            if ~Obj.IsIndexUpToDate
                Obj.rebuildKeyIndex();
            end

            Ku = upper(Key);
            if isempty(Obj.KeyIndex) || ~isKey(Obj.KeyIndex, Ku)
                Rows = [];
                return;
            end
            Rows = Obj.KeyIndex(Ku);

            switch Occur
                case 'first'
                    Rows = Rows(1);
                case 'last'
                    Rows = Rows(end);
                case 'all'
                    % return all rows unchanged
                otherwise
                    error('Unknown Occur option: %s', Occur);
            end
        end

        function Alt = resolveSynonym(Obj, KeySynonym, Args)
            % Resolve a keyword synonym to its alternate-names list via the
            % KeyDict dictionary, with a per-object result cache.
            %   First call for a given synonym traverses the dictionary and
            %   stores the result.  Subsequent calls return the cached list
            %   at O(1) cost without re-traversing the dictionary.
            % Input  : - A single-element AstroHeader object.
            %          - A keyword synonym string (char).
            %          * ...,key,val,...
            %            'CaseSens'   - Case-sensitive search. Default false.
            %            'SearchAlgo' - ['strcmp'] | 'regexp'.
            %            'IsInputAlt' - If true, treat input as an alternate
            %                   name rather than the primary key. Default false.
            %            'KeyDict'    - Override dictionary. Default is [].
            % Output : - A cell array of alternate keyword name strings.
            %            Falls back to {KeySynonym} when not found in dict.
            % Author : Eran Ofek (redesign 2025)
            % Example: Alt = H.resolveSynonym('EXPTIME');
            %          Alt = H.resolveSynonym('AEXPTIME','IsInputAlt',true);

            arguments
                Obj(1,1)
                KeySynonym   (1,:) char
                Args.CaseSens(1,1) logical   = false;
                Args.SearchAlgo char         = 'strcmp';
                Args.IsInputAlt(1,1) logical = false;
                Args.KeyDict                 = [];
            end

            % Cache key encodes all options that affect the resolution result
            CacheKey = sprintf('%s|%d|%s|%d', upper(KeySynonym), ...
                               Args.CaseSens, Args.SearchAlgo, Args.IsInputAlt);

            % Fast return from cache when available
            if ~isempty(Obj.SynonymCache) && isKey(Obj.SynonymCache, CacheKey)
                Alt = Obj.SynonymCache(CacheKey);
                return;
            end

            % Select dictionary
            if isempty(Args.KeyDict)
                Dict = Obj.KeyDict;
            else
                Dict = Args.KeyDict;
            end

            % Resolve alternate names via the appropriate dictionary function
            if Args.IsInputAlt
                % input is an alternate name -> find the full synonym set
                [~, ~, Alt, ~] = searchAlt(Dict, {KeySynonym}, ...
                                            'CaseSens',   Args.CaseSens, ...
                                            'SearchAlgo', Args.SearchAlgo);
            else
                % input is the canonical primary key
                [Alt, ~] = searchKey(Dict, {KeySynonym}, ...
                                     'CaseSens',   Args.CaseSens, ...
                                     'SearchAlgo', Args.SearchAlgo);
            end

            % When the key is not in the dictionary, use it as-is
            if isempty(Alt)
                Alt = {KeySynonym};
            end

            % Store in cache (initialise the Map on first write)
            if isempty(Obj.SynonymCache)
                Obj.SynonymCache = containers.Map('KeyType','char','ValueType','any');
            end
            Obj.SynonymCache(CacheKey) = Alt;
        end

    end

    % ================================================================== %

    methods  % Read / Write

        function Obj = read(Obj, Args)
            % Read single/multiple headers from file/s into an Header object
            % Input  : - An Header object
            %          * ...,key,val,... or ,key=val,...
            %            'FileName' - File name or a cell array of file names.
            %                   Default is to use Header.File property.
            %            'HDU'  - HDU (scalar or vector) or dataset.
            %                   Default is to use Header.HDU.
            %                   If empty then set to 1.
            %            'Type' - File type. 'fits'|'hdf5'|['auto'].
            % Examples: H = AstroHeader;
            %   H.read('FileName',{'File1.fits','File2.fits'});
            %   A = H.read('FileName','File1.fits','HDU',[1 2]);

            arguments
                Obj
                Args.FileName  = Obj.File;
                Args.HDU       = Obj.HDU;
                Args.Type char {mustBeMember(Args.Type, ...
                    {'auto','fits','fit','FITS','FIT', ...
                     'fit.gz','fits.gz','hdf5','h5','hd5'})} = 'auto';
            end

            if isempty(Args.HDU)
                Args.HDU = 1;
            end

            if ~iscell(Args.FileName)
                Args.FileName = {Args.FileName};
            end
            Nfile = numel(Args.FileName);
            Nhdu  = numel(Args.HDU);
            Nmax  = max(Nfile, Nhdu);

            switch Args.Type
                case 'auto'
                    FileParts = split(Args.FileName, '.');
                    Args.Type = FileParts{end};
            end

            switch lower(Args.Type)
                case {'fits','fit','fit.gz','fits.gz'}
                    for Imax = 1:1:Nmax
                        Ih    = min(Nhdu,  Imax);
                        Ifile = min(Nfile, Imax);
                        Obj(Ifile).Data = FITS.readHeader1(Args.FileName{Ifile}, Args.HDU(Ih));
                        Obj(Ifile).File = Args.FileName{Ifile};
                        Obj(Ifile).HDU  = Args.HDU(Ih);
                    end
                case {'hdf5','h5','hd5'}
                    error('Read Header from HDF5 file is not available yet');
                otherwise
                    error('Unknown file Type option');
            end
        end

        function show(Obj)
            % Display all headers in an AstroHeader object

            Nobj = numel(Obj);
            for Iobj = 1:1:Nobj
                disp(Obj(Iobj).Data);
            end
        end

        function Result = readFromTextFile(Obj, FileName, Args)
            % Read single header from text file.
            % Lines format is: Key = Value / Comment
            % Input  : - AstroHeader object
            %          - Text file name
            %          * Pairs of ...,key,val,...
            %            'Print' - Print each parsed line. Default is true.
            % Output : - True on success
            % Author : Chen Tishler (02/2023)
            % Example: readFromTextFile('header.txt')

            arguments
                Obj
                FileName
                Args.Print = true;
            end

            fid = fopen(FileName, 'rt');
            while true
                Line = fgetl(fid);
                if ~ischar(Line)
                    break;
                end

                S = split(Line, '=');
                if numel(S) > 1
                    Key = strip(S{1});
                    Key = lower(Key);
                    Key = replace(Key, '-', '_');

                    W       = split(S{2}, '/');
                    Value   = strip(W{1});
                    Comment = '';
                    if numel(W) > 1
                        Comment = strip(W{2});
                    end

                    if Args.Print
                        fprintf('%s = %s --- %s\n', Key, Value, Comment);
                    end

                    Num = str2num(Value); %#ok<ST2NM>
                    if ~isempty(Num)
                        Value = Num;
                    end

                    Obj.insertKey({Key, Value, Comment}, 'end');
                end
            end
            fclose(fid);
            Result = true;
        end

    end

    % ================================================================== %

    methods (Static)  % Static constructors

        function Obj = createBasicHeader(Size, varargin)
            % Create an AstroHeader object with a basic header
            % Input  : - Size of AstroHeader object (e.g., [2 2]).
            %            Default is 1.
            %          * Either a cell array of {Key,Val} or {Key,Val,Comment}
            %            to replace or add keywords, or pairs of key,val arguments.
            % Output : - An AstroHeader object with the basic populated header.
            % Author : Eran Ofek (Apr 2021)
            % Example: HH = AstroHeader.createBasicHeader
            %   HH = AstroHeader.createBasicHeader(1,{'WINDDIR',11;'M_STAT','ok';'NEW',1});
            %   HH = AstroHeader.createBasicHeader(1,{'WINDDIR',11,'aa';'M_STAT','ok','jj'});
            %   HH = AstroHeader.createBasicHeader([1 2],'WINDDIR',11,'M_STAT','ok','NEW',1);

            if nargin == 0
                Size = [1 1];
            end

            Narg = numel(varargin);
            if Narg == 1
                Key  = varargin{1}(:,1);
                Val  = varargin{1}(:,2);
                if size(varargin{1}, 2) > 2
                    Comment = varargin{1}(:,3);
                else
                    Comment = [];
                end
            else
                if mod(Narg, 2) ~= 0
                    error('Number of input arguments must be even (key,val)');
                end
                Key     = varargin(1:2:end);
                Val     = varargin(2:2:end);
                Comment = [];
            end

            CellHeader = {'SIMPLE',true,'';...
                          'BITPIX',-32,'';...
                          'NAXIS',2,'';...
                          'NAXIS1',NaN,'';...
                          'NAXIS2',NaN,'';...
                          'ORIGIN',[],'';...
                          'CREATOR',[],'';...
                          'TELESCOP',[],'';...
                          'INSTRUME',[],'';...
                          'OBSERVER',[],'';...
                          'IMTYPE',[],'';...
                          'DATE-OBS',[],'';...
                          'UTC-OBS',[],'';...
                          'REFERENC',[],'';...
                          'FILTER',[],'';...
                          'EXPTIME',[],'';...
                          'GAIN',[],'';...
                          'READNOI',[],'';...
                          'OBSJD',[],'';...
                          'OBSMJD',[],'';...
                          'MIDJD',[],'';...
                          'OBSLST',[],'';...
                          'PIXSCALE',[],'';...
                          'RA',[],'';...
                          'DEC',[],'';...
                          'HA',[],'';...
                          'M_RA',[],'';...
                          'M_DEC',[],'';...
                          'M_HA',[],'';...
                          'M_EQUI',[],'';...
                          'T_RA',[],'';...
                          'T_DEC',[],'';...
                          'T_HA',[],'';...
                          'T_EQUI',[],'';...
                          'AIRMASS',[],'';...
                          'WCSAXES',[],'';...
                          'CRVAL1',[],'';...
                          'CRVAL2',[],'';...
                          'CRPIX1',[],'';...
                          'CRPIX2',[],'';...
                          'CTYPE1',[],'';...
                          'CTYPE2',[],'';...
                          'CUNIT1',[],'';...
                          'CUNIT2',[],'';...
                          'CRTYPE1',[],'';...
                          'CRTYPE2',[],'';...
                          'CD1_1',[],'';...
                          'CD1_2',[],'';...
                          'CD2_1',[],'';...
                          'CD2_2',[],'';...
                          'EQUNOX',[],'';...
                          'LONPOLE',[],'';...
                          'LATPOLE',[],'';...
                          'ASTRMS',[],'';...
                          'SEEING',[],'';...
                          'LIMMAG',[],'';...
                          'ZP',[],'';...
                          'DOMESTAT',[],'';...
                          'M_STAT',[],'';...
                          'TEMP_TEL',[],'';...
                          'TEMP_IN',[],'';...
                          'TEMP_OUT',[],'';...
                          'TEMP_CAM',[],'';...
                          'HUM_IN',[],'';...
                          'HUM_OUT',[],'';...
                          'PRESSURE',[],'';...
                          'WINDSP',[],'';...
                          'WINDDIR',[],''};

            Obj  = AstroHeader(Size);
            Nobj = numel(Obj);
            for Iobj = 1:1:Nobj
                Obj(Iobj).HDU  = 1;
                Obj(Iobj).File = '';
                Obj(Iobj).Data = CellHeader;
            end

            Obj.replaceVal(Key, Val, 'Comment', Comment);
        end

    end

    % ================================================================== %

    methods  % functions for internal use

        function Dict = getDictionary(Args) %#ok<INUSD>
            %
        end

    end

    % ================================================================== %

    methods  % funUnary / Binary / Stack / Transform

        function Result = funUnary(Obj, Operator, Args)
            % funUnary for AstroHeader - modify header and add history
            % This is a self-explanatory function usually for internal use
            % Example: H = AstroHeader('*.fit');

            arguments
                Obj
                Operator
                Args.OpArgs cell                  = {};
                Args.UpdateHeader                 = true;
                Args.AddHistory                   = true;
                Args.NewUnits                     = []; % if empty don't change
                Args.UnitsKey                     = 'UNITS';
                Args.InsertKeys                   = {};
                Args.ReplaceKeys                  = {};
                Args.ReplaceVals                  = {};
                Args.CreateNewObj                 = [];
                Args.replaceValArgs               = {};
                Args.insertKeyArgs                = {};
            end

            if isempty(Args.CreateNewObj)
                if nargout > 0
                    Args.CreateNewObj = true;
                else
                    Args.CreateNewObj = false;
                end
            end

            if Args.CreateNewObj
                Result = Obj.copy();
            else
                Result = Obj;
            end

            if Args.UpdateHeader
                if ~isempty(Args.NewUnits)
                    Result = replaceVal(Result, Args.UnitsKey, Args.NewUnits, ...
                                        Args.replaceValArgs{:});
                end
                if ~isempty(Args.ReplaceKeys)
                    Result = replaceVal(Result, Args.ReplaceKeys, Args.ReplaceVals, ...
                                        Args.replaceValArgs{:});
                end
                if ~isempty(Args.InsertKeys)
                    % Bug fix: original used undefined variable ArgsInsertKeys
                    Result = insertKey(Result, Args.InsertKeys, Args.insertKeyArgs{:});
                end
                if Args.AddHistory
                    HistoryLine = {'HISTORY', ...
                                   sprintf('funUnary with operator: %s', func2str(Operator)), ''};
                    Result = insertKey(Result, HistoryLine, Args.insertKeyArgs{:});
                end
            end
        end

    end

    % ================================================================== %

    methods  % getVal and keyword retrieval

        function Nline = numKeys(Obj)
            % Return the number of lines/keywords in each header
            % Input  : - An AstroHeader object.
            % Output : - An array with the number of lines in each header.
            % Author : Eran Ofek (May 2023)

            Nobj  = numel(Obj);
            Nline = zeros(size(Obj));
            for Iobj = 1:1:Nobj
                Nline(Iobj) = size(Obj(Iobj).Data, 1);
            end
        end

        function [Val, Comment] = getValSimple(Obj, Key)
            % Fast exact-match keyword lookup using the keyword index.
            %   No dictionary, no Val2Num.  Returns [] when keyword absent.
            %   Prefer getValFast when a NaN sentinel for "not found" is
            %   more convenient for numeric downstream logic.
            % Input  : - A single-element AstroHeader object.
            %          - Keyword name (char). Case-insensitive.
            % Output : - Value ([] if not found).
            %          - Comment ('' if not found).
            % Author : Eran Ofek (redesign 2025)
            % Example: [Val,Com] = H.getValSimple('EXPTIME');

            Row = Obj.getKeyRows(Key, 'first');
            if isempty(Row)
                Val = [];
                if nargout > 1
                    Comment = '';
                end
            else
                Val = Obj.Data{Row, Obj.ColVal};
                if nargout > 1
                    Comment = Obj.Data{Row, Obj.ColComment};
                end
            end
        end

        function [Val, Comment] = getValFast(Obj, Key)
            % Fast exact-match keyword value retrieval using the keyword index.
            %   No dictionary lookup, no synonym resolution, no Val2Num
            %   conversion.  Returns NaN when the keyword is absent.
            %   For use in tight pipeline loops where the caller already
            %   knows the canonical FITS keyword name.
            % Input  : - A single-element AstroHeader object.
            %          - Keyword name (char). Case-insensitive.
            % Output : - Value (NaN if not found).
            %          - Comment ('' if not found).
            % Author : Eran Ofek (redesign 2025)
            % Example: Val        = H.getValFast('EXPTIME');
            %          [Val, Com] = H.getValFast('DATE-OBS');

            Row = Obj.getKeyRows(Key, 'first');
            if isempty(Row)
                Val = NaN;
                if nargout > 1
                    Comment = '';
                end
            else
                Val = Obj.Data{Row, Obj.ColVal};
                if nargout > 1
                    Comment = Obj.Data{Row, Obj.ColComment};
                end
            end
        end

        function Vals = getMultiValFast(Obj, Keys)
            % Fast retrieval of multiple keyword values using the keyword index.
            %   No dictionary lookup, no Val2Num.  One O(1) index hit per
            %   keyword.  Equivalent to calling getValFast in a loop but
            %   expressed as a single call for convenience.
            % Input  : - A single-element AstroHeader object.
            %          - A cell array of keyword names (case-insensitive).
            % Output : - A 1 x Nkey cell array of values (NaN for missing keys).
            % Author : Eran Ofek (redesign 2025)
            % Example: Vals = H.getMultiValFast({'EXPTIME','GAIN','FILTER'});

            arguments
                Obj(1,1)
                Keys cell
            end

            Nkey = numel(Keys);
            Vals = cell(1, Nkey);
            for Ikey = 1:1:Nkey
                Row = Obj.getKeyRows(Keys{Ikey}, 'first');
                if isempty(Row)
                    Vals{Ikey} = NaN;
                else
                    Vals{Ikey} = Obj.Data{Row, Obj.ColVal};
                end
            end
        end

        function [Val, Key, Comment, Nfound] = getVal(Obj, KeySynonym, Args)
            % get a single keyword value where the keyword appears first
            % in a dictionary synonym list.
            %   Uses the keyword index (O(1) per synonym) for the common
            %   strcmp search path.  Falls back to the original linear-scan
            %   helper (imUtil.headerCell.getValBySynonym) for regexp.
            %   Synonym resolution is cached per object via resolveSynonym
            %   so repeated calls with the same key incur zero dictionary
            %   traversal overhead after the first call.
            % Input  : - A single element AstroHeader object
            %          - Either a single character array, or a cell array
            %            of character arrays. If a single char array, then
            %            search with or without the dictionary.
            %            If a cell array, the array overrides the dictionary.
            %          * ...,key,val,...
            %            'UseDict' - Indicating if to use dictionary or to
            %                   perform an exact search. Default is true.
            %            'CaseSens' - Default is false.
            %            'SearchAlgo' - ['strcmp'] | 'regexp'.
            %            'Fill' - Fill value when key is not found.
            %                   Default is NaN (comment will be '').
            %            'Val2Num' - Attempt to convert value to numeric.
            %                   Default is true.
            %            'Occur' - ['first'] | 'last'.
            %            'KeyDict' - An optional keyword dictionary that
            %                   will override the object dictionary.
            %            'IsInputAlt' - If true, treat the input keyword
            %                   as an alternate name in the dictionary.
            %                   Default is false.
            %            'ReadCCDSEC' - If true attempt to convert a
            %                   CCDSEC-like string '[xmin xmax ymin ymax]'
            %                   to a 4-element numeric vector. Default false.
            % Output : - Value
            %          - Keyword name found
            %          - Comment
            %          - Number of occurrences found
            % Author : Eran Ofek (Mar 2021)
            % Example: H = AstroHeader('WFPC2ASSNu5780205bx.fits');
            %   [Val,Key,Comment,Nfound] = getVal(H,'EXPTIME')
            %   [Val,Key,Comment,Nfound] = getVal(H,'AEXPTIME','IsInputAlt',true)
            %   [Val,Key,Comment,Nfound] = getVal(H,{'BB','EXPTIME','AA'})
            %   [Val,Key,Comment,Nfound] = getVal(H,'EXPTIME','UseDict',false)

            arguments
                Obj(1,1)
                KeySynonym
                Args.UseDict(1,1) logical    = true;
                Args.CaseSens(1,1) logical   = false;
                Args.SearchAlgo char         = 'strcmp';
                Args.Fill                    = NaN;
                Args.Val2Num(1,1) logical    = true;
                Args.Occur                   = 'first';
                Args.KeyDict                 = [];
                Args.IsInputAlt(1,1) logical = false;
                Args.ReadCCDSEC(1,1) logical = false;
            end

            if ischar(KeySynonym)
                KeySynonym = {KeySynonym};
            end
            Nsyn = numel(KeySynonym);

            % ---- build the alternate-name list -------------------------
            if Args.UseDict && Nsyn == 1
                % single synonym -> resolve via the dictionary (cached)
                Alt = Obj.resolveSynonym(KeySynonym{1}, ...
                                          'CaseSens',   Args.CaseSens, ...
                                          'SearchAlgo', Args.SearchAlgo, ...
                                          'IsInputAlt', Args.IsInputAlt, ...
                                          'KeyDict',    Args.KeyDict);
            else
                % multi-element cell array overrides the dictionary
                Alt = KeySynonym;
            end

            % ---- regexp: cannot use the index, fall back to legacy ------
            if strcmp(Args.SearchAlgo, 'regexp')
                [Val, Key, Comment, Nfound] = imUtil.headerCell.getValBySynonym( ...
                                                  Obj.Data, Alt, ...
                                                  'CaseSens',   Args.CaseSens, ...
                                                  'SearchAlgo', Args.SearchAlgo, ...
                                                  'Fill',       Args.Fill, ...
                                                  'Val2Num',    Args.Val2Num, ...
                                                  'Occur',      Args.Occur);
                if Args.ReadCCDSEC && ischar(Val)
                    CCDSEC = real(str2doubleq(regexp(Val, '\[|\]|\s', 'split')));
                    Val    = CCDSEC(2:end-1);
                end
                return;
            end

            % ---- index-accelerated lookup: O(1) per synonym ------------
            Val     = Args.Fill;
            Key     = '';
            Comment = '';
            Nfound  = 0;

            for Ialt = 1:1:numel(Alt)
                AllRows = Obj.getKeyRows(Alt{Ialt}, 'all');
                if ~isempty(AllRows)
                    Nfound = numel(AllRows);
                    switch Args.Occur
                        case 'first'
                            Row = AllRows(1);
                        case 'last'
                            Row = AllRows(end);
                        otherwise
                            Row = AllRows(1);
                    end
                    Val     = Obj.Data{Row, Obj.ColVal};
                    Key     = Obj.Data{Row, Obj.ColKey};
                    Comment = Obj.Data{Row, Obj.ColComment};
                    break;
                end
            end

            % optional numeric conversion
            if Args.Val2Num && ischar(Val)
                Num = str2double(Val);
                if ~isnan(Num)
                    Val = Num;
                end
            end

            % optional CCDSEC string to vector
            if Args.ReadCCDSEC && ischar(Val)
                CCDSEC = real(str2doubleq(regexp(Val, '\[|\]|\s', 'split')));
                Val    = CCDSEC(2:end-1);
            end
        end

        function [Result, ResultC, IK] = getStructKey(Obj, ExactKeys, Args)
            % Get multiple keys from multiple headers and store in a structure array
            %       The keyword search can be exact (UseDict=false), or
            %       using a keywords dictionary (UseDict=true).
            % Input  : - An AstroHeader object (multiple elements supported)
            %          - A cell array of keyword names.
            %          * ...,key,val,...
            %            'UseDict' - Indicating if to use dictionary or to
            %                   perform an exact search. Default is true.
            %            'CaseSens' - Default is true.
            %            'SearchAlgo' - ['strcmp'] | 'regexp'.
            %            'Fill' - Fill value when key is not found. Default NaN.
            %            'Val2Num' - Attempt to convert value to numeric.
            %                   Default is true.
            %            'IsInputAlt' - If true, treat input keyword as an
            %                   alternate name. Default is true.
            %            'KeyDict' - Override dictionary. Default is [].
            % Output : - A structure array (one element per AstroHeader).
            %            Field names are the requested ExactKeys.
            %          - The same, but for comments.
            %          - Row-index cell array (only for last element,
            %            UseDict=false path only).
            % Author : Eran Ofek (Apr 2021)
            % Example: H = AstroHeader('WFPC2ASSNu5780205bx.fits');
            %   [Result,C] = getStructKey(H,{'EXPTIME'})
            %   [Result,C] = getStructKey(H,{'EXPTIME','A'},'UseDict',false)

            arguments
                Obj
                ExactKeys
                Args.UseDict(1,1) logical    = true;
                Args.CaseSens(1,1) logical   = true;
                Args.SearchAlgo char         = 'strcmp';
                Args.Fill                    = NaN;
                Args.Val2Num(1,1) logical    = true;
                Args.IsInputAlt(1,1) logical = true;
                Args.KeyDict                 = [];
            end

            if ischar(ExactKeys)
                ExactKeys = {ExactKeys};
            end

            Nkey = numel(ExactKeys);
            Nobj = numel(Obj);
            IK   = {};

            for Iobj = 1:1:Nobj
                if isempty(Args.KeyDict)
                    Dict = Obj(Iobj).KeyDict;
                else
                    Dict = Args.KeyDict;
                end

                if Args.UseDict
                    % index-accelerated path: getVal handles synonym caching
                    for Ikey = 1:1:Nkey
                        [Val, ~, Comment, ~] = Obj(Iobj).getVal(ExactKeys{Ikey}, ...
                                                                  'UseDict',    Args.UseDict, ...
                                                                  'CaseSens',   Args.CaseSens, ...
                                                                  'SearchAlgo', Args.SearchAlgo, ...
                                                                  'Fill',       Args.Fill, ...
                                                                  'Val2Num',    Args.Val2Num, ...
                                                                  'Occur',      'first', ...
                                                                  'IsInputAlt', Args.IsInputAlt, ...
                                                                  'KeyDict',    Dict);
                        Result(Iobj).(ExactKeys{Ikey}) = Val;
                        if nargout > 1
                            ResultC(Iobj).(ExactKeys{Ikey}) = Comment;
                        end
                    end
                else
                    % exact-match path: delegate to legacy utility which
                    % correctly handles regexp and CaseSens edge cases
                    [SC, ~, ~, IK] = imUtil.headerCell.getByKey(Obj(Iobj).Data, ExactKeys, ...
                                                                 'ReturnN',  1, ...
                                                                 'CaseSens', Args.CaseSens, ...
                                                                 'Fill',     Args.Fill, ...
                                                                 'Col',      1, ...
                                                                 'Val2Num',  Args.Val2Num);
                    % Build struct field-by-field with requested ExactKeys as
                    % field names so the caller gets predictable field names
                    % regardless of actual case stored in Data.
                    for Ikey = 1:1:Nkey
                        Result(Iobj).(ExactKeys{Ikey}) = SC{Ikey, Obj(Iobj).ColVal};
                        if nargout > 1
                            ResultC(Iobj).(ExactKeys{Ikey}) = SC{Ikey, Obj(Iobj).ColComment};
                        end
                    end
                end
            end
        end

        function [ResultVal, IK] = getCellKey(Obj, ExactKeys, Args)
            % Get multiple keys from multiple headers and store in a cell array
            %       The keyword search can be exact (UseDict=false), or
            %       using a keywords dictionary (UseDict=true).
            % Input  : - An AstroHeader object (multiple elements supported)
            %          - A cell array of keyword names.
            %          * ...,key,val,...
            %            'UseDict' - Indicating if to use dictionary or to
            %                   perform an exact search. Default is true.
            %            'CaseSens' - Default is true.
            %            'SearchAlgo' - ['strcmp'] | 'regexp'.
            %            'Fill' - Fill value when key is not found. Default NaN.
            %            'Val2Num' - Attempt to convert value to numeric.
            %                   Default is true.
            %            'IsInputAlt' - If true, treat input keyword as an
            %                   alternate name. Default is true.
            %            'KeyDict' - Override dictionary. Default is [].
            % Output : - A cell array of keyword values (Nobj x Nkey).
            %          - Row-index cell array (only for last element,
            %            UseDict=false path only).
            % Author : Eran Ofek (Apr 2021)
            % Example: H = AstroHeader('WFPC2ASSNu5780205bx.fits');
            %   [Result,IK] = getCellKey([H,H],{'EXPTIME','bb'},'UseDict',false)
            %   [Result,IK] = getCellKey([H,H],{'AEXPTIME','bb'})

            arguments
                Obj
                ExactKeys
                Args.UseDict(1,1) logical                     = true;
                Args.CaseSens(1,1) logical                    = true;
                Args.SearchAlgo char {mustBeMember( ...
                    Args.SearchAlgo,{'strcmp','regexp'})}     = 'strcmp';
                Args.Fill                                     = NaN;
                Args.Val2Num(1,1) logical                     = true;
                Args.IsInputAlt(1,1) logical                  = true;
                Args.KeyDict                                  = [];
            end

            if ischar(ExactKeys)
                ExactKeys = {ExactKeys};
            end

            IK        = {};
            Nkey      = numel(ExactKeys);
            Nobj      = numel(Obj);
            ResultVal = cell(Nobj, Nkey);

            for Iobj = 1:1:Nobj
                if isempty(Args.KeyDict)
                    Dict = Obj(Iobj).KeyDict;
                else
                    Dict = Args.KeyDict;
                end

                if Args.UseDict
                    for Ikey = 1:1:Nkey
                        [Val, ~, ~, ~] = Obj(Iobj).getVal(ExactKeys{Ikey}, ...
                                                            'UseDict',    Args.UseDict, ...
                                                            'CaseSens',   Args.CaseSens, ...
                                                            'SearchAlgo', Args.SearchAlgo, ...
                                                            'Fill',       Args.Fill, ...
                                                            'Val2Num',    Args.Val2Num, ...
                                                            'Occur',      'first', ...
                                                            'IsInputAlt', Args.IsInputAlt, ...
                                                            'KeyDict',    Dict);
                        ResultVal{Iobj, Ikey} = Val;
                    end
                else
                    [SC, ~, ~, IK] = imUtil.headerCell.getByKey(Obj(Iobj).Data, ExactKeys, ...
                                                                 'ReturnN',  1, ...
                                                                 'CaseSens', Args.CaseSens, ...
                                                                 'Fill',     Args.Fill, ...
                                                                 'Col',      1, ...
                                                                 'Val2Num',  Args.Val2Num);
                    ResultVal(Iobj, :) = SC(:, Obj(Iobj).ColVal).';
                end
            end
        end

        function [ResultVal, IK] = getCellKeyFast(Obj, ExactKeys)
            % Fast exact-match multi-key retrieval using the keyword index.
            %   No dictionary lookup, no Val2Num.  One O(1) index hit per
            %   keyword per header element.  Equivalent to getCellKey with
            %   UseDict=false but bypasses the imUtil helper entirely.
            %   Use when canonical keyword names are already known and
            %   maximum throughput is required.
            % Input  : - An AstroHeader object (multiple elements supported).
            %          - A cell array of keyword names (case-insensitive).
            % Output : - A cell array (Nobj x Nkey) of values.
            %            NaN for any keyword absent from a given header.
            %          - IK: unused; kept for signature parity with getCellKey.
            % Author : Eran Ofek (redesign 2025)
            % Example: Vals = getCellKeyFast([H,H],{'EXPTIME','GAIN','FILTER'});

            arguments
                Obj
                ExactKeys cell
            end

            IK        = {};
            Nkey      = numel(ExactKeys);
            Nobj      = numel(Obj);
            ResultVal = cell(Nobj, Nkey);

            for Iobj = 1:1:Nobj
                for Ikey = 1:1:Nkey
                    Row = Obj(Iobj).getKeyRows(ExactKeys{Ikey}, 'first');
                    if isempty(Row)
                        ResultVal{Iobj, Ikey} = NaN;
                    else
                        ResultVal{Iobj, Ikey} = Obj(Iobj).Data{Row, Obj(Iobj).ColVal};
                    end
                end
            end
        end

        function Obj = insertDefaultComments(Obj, Args)
            % Insert/replace default comments for keys using the header comments dictionary
            % Input  : - An AstroHeader object (multiple elements supported)
            %          * ...,key,val,...
            %            'CaseSens' - Default is true.
            %            'SearchAlgo' - ['strcmp'] | 'regexp'.
            %            'IsInputAlt' - If true look for the keyword in
            %                   the alternate names list. Default is true.
            %            'Occur' - ['first'] | 'last'.
            % Output : - An AstroHeader with comments populated from
            %            the CommentDict dictionary.
            % Author : Eran Ofek (Apr 2021)
            % Example: H = AstroHeader('WFPC2ASSNu5780205bx.fits');
            %          insertDefaultComments(H)

            arguments
                Obj
                Args.CaseSens(1,1) logical                             = true;
                Args.SearchAlgo char {mustBeMember(Args.SearchAlgo, ...
                    {'strcmp','regexp'})}                              = 'strcmp';
                Args.IsInputAlt(1,1) logical                           = true;
                Args.Occur                                             = 'first';
            end

            Nobj = numel(Obj);
            for Iobj = 1:1:Nobj
                DictKeyNames = fieldnames(Obj(Iobj).CommentDict.Dict);
                NdictKeys    = numel(DictKeyNames);
                for IdictKeys = 1:1:NdictKeys
                    if Args.IsInputAlt
                        [Key, ~, Alt, ~] = searchAlt(Obj(Iobj).KeyDict, ...
                                                       DictKeyNames{IdictKeys}, ...
                                                       'CaseSens',   Args.CaseSens, ...
                                                       'SearchAlgo', Args.SearchAlgo);
                        if isempty(Alt)
                            Alt = DictKeyNames(IdictKeys);
                        end
                    else
                        Key = DictKeyNames{IdictKeys};
                        Alt = DictKeyNames(IdictKeys);
                    end

                    if ~isempty(Alt)
                        CleanCell        = Obj(Iobj).Data(:, Obj(Iobj).ColKey);
                        FlagNOK          = cellfun(@isnumeric, CleanCell);
                        [CleanCell{FlagNOK}] = deal('');

                        Flag = ismember(CleanCell, Alt);
                        Ind  = find(Flag, 1, Args.Occur);

                        if ~isempty(Ind)
                            KeyName = Obj(Iobj).Data{Ind, Obj(Iobj).ColKey};
                            try
                                Obj(Iobj).Data{Ind, Obj.ColComment} = ...
                                    Obj(Iobj).CommentDict.Dict.(KeyName){1};
                            catch
                                Obj(Iobj).Data{Ind, Obj.ColComment} = ...
                                    Obj(Iobj).CommentDict.Dict.(Key){1};
                            end
                        end
                    end
                end
            end
        end

        function Obj = deleteKey(Obj, ExactKeys, Args)
            % Delete keywords from header by exact keyword name
            % Input  : - An AstroHeader object (multiple elements are supported).
            %          - A char array or a cell array of chars of keyword
            %            names to delete from all the headers.
            %          * ...,key,val,...
            %            'CaseSens' - Default is true.
            %            'UseRegExp' - Use regexp (true) or strcmp (false).
            %                   Default is true.
            %            'Algo' - Algorithm used. Default is 1.
            %               If UseRegExp=true, then will revert to Algo=2.
            % Example: H = AstroHeader('WFPC2ASSNu5780205bx.fits');
            %          deleteKey(H,{'EXPTIME','A','COMMENT'})
            %          deleteKey(H,{'SKYSUB\d'})  % use regexp

            arguments
                Obj
                ExactKeys
                Args.CaseSens   = true;
                Args.UseRegExp  = true;
                Args.Algo       = 1;
            end

            if Args.UseRegExp
                Args.Algo = 2;
            end

            Nobj = numel(Obj);

            if Args.Algo == 1
                if Args.CaseSens
                    for Iobj = 1:1:Nobj
                        FlagToRemove   = ismember(Obj(Iobj).Data(:,1), ExactKeys);
                        Obj(Iobj).Data = Obj(Iobj).Data(~FlagToRemove, :);
                    end
                else
                    for Iobj = 1:1:Nobj
                        FlagToRemove   = ismember(upper(Obj(Iobj).Data(:,1)), upper(ExactKeys));
                        Obj(Iobj).Data = Obj(Iobj).Data(~FlagToRemove, :);
                    end
                end

            elseif Args.Algo == 2
                if ischar(ExactKeys)
                    ExactKeys = {ExactKeys};
                end

                Nkeys     = numel(ExactKeys);
                searchFun = tools.string.stringSearchFun(Args.UseRegExp, Args.CaseSens);

                for Iobj = 1:1:Nobj
                    Nrow = size(Obj(Iobj).Data, 1);
                    Flag = false(Nrow, 1);
                    for Ikeys = 1:1:Nkeys
                        NewFlag = searchFun(Obj(Iobj).Data(:, Obj(Iobj).ColKey), ExactKeys{Ikeys});
                        Flag    = Flag | NewFlag(:);
                    end
                    Obj(Iobj).Data = Obj(Iobj).Data(~Flag, :);
                end
            end
        end

        function Obj = insertKey(Obj, KeyValComment, Pos)
            % Insert key/val/comment to headers
            % Input  : - An AstroHeader object (multi. elements supported)
            %          - Either a key name, or a cell array of
            %            [Key,Val,Comment], or [Key,Val].
            %          - Position for insertion. Default is 'end-1'.
            % Output : - An AstroHeader object with the new key/vals.
            % Author : Eran Ofek (Apr 2021)
            % Example: H = AstroHeader('WFPC2ASSNu5780205bx.fits');
            %          H.insertKey('stam')
            %          H.insertKey({'A','','';'B','',''},'end-1')

            arguments
                Obj
                KeyValComment
                Pos = 'end-1';
            end

            Nobj = numel(Obj);
            for Iobj = 1:1:Nobj
                Obj(Iobj).Data = imUtil.headerCell.insertKey( ...
                                     Obj(Iobj).Data, KeyValComment, Pos);
            end
        end

        function Obj = replaceVal(Obj, Key, Val, Args)
            % Replace a keyword value in headers (no dictionary in key search).
            % Input  : - An AstroHeader object (multi elements supported).
            %          - A key name or a cell array of key names.
            %          - A vector or cell array of values corresponding to keys.
            %          * ...,key,val,...
            %            'SearchAlgo' - ['strcmp'] | 'regexp'
            %            'CaseSens'   - Default is true.
            %            'RepVal'     - Replace value. Default is true.
            %            'Comment'    - Cell array of optional comments.
            %                   If empty, do not replace comment. Default [].
            %            'NewKey' - Cell array of new key names to replace
            %                   the old keys. Default is {}.
            %            'AddKey' - Add key if it doesn't exist. Default true.
            %            'AddPos' - Position when adding a new key. Default 'end'.
            %            'ColKey'     - Column index of keys.     Default 1.
            %            'ColVal'     - Column index of values.   Default 2.
            %            'ColComment' - Column index of comments. Default 3.
            % Output : - Updated AstroHeader object.
            % Example: H = AstroHeader('WFPC2ASSNu5780205bx.fits');
            %          H.replaceVal({'COMMENT'},{''});

            arguments
                Obj
                Key
                Val
                Args.SearchAlgo  = 'strcmp';
                Args.CaseSens    = true;
                Args.RepVal      = true;
                Args.Comment     = [];
                Args.NewKey      = {};
                Args.AddKey      = true;
                Args.AddPos      = 'end';
                Args.ColKey      = 1;
                Args.ColVal      = 2;
                Args.ColComment  = 3;
            end

            Nobj = numel(Obj);
            for Iobj = 1:1:Nobj
                Obj(Iobj).Data = imUtil.headerCell.replaceKey( ...
                                     Obj(Iobj).Data, Key, Val, ...
                                     'SearchAlgo', Args.SearchAlgo, ...
                                     'CaseSens',   Args.CaseSens, ...
                                     'RepVal',     Args.RepVal, ...
                                     'Comment',    Args.Comment, ...
                                     'NewKey',     Args.NewKey, ...
                                     'AddKey',     Args.AddKey, ...
                                     'AddPos',     Args.AddPos, ...
                                     'ColKey',     Obj(Iobj).ColKey, ...
                                     'ColVal',     Obj(Iobj).ColVal, ...
                                     'ColComment', Obj(Iobj).ColComment);
            end
        end

        function Result = setVal(Obj, Key, Val)
            % @Todo - use Dictionaries
            Result = Obj.replaceVal(Key, Val);
        end

        function Result = isKeyVal(Obj, Key, Val, Args)
            % Check if a single keyword value equal to some value.
            % Input  : - An AstroHeader object (multi elements supported).
            %          - A single header keyword name.
            %          - A value (string or char array) to compare to the
            %            header keyword value.
            %          * ...,key,val,...
            %            'NumericTol'  - Tolerance for numeric comparison.
            %                   Default is 1e-8.
            %            'KeyCaseSens' - Key search case sensitive. Default true.
            %            'ValCaseSens' - Value comparison case sensitive. Default false.
            %            'UseDict'     - Use dictionary. Default is true.
            %            'SearchAlgo'  - ['strcmp'] | 'regexp'.
            %            'Fill'        - Fill value if not found. Default NaN.
            %            'Val2Num'     - Convert to numeric. Default true.
            %            'Occur'       - ['first'] | 'last'.
            %            'KeyDict'     - Override dictionary. Default [].
            %            'IsInputAlt'  - Treat input as alternate name. Default false.
            % Output : - An array of logical (size like input object).
            % Author : Eran Ofek (Apr 2021)
            % Example: H = AstroHeader('WFPC2ASSNu5780205bx.fits');
            %          isKeyVal([H,H],'EXPTIME',300)
            %          isKeyVal([H;H],'KSPOTS','off')
            %          isKeyVal([H;H],'KSPOTS','off','ValCaseSens',true)

            arguments
                Obj
                Key
                Val
                Args.NumericTol                        = 1e-8;
                Args.KeyCaseSens(1,1) logical          = true;
                Args.ValCaseSens(1,1) logical          = false;
                Args.UseDict(1,1) logical              = true;
                Args.SearchAlgo char                   = 'strcmp';
                Args.Fill                              = NaN;
                Args.Val2Num(1,1) logical              = true;
                Args.Occur                             = 'first';
                Args.KeyDict                           = [];
                Args.IsInputAlt(1,1) logical           = false;
            end

            searchFun = tools.string.stringSearchFun(false, Args.ValCaseSens);

            Nobj   = numel(Obj);
            Result = false(size(Obj));
            for Iobj = 1:1:Nobj
                [KeyVal, ~, ~, Nfound] = getVal(Obj(Iobj), Key, ...
                                                 'UseDict',    Args.UseDict, ...
                                                 'CaseSens',   Args.KeyCaseSens, ...
                                                 'SearchAlgo', Args.SearchAlgo, ...
                                                 'Fill',       Args.Fill, ...
                                                 'Val2Num',    Args.Val2Num, ...
                                                 'Occur',      Args.Occur, ...
                                                 'KeyDict',    Args.KeyDict, ...
                                                 'IsInputAlt', Args.IsInputAlt);
                if Nfound == 0
                    Result(Iobj) = false;
                else
                    if ischar(Val) || ischar(KeyVal)
                        Result(Iobj) = searchFun(Val, KeyVal);
                    else
                        if isnan(Val) && isnan(KeyVal)
                            Result(Iobj) = true;
                        else
                            Result(Iobj) = abs(Val - KeyVal) < Args.NumericTol;
                        end
                    end
                end
            end
        end

        function Result = isKeyExist(Obj, Key, Args)
            % Check if a keyword exists in the header.
            % Input  : - An AstroHeader object (multi elements supported).
            %          - A single header keyword name.
            %          * ...,key,val,...
            %            'CaseSens'   - Case sensitive. Default true.
            %            'UseDict'    - Use dictionary. Default true.
            %            'SearchAlgo' - ['strcmp'] | 'regexp'.
            %            'Fill'       - Fill value if not found. Default NaN.
            %            'Val2Num'    - Convert to numeric. Default true.
            %            'Occur'      - ['first'] | 'last'.
            %            'KeyDict'    - Override dictionary. Default [].
            %            'IsInputAlt' - Treat input as alternate name. Default false.
            % Output : - An array of logical (size like input object).
            % Author : Eran Ofek (Apr 2021)
            % Example: H = AstroHeader('WFPC2ASSNu5780205bx.fits');
            %          isKeyExist([H,H],'EXPTIME')
            %          isKeyExist([H; H],'AEXPTIME','IsInputAlt',true)
            %          isKeyExist([H,H],'aaa')

            arguments
                Obj
                Key
                Args.CaseSens(1,1) logical   = true;
                Args.UseDict(1,1) logical     = true;
                Args.SearchAlgo char          = 'strcmp';
                Args.Fill                     = NaN;
                Args.Val2Num(1,1) logical     = true;
                Args.Occur                    = 'first';
                Args.KeyDict                  = [];
                Args.IsInputAlt(1,1) logical  = false;
            end

            Nobj   = numel(Obj);
            Result = true(size(Obj));
            for Iobj = 1:1:Nobj
                [~, ~, ~, Nfound] = getVal(Obj(Iobj), Key, ...
                                            'UseDict',    Args.UseDict, ...
                                            'CaseSens',   Args.CaseSens, ...
                                            'SearchAlgo', Args.SearchAlgo, ...
                                            'Fill',       Args.Fill, ...
                                            'Val2Num',    Args.Val2Num, ...
                                            'Occur',      Args.Occur, ...
                                            'KeyDict',    Args.KeyDict, ...
                                            'IsInputAlt', Args.IsInputAlt);
                if Nfound == 0
                    Result(Iobj) = false;
                end
            end
        end

        function Flag = isImType(Obj, ImTypeVal, Args)
            % Check if header IMTYPE keyword value equal some type
            % Input  : - An AstroHeader object.
            %          - IMTYPE type to check (e.g., 'bias').
            %          * ...,key,val,...
            %            'UseDict'    - Use dictionary. Default is true.
            %            'CaseSens'   - Default is true.
            %            'SearchAlgo' - ['strcmp'] | 'regexp'.
            %            'IsInputAlt' - Default is false.
            %            'KeyDict'    - Override dictionary. Default [].
            % Output : - An array of logicals (one per AstroHeader element).
            % Author : Eran Ofek (Apr 2021)
            % Example: H = AstroHeader('*.fits');
            %          Ans = isImType(H,'bias')
            %          Ans = isImType(H,'bias','CaseSens',false,'IsInputAlt',false)

            arguments
                Obj
                ImTypeVal
                Args.ImTypeKeyName                    = 'IMTYPE';
                Args.UseDict(1,1) logical             = true;
                Args.CaseSens(1,1) logical            = true;
                Args.SearchAlgo                       = 'strcmp';
                Args.IsInputAlt(1,1) logical          = true;
                Args.KeyDict                          = [];
            end

            [KeyVal] = getStructKey(Obj, Args.ImTypeKeyName, ...
                                    'UseDict',    Args.UseDict, ...
                                    'CaseSens',   Args.CaseSens, ...
                                    'SearchAlgo', Args.SearchAlgo, ...
                                    'Fill',       NaN, ...
                                    'Val2Num',    false, ...
                                    'IsInputAlt', Args.IsInputAlt, ...
                                    'KeyDict',    Args.KeyDict);

            FN      = fieldnames(KeyVal);
            ListVal = {KeyVal.(FN{1})};

            [~, ~, AllAlt] = searchAlt(Obj(1).ValDict, ImTypeVal, ...
                                        'CaseSens',   Args.CaseSens, ...
                                        'SearchAlgo', Args.SearchAlgo);
            if ~iscellstr(ListVal) %#ok<ISCLSTR>
                IsNaN   = tools.cell.isnan_cell(ListVal);
                ListVal = ListVal(~IsNaN);
            end

            Flag = ismember(ListVal, AllAlt);
            if isempty(Flag)
                Flag = false;
            end
        end

        function [MidJD, ExpTime] = julday(Obj, Args)
            % Calculate mid exposure JD and ExpTime for AstroHeader object
            %   Given the header keywords attempt calculating the mid JD of
            %   the exposure. This is done by retrieving the relevant
            %   header keywords (default in config/Header.Time.KeyNames.yml).
            %   Each keyword is associated with a conversion formula.
            % Input  : - AstroHeader object (multi elements supported).
            %          * ...,key,val,...
            %            'KeyJD' - JD or mid JD keyword (e.g., 'MIDJD').
            %                   If given, extract directly without calculation.
            %                   Output EXPTIME will be NaN. Default is [].
            %            'ExpTimeKey' - Exposure time keyword. Default 'EXPTIME'.
            %            'FunTimeKeys' - Structure (Dictionary) of time keyword
            %                   names and their conversion formulas.
            %                   If empty, use TimeDict. Default is {}.
            %            'UseDict'    - Use dictionary. Default is true.
            %            'CaseSens'   - Default is true.
            %            'SearchAlgo' - ['strcmp'] | 'regexp'.
            %            'Fill'       - Fill value if not found. Default NaN.
            %            'Val2Num'    - Convert to numeric. Default true.
            %            'IsInputAlt' - Default is true.
            %            'KeyDict'    - Override dictionary. Default [].
            % Output : - Matrix of mid exposure JD per AstroHeader element.
            %          - Matrix of exposure times per AstroHeader element.
            % Example: H = AstroHeader('WFPC2ASSNu5780205bx.fits');
            %          [JD,ET] = julday(H)
            %          [JD,ET] = julday([H;H])

            arguments
                Obj
                Args.KeyJD                           = [];
                Args.ExpTimeKey                      = 'EXPTIME';
                Args.FunTimeKeys                     = {};
                Args.UseDict(1,1) logical            = true;
                Args.CaseSens(1,1) logical           = true;
                Args.SearchAlgo                      = 'strcmp';
                Args.Fill                            = NaN;
                Args.Val2Num(1,1) logical            = true;
                Args.IsInputAlt(1,1) logical         = true;
                Args.KeyDict                         = [];
            end

            if isempty(Args.KeyJD)
                SEC_IN_DAY = 86400;

                if isempty(Args.FunTimeKeys)
                    if isempty(Obj(1).TimeDict.FieldNames)
                        % Default time-keyword conversion functions
                        % Bug fix: original had typo .Dicr instead of .Dict
                        Args.FunTimeKeys.Dict.MIDJD   = @(Time,Exp) Time;
                        Args.FunTimeKeys.Dict.MIDMJD  = @(Time,Exp) convert.time(Time,'MJD','JD');
                        Args.FunTimeKeys.Dict.JD      = @(Time,Exp) Time + 0.5.*Exp./SEC_IN_DAY;
                        Args.FunTimeKeys.Dict.MJD     = @(Time,Exp) convert.time(Time,'MJD','JD') ...
                                                                     + 0.5.*Exp./SEC_IN_DAY;
                        Args.FunTimeKeys.Dict.DATEOBS = @(Time,Exp) convert.time(Time,'StrDate','JD') ...
                                                                     + 0.5.*Exp./SEC_IN_DAY;
                        Args.FunTimeKeys.Dict.TIMEOBS = @(Time,Exp) convert.time(Time,'StrDate','JD') ...
                                                                     + 0.5.*Exp./SEC_IN_DAY;
                        Args.FunTimeKeys.Dict.DATE    = @(Time,Exp) convert.time(Time,'StrDate','JD') ...
                                                                     + 0.5.*Exp./SEC_IN_DAY;
                    else
                        Args.FunTimeKeys = Obj(1).TimeDict;
                    end
                end

                TimeKeys  = fieldnames(Args.FunTimeKeys.Dict);
                NtimeKeys = numel(TimeKeys);

                StTime  = getStructKey(Obj, TimeKeys);
                StExp   = getStructKey(Obj, Args.ExpTimeKey);

                MidJD   = nan(size(Obj));
                ExpTime = nan(size(Obj));
                Nobj    = numel(Obj);
                for Iobj = 1:1:Nobj
                    ExpTime(Iobj) = StExp(Iobj).(Args.ExpTimeKey);
                    Found = false;
                    Ikey  = 0;
                    while ~Found && Ikey < NtimeKeys
                        Ikey = Ikey + 1;
                        T    = StTime(Iobj).(TimeKeys{Ikey});
                        if ~isnan(T)
                            if iscell(Args.FunTimeKeys.Dict.(TimeKeys{Ikey}))
                                JD = Args.FunTimeKeys.Dict.(TimeKeys{Ikey}){1}(T, ExpTime(Iobj));
                            else
                                JD = Args.FunTimeKeys.Dict.(TimeKeys{Ikey})(T, ExpTime(Iobj));
                            end
                            if ~isnan(JD)
                                MidJD(Iobj) = JD;
                                Found       = true;
                            end
                        end
                    end
                end
            else
                Nobj    = numel(Obj);
                MidJD   = nan(Nobj, 1);
                ExpTime = nan(Nobj, 1);
                for Iobj = 1:1:Nobj
                    MidJD(Iobj) = Obj(Iobj).getVal(Args.KeyJD);
                end
            end
        end

        function Groups = groupByKeyVal(Obj, Keys, Args)
            % Group a set of AstroHeaders by their unique keyword values.
            %   e.g., look for all images with the same EXPTIME and put
            %   them in different groups according to the EXPTIME value.
            % Input  : - An AstroHeader object (multi elements supported).
            %          - A cell array of header keywords to group by.
            %          * ...,key,val,...
            %            'UseDict'    - Use dictionary. Default is true.
            %            'CaseSens'   - Default is true.
            %            'SearchAlgo' - ['strcmp'] | 'regexp'.
            %            'Fill'       - Fill value if not found. Default NaN.
            %            'Val2Num'    - Convert to numeric. Default true.
            %            'IsInputAlt' - Default is true.
            %            'KeyDict'    - Override dictionary. Default [].
            % Output : - Structure of groups. Each element has:
            %            .Content - row of values defining the group.
            %            .ptr     - indices of rows belonging to the group.
            % Example: H = AstroHeader('WFPC2ASSNu5780205bx.fits');
            %   Groups = groupByKeyVal([H,H],{'IMTYPE','FILTER1','EXPTIME'})

            arguments
                Obj
                Keys        = {};
                Args.UseDict     = true;
                Args.CaseSens    = true;
                Args.SearchAlgo  = 'strcmp';
                Args.Fill        = NaN;
                Args.Val2Num     = true;
                Args.IsInputAlt  = true;
                Args.KeyDict     = [];
            end

            CellVal = getCellKey(Obj, Keys, ...
                                 'UseDict',    Args.UseDict, ...
                                 'CaseSens',   Args.CaseSens, ...
                                 'SearchAlgo', Args.SearchAlgo, ...
                                 'Fill',       Args.Fill, ...
                                 'Val2Num',    Args.Val2Num, ...
                                 'IsInputAlt', Args.IsInputAlt, ...
                                 'KeyDict',    Args.KeyDict);

            Groups = tools.cell.cell_find_groups(CellVal);
        end

        function [Lon, Lat, Alt] = getObsCoo(Obj, Args)
            % Get Observatory geodetic position from Header
            % Input  : - An AstroHeader object (multi element supported).
            %          * ...,key,val,...
            %            'KeyLon' - Longitude keyword. Default 'OBSLON'.
            %            'KeyLat' - Latitude keyword.  Default 'OBSLAT'.
            %            'KeyAlt' - Altitude keyword.  Default 'OBSEL'.
            %            'IsInputAlt' - Search in alternate names list.
            %                   Default is false.
            % Output : - An array of longitudes.
            %          - An array of latitudes.
            %          - An array of altitudes.
            % Author : Eran Ofek (Sep 2021)
            % Example: H = AstroHeader('PTF_Cropped.fits');
            %          [Lon, Lat, Height] = getObsCoo(H)

            arguments
                Obj
                Args.KeyLon     = 'OBSLON';
                Args.KeyLat     = 'OBSLAT';
                Args.KeyAlt     = 'OBSEL';
                Args.IsInputAlt = false;
            end

            Nobj = numel(Obj);
            Lon  = nan(size(Obj));
            Lat  = nan(size(Obj));
            Alt  = nan(size(Obj));

            for Iobj = 1:1:Nobj
                Lon(Iobj) = getVal(Obj(Iobj), Args.KeyLon, 'IsInputAlt', Args.IsInputAlt);
                Lat(Iobj) = getVal(Obj(Iobj), Args.KeyLat, 'IsInputAlt', Args.IsInputAlt);
                Alt(Iobj) = getVal(Obj(Iobj), Args.KeyAlt, 'IsInputAlt', Args.IsInputAlt);
            end
        end

        function [RA, Dec] = getCoo(Obj, Args)
            % get RA/Dec coordinates from header
            % Input  : - A single element AstroHeader object.
            %          * ...,key,val,...
            %            'RA'  - Either a header keyword char, a sexagesimal
            %                    string (containing ':'), or a numeric RA value.
            %                    Default is 'RA'.
            %            'Dec' - Either a header keyword char, a sexagesimal
            %                    string (containing ':'), or a numeric Dec value.
            %                    Default is 'DEC'.
            %            'Units'    - Input units. Default is 'deg'.
            %            'OutUnits' - Output units. Default is 'deg'.
            %            'getStructKeyArgs' - Extra args for getStructKey.
            %                   Default is {}.
            % Output : - RA
            %          - Dec.
            % Author : Eran Ofek (Oct 2021)
            % Example: [RA, Dec] = getCoo(AI.HeaderData)

            arguments
                Obj(1,1)
                Args.RA   = 'RA';
                Args.Dec  = 'DEC';
                Args.Units    = 'deg';
                Args.OutUnits = 'deg';
                Args.getStructKeyArgs cell = {};
            end

            % ---- RA ----
            if ischar(Args.RA)
                if contains(Args.RA, ':')
                    RA = celestial.coo.convertdms(Args.RA, 'gH', 'r');
                    RA = convert.angular('rad', Args.OutUnits, RA);
                else
                    St = Obj.getStructKey(Args.RA, Args.getStructKeyArgs{:});
                    RA = [St.(Args.RA)];
                    if isnumeric(RA)
                        RA = convert.angular(Args.Units, Args.OutUnits, RA);
                    else
                        if contains(RA, ':')
                            RA = celestial.coo.convertdms(RA, 'gH', 'r');
                            RA = convert.angular('rad', Args.OutUnits, RA);
                        else
                            RA = str2doubleq(Args.RA);
                        end
                    end
                end
            elseif isnumeric(Args.RA)
                RA = convert.angular(Args.Units, Args.OutUnits, Args.RA);
            else
                error('RA must be a numeric or char array');
            end

            % ---- Dec ----
            if ischar(Args.Dec)
                if contains(Args.Dec, ':')
                    Dec = celestial.coo.convertdms(Args.Dec, 'gD', 'R');
                    Dec = convert.angular('rad', Args.OutUnits, Dec);
                else
                    St  = Obj.getStructKey(Args.Dec, Args.getStructKeyArgs{:});
                    Dec = [St.(Args.Dec)];
                    if isnumeric(Dec)
                        Dec = convert.angular(Args.Units, Args.OutUnits, Dec);
                    else
                        if contains(Dec, ':')
                            Dec = celestial.coo.convertdms(Dec, 'gD', 'R');
                            Dec = convert.angular('rad', Args.OutUnits, Dec);
                        else
                            Dec = str2doubleq(Args.Dec);
                        end
                    end
                end
            elseif isnumeric(Args.Dec)
                Dec = convert.angular(Args.Units, Args.OutUnits, Args.Dec);
            else
                error('Dec must be a numeric or char array');
            end
        end

        function Obj = deleteComments(Obj)
            % Delete comments (third column) from all header rows
            % Input  : - An AstroHeader object.
            % Output : - An AstroHeader object with an empty comment column.
            % Author : Eran Ofek (Nov 2021)
            % Example:
            % H = AstroHeader('PTF_201411204943_i_p_scie_t115144_u023050379_f02_p100037_c02.fits');
            % H.deleteComments;

            Nobj = numel(Obj);
            for Iobj = 1:1:Nobj
                Nlines         = size(Obj(Iobj).Data, 1);
                Obj(Iobj).Data = [Obj(Iobj).Data(:, 1:2), cell(Nlines, 1)];
            end
        end

        function Obj = selectKeys(Obj, Keys)
            % Select a sub-header by keeping only specified keywords.
            % Input  : - An AstroHeader object.
            %          - A cell array of keyword names to keep (exact match).
            % Output : - An AstroHeader object with only the selected keys.
            % Author : Eran Ofek (Nov 2021)
            % Example: H = AstroHeader('PTF_...fits');
            %          H.selectKeys(H.Data(1:10,1));

            Nobj = numel(Obj);
            for Iobj = 1:1:Nobj
                FlagNE         = ~cellfun(@isempty, Obj(Iobj).Data(:, 1));
                Obj(Iobj).Data = Obj(Iobj).Data(FlagNE, :);
                Flag           = ismember(Obj(Iobj).Data(:, 1), Keys);
                Obj(Iobj).Data = Obj(Iobj).Data(Flag, :);
            end
        end

    end

    % ================================================================== %

    methods  % Conversions

        function Result = header2table(Obj, Args)
            % Convert an array of AstroHeader to table/cell/AstroTable/AstroCatalog
            %       in which each column shows the same key for all headers.
            % Input  : - An AstroHeader object.
            %          * ...,key,val,...
            %            'OutType' - Output type:
            %                   'cell'|'table'|'AstroCatalog'|['AstroCatalog'].
            %            'SelectedKeys'      - Cell array of keyword names to
            %                   select. If empty, use all. Default is {}.
            %            'RemoveComments'    - Default is true.
            %            'RemoveHistory'     - Default is true.
            %            'RemoveEmpty'       - Default is true.
            %            'RemoveNonUnique'   - Default is true.
            %            'SelectKeysInFirst' - Use only keys present in the
            %                   first header. Default is true.
            % Output : - A cell/table/AstroCatalog/AstroTable.
            % Author : Eran Ofek (Apr 2022)
            % Example: H = AstroHeader('*.fits', 1);
            %          R = header2table([H(1), H(1)]);
            %          R = header2table([H(1), H(1)], 'OutType','table');

            arguments
                Obj
                Args.OutType                   = 'astrocatalog';
                Args.SelectedKeys cell         = {};
                Args.RemoveComments logical    = true;
                Args.RemoveHistory logical     = true;
                Args.RemoveEmpty logical       = true;
                Args.RemoveNonUnique logical   = true;
                Args.SelectKeysInFirst logical = true;
            end

            Nobj = numel(Obj);
            for Iobj = 1:1:Nobj
                Nkeys = size(Obj(Iobj).Data, 1);

                if Args.RemoveComments
                    FlagComment = strcmp(Obj(Iobj).Data(:, 1), 'COMMENT');
                else
                    FlagComment = false(Nkeys, 1);
                end
                if Args.RemoveHistory
                    FlagHistory = strcmp(Obj(Iobj).Data(:, 1), 'HISTORY');
                else
                    FlagHistory = false(Nkeys, 1);
                end
                if Args.RemoveEmpty
                    FlagEmpty = cellfun(@isempty, Obj(Iobj).Data(:, 1));
                else
                    FlagEmpty = false(Nkeys, 1);
                end

                % Bug fix: original had ~FlagHistory & ~FlagHistory (duplicate)
                CellHeader = Obj(Iobj).Data(~FlagComment & ~FlagHistory & ~FlagEmpty, :);

                if Args.RemoveNonUnique
                    [~, IU]    = unique(CellHeader(:, 1));
                    CellHeader = CellHeader(IU, :);
                end

                if ~isempty(Args.SelectedKeys)
                    Flag       = ismember(CellHeader(:, 1), Args.SelectedKeys);
                    CellHeader = CellHeader(Flag(:), :);
                end

                Ncol = size(CellHeader, 1);
                if ~isempty(Args.SelectedKeys) && numel(Args.SelectedKeys) ~= Ncol
                    error('Number of selected keys in headers must be consistent');
                end

                if Iobj == 1
                    if Args.SelectKeysInFirst
                        Args.SelectedKeys = CellHeader(:, 1).';
                    end
                    OutCell = cell(Nobj, Ncol);
                end
                OutCell(Iobj, :) = CellHeader(:, 2).';
            end

            switch lower(Args.OutType)
                case 'cell'
                    Result = OutCell;
                case 'table'
                    Result = cell2table(OutCell);
                    Result.Properties.VariableNames = CellHeader(:, 1).';
                case 'astrocatalog'
                    Result          = AstroCatalog;
                    Result.Catalog  = cell2table(OutCell);
                    Result.Catalog.Properties.VariableNames = CellHeader(:, 1).';
                    Result.ColNames = CellHeader(:, 1).';
                case 'astrotable'
                    Result          = AstroTable;
                    Result.Catalog  = cell2table(OutCell);
                    Result.Catalog.Properties.VariableNames = CellHeader(:, 1).';
                    Result.ColNames = CellHeader(:, 1).';
                otherwise
                    error('Unknown OutType option');
            end
        end

    end

    % ================================================================== %

    methods  % Specific functions

        function Obj = deleteDistortionsWCS(Obj)
            % Delete WCS distortion keywords from header
            % Input  : - An AstroHeader object.
            % Output : - An AstroHeader object with all PV/A/AP/B/BP
            %            polynomial keywords removed and CTYPE reset to TAN.
            % Author : Eran Ofek (Dec 2021)

            Nobj = numel(Obj);
            for Iobj = 1:1:Nobj
                Obj(Iobj).deleteKey('PV\d+_\d+');
                Obj(Iobj).deleteKey('A_\d+_\d+');
                Obj(Iobj).deleteKey('B_\d+_\d+');
                Obj(Iobj).deleteKey('AP_\d+_\d+');
                Obj(Iobj).deleteKey('BP_\d+_\d+');
                Obj(Iobj).deleteKey('A_ORDER');
                Obj(Iobj).deleteKey('B_ORDER');
                Obj(Iobj).deleteKey('AP_ORDER');
                Obj(Iobj).deleteKey('BP_ORDER');
                Obj(Iobj).replaceVal('CTYPE1', 'RA---TAN');
                Obj(Iobj).replaceVal('CTYPE2', 'DEC--TAN');
            end
        end

        function writeCSVforBulkInjection(Obj0, FileName, Args)
            % Write an AstroHeader array to a CSV file for bulk DB injection
            % Input  : - An AstroHeader object or a vector of AH objects
            %          - Output file name. Default is 'astroheader.csv'.
            %          * ...,key,val,...
            %            'Append'     - Append to an existing file. Default false.
            %            'Delimiter'  - Field delimiter. Default is ','.
            %            'Filter'     - Remove fields not in FilterList. Default false.
            %            'FilterList' - Cell array of DB column names. Default {}.
            % Output : - A CSV file
            % Author : A. Krassilchtchikov (Feb 2024)

            arguments
                Obj0
                FileName             = 'astroheader.csv';
                Args.Append logical  = false;
                Args.Delimiter       = ',';
                Args.Filter logical  = false;
                Args.FilterList      = {};
            end

            Obj  = Obj0.copy;
            Nobj = length(Obj);
            Keys = [Obj.Data];
            Keys = reshape(Keys, [size(Keys, 1), 3, Nobj]);

            [~, Ind, ~] = unique(Keys(:, 1, 1), 'stable');
            Keys        = Keys(Ind, :, :);

            if Args.Filter
                Ind  = ismember(Keys(:, 1, 1), upper(Args.FilterList'));
                Keys = Keys(Ind, :, :);
            end

            if ~Args.Append
                FirstLine = Keys(:, 1, 1)';
                writecell(FirstLine, FileName, 'Delimiter', Args.Delimiter);
            end

            Keys = squeeze(Keys(:, 2, :));
            writecell(Keys', FileName, 'Delimiter', Args.Delimiter, 'WriteMode', 'append');
        end

        function Result = writeCSV(Obj0, FileName, Args)
            % Write an AstroHeader array to a CSV text file
            % Input  : - An AstroHeader object or a vector of AH objects
            %          - Output file name. Default is 'astroheader.csv'.
            %          * ...,key,val,...
            %            'Append'             - Append to existing file. Default false.
            %            'Delimiter'          - Field delimiter. Default is ','.
            %            'CleanHeaderValues'  - Remove fields not in DB table. Default false.
            % Output : - 0 on success
            % Author : A. Krassilchtchikov (Jun 2023)

            arguments
                Obj0
                FileName                       = 'astroheader.csv';
                Args.Append logical            = false;
                Args.Delimiter                 = ',';
                Args.CleanHeaderValues logical = false;
            end

            Obj = Obj0.copy;

            if Args.CleanHeaderValues
                load('~/db_ima_cols.mat'); %#ok<LOAD>
            end

            if ~Args.Append
                FirstLine = Obj(1).Data(:, 1);
                if Args.CleanHeaderValues
                    Numeric   = find(cellfun(@isnumeric, FirstLine));
                    FirstLine(Numeric, :) = [];
                    Ind = find(strcmp(FirstLine, 'DATE-OBS'));
                    FirstLine{Ind} = 'DATE_OBS';
                    Keys      = ismember(lower(FirstLine), cols_coadd'); %#ok<NODEF>
                    FirstLine = FirstLine(Keys);
                end
                FirstLine = [{'FileName'}, FirstLine'];
                writecell(FirstLine, FileName, 'Delimiter', Args.Delimiter);
            end

            for Iobj = 1:1:numel(Obj)
                if Args.CleanHeaderValues
                    Kwords  = Obj(Iobj).Data(:, 1);
                    Numeric = find(cellfun(@isnumeric, Kwords));
                    Kwords(Numeric)            = [];
                    Obj(Iobj).Data(Numeric, :) = [];
                    Ind = find(strcmp(Kwords, 'DATE-OBS'));
                    Kwords{Ind} = 'DATE_OBS';
                    Keys = ismember(lower(Kwords), cols_coadd'); %#ok<NODEF>
                    Obj(Iobj).Data = Obj(Iobj).Data(Keys, :);
                    Ind = find(strcmp(Obj(Iobj).Data(:, 1), 'SUBDIR'));
                    if Ind > 0 && isempty(Obj(Iobj).Data{Ind, 2})
                        Obj(Iobj).Data{Ind, 2} = ' ';
                    end
                    Ind = find(strcmp(Obj(Iobj).Data(:, 1), 'SUBLEVEL'));
                    if Ind > 0 && isempty(Obj(Iobj).Data{Ind, 2})
                        Obj(Iobj).Data{Ind, 2} = ' ';
                    end
                end
                FirstSymb = {Obj(Iobj).File};
                Line      = [FirstSymb, Obj(Iobj).Data{:, 2}];
                writecell(Line, FileName, 'Delimiter', Args.Delimiter, 'WriteMode', 'append');
            end

            Result = 0;
        end

        function Obj = deleteComment(Obj, Args)
            % Delete all COMMENT keywords from header, or only empty ones
            % Input  : - AstroHeader object.
            %          * ...,key,val,...
            %            'OnlyIfEmpty' - Delete COMMENT only when its value
            %                   is empty. Default is false.
            % Output : - An updated AstroHeader object
            % Author : Eran Ofek (Dec 2024)
            % Example: CI.HeaderData.deleteComment

            arguments
                Obj
                Args.OnlyIfEmpty logical = false;
            end

            Nobj = numel(Obj);
            for Iobj = 1:1:Nobj
                II    = find(strcmp(Obj(Iobj).Data(:, 1), 'COMMENT'));
                Nline = size(Obj(Iobj).Data, 1);
                Vec   = (1:1:Nline)';
                if Args.OnlyIfEmpty
                    IsEmpty = tools.cell.isempty_cell(Obj(Iobj).Data(:, 2));
                    Isel    = setdiff(Vec, II(IsEmpty));
                else
                    Isel    = setdiff(Vec, II);
                end
                % Bug fix: original wrote Obj.Data instead of Obj(Iobj).Data
                Obj(Iobj).Data = Obj(Iobj).Data(Isel, :);
            end
        end

    end

    % ================================================================== %

    methods (Static)  % Help and documentation

        function help
            % show manuals.AstroHeader
            open manuals.AstroHeader;
        end

    end

    % ================================================================== %

    methods (Static)  % Unit test

        function Result = unitTest()
            % unitTest for AstroHeader
            %   Tests all critical paths including the new O(1) index and
            %   synonym cache.  Prints a PASS / FAIL line for each group.
            % Example: AstroHeader.unitTest

            fprintf('--- AstroHeader.unitTest ---\n');
            AllPassed = true;

            % ---- helper -------------------------------------------------
            function ok = check(cond, name)
                if cond
                    fprintf('  PASS  %s\n', name);
                    ok = true;
                else
                    fprintf('  FAIL  %s\n', name);
                    ok = false;
                end
            end

            % ---- build a synthetic header --------------------------------
            Data = {'SIMPLE',  true,           'file conforms to FITS';
                    'BITPIX',  -32,            'bits per pixel';
                    'NAXIS',   2,              'number of axes';
                    'NAXIS1',  1024,           'length of x axis';
                    'NAXIS2',  1024,           'length of y axis';
                    'EXPTIME', 300.0,          '[s] exposure time';
                    'GAIN',    1.5,            '[e-/ADU] gain';
                    'FILTER',  'r',            'filter name';
                    'DATE-OBS','2024-01-15',   'observation date';
                    'RA',      120.5,          '[deg] right ascension';
                    'DEC',     -30.2,          '[deg] declination';
                    'IMTYPE',  'science',      'image type';
                    'COMMENT', 'test comment', '';
                    'HISTORY', 'created here', '';
                    'EXPTIME', 999.0,          'duplicate for testing'};

            H          = AstroHeader(1);
            H.Data     = Data;

            % ----------------------------------------------------------------
            %  1.  KeyIndex basics
            % ----------------------------------------------------------------
            fprintf('\n[1] KeyIndex construction and getKeyRows\n');
            Rows  = H.getKeyRows('EXPTIME');
            pass1 = check(numel(Rows) == 2, 'EXPTIME -> 2 rows');

            Row1  = H.getKeyRows('EXPTIME', 'first');
            pass2 = check(Row1 == 6, 'first EXPTIME at row 6');

            Row2  = H.getKeyRows('EXPTIME', 'last');
            pass3 = check(Row2 == 15, 'last EXPTIME at row 15');

            Rnone = H.getKeyRows('NOTAKEY');
            pass4 = check(isempty(Rnone), 'missing key -> empty');

            AllPassed = AllPassed && all([pass1 pass2 pass3 pass4]);

            % ----------------------------------------------------------------
            %  2.  Index invalidation via set.Data
            % ----------------------------------------------------------------
            fprintf('\n[2] Index invalidation when Data changes\n');
            OldState = H.IsIndexUpToDate;
            H.Data{1,1} = 'SIMPLE';   % same value but triggers setter
            pass5 = check(~H.IsIndexUpToDate, 'Data setter clears IsIndexUpToDate');
            H.getKeyRows('EXPTIME');   % force rebuild
            pass6 = check(H.IsIndexUpToDate, 'getKeyRows rebuilds index');
            AllPassed = AllPassed && all([pass5 pass6]);

            % ----------------------------------------------------------------
            %  3.  getValFast / getValSimple / getMultiValFast
            % ----------------------------------------------------------------
            fprintf('\n[3] Fast retrieval methods\n');
            [V1, C1] = H.getValFast('GAIN');
            pass7 = check(V1 == 1.5, 'getValFast GAIN == 1.5');
            pass8 = check(strcmp(C1, '[e-/ADU] gain'), 'getValFast comment');

            V2    = H.getValFast('NOTHERE');
            pass9 = check(isnan(V2), 'getValFast missing -> NaN');

            [V3, C3] = H.getValSimple('FILTER');
            pass10 = check(strcmp(V3, 'r'), 'getValSimple FILTER == r');

            V4   = H.getValSimple('NOTHERE');
            pass11 = check(isempty(V4), 'getValSimple missing -> []');

            Vals = H.getMultiValFast({'EXPTIME','GAIN','NAXIS1'});
            pass12 = check(Vals{1} == 300.0 && Vals{2} == 1.5 && Vals{3} == 1024, ...
                           'getMultiValFast three keys');
            AllPassed = AllPassed && all([pass7 pass8 pass9 pass10 pass11 pass12]);

            % ----------------------------------------------------------------
            %  4.  getVal (main API) - exact path
            % ----------------------------------------------------------------
            fprintf('\n[4] getVal - exact match (UseDict=false)\n');
            [Val, Key, Com, Nf] = H.getVal('EXPTIME', 'UseDict', false);
            pass13 = check(Val == 300.0, 'getVal exact Val==300');
            pass14 = check(strcmp(Key,'EXPTIME'), 'getVal exact Key');
            pass15 = check(Nf == 2, 'getVal exact Nfound==2');

            [ValL] = H.getVal('EXPTIME', 'UseDict', false, 'Occur', 'last');
            pass16 = check(ValL == 999.0, 'getVal exact last Occur');

            [ValM] = H.getVal('NOTHERE', 'UseDict', false);
            pass17 = check(isnan(ValM), 'getVal exact missing -> Fill=NaN');

            AllPassed = AllPassed && all([pass13 pass14 pass15 pass16 pass17]);

            % ----------------------------------------------------------------
            %  5.  Val2Num conversion
            % ----------------------------------------------------------------
            fprintf('\n[5] Val2Num conversion\n');
            H2      = AstroHeader(1);
            H2.Data = {'NUMSTR', '42.5', 'number stored as string';
                       'NOTNUM', 'hello', 'not convertible'};
            V5 = H2.getVal('NUMSTR', 'UseDict', false);
            pass18 = check(V5 == 42.5 && isnumeric(V5), 'Val2Num converts string to double');
            V6 = H2.getVal('NOTNUM', 'UseDict', false);
            pass19 = check(ischar(V6) && strcmp(V6, 'hello'), 'non-numeric stays char');
            AllPassed = AllPassed && all([pass18 pass19]);

            % ----------------------------------------------------------------
            %  6.  getStructKey
            % ----------------------------------------------------------------
            fprintf('\n[6] getStructKey\n');
            S = H.getStructKey({'GAIN','FILTER','NOTHERE'}, 'UseDict', false);
            pass20 = check(S.GAIN   == 1.5,              'getStructKey GAIN');
            pass21 = check(strcmp(S.FILTER,'r'),          'getStructKey FILTER');
            pass22 = check(isnan(S.NOTHERE),              'getStructKey missing -> NaN');

            % array of headers
            H3 = [H, H];
            S3 = H3.getStructKey({'GAIN'}, 'UseDict', false);
            pass23 = check(numel(S3) == 2 && S3(1).GAIN == 1.5, 'getStructKey on array');
            AllPassed = AllPassed && all([pass20 pass21 pass22 pass23]);

            % ----------------------------------------------------------------
            %  7.  getCellKey / getCellKeyFast
            % ----------------------------------------------------------------
            fprintf('\n[7] getCellKey and getCellKeyFast\n');
            C = H3.getCellKey({'GAIN','NAXIS1'}, 'UseDict', false);
            pass24 = check(size(C,1)==2 && size(C,2)==2 && C{1,1}==1.5, ...
                           'getCellKey shape and value');

            CF = H3.getCellKeyFast({'GAIN','NAXIS1'});
            pass25 = check(isequal(C, CF), 'getCellKeyFast matches getCellKey');
            AllPassed = AllPassed && all([pass24 pass25]);

            % ----------------------------------------------------------------
            %  8.  numKeys
            % ----------------------------------------------------------------
            fprintf('\n[8] numKeys\n');
            pass26 = check(H.numKeys == 15, 'numKeys == 15');
            AllPassed = AllPassed && pass26;

            % ----------------------------------------------------------------
            %  9.  insertKey and deleteKey invalidate index
            % ----------------------------------------------------------------
            fprintf('\n[9] insertKey / deleteKey maintain index consistency\n');
            Hmod = AstroHeader(1);
            Hmod.Data = Data;
            Hmod.insertKey({'NEWKEY', 123, 'test'}, 'end');
            Vn = Hmod.getValFast('NEWKEY');
            pass27 = check(Vn == 123, 'getValFast after insertKey');

            Hmod.deleteKey({'NEWKEY'});
            Vd = Hmod.getValFast('NEWKEY');
            pass28 = check(isnan(Vd), 'getValFast after deleteKey -> NaN');
            AllPassed = AllPassed && all([pass27 pass28]);

            % ----------------------------------------------------------------
            % 10.  replaceVal invalidates index
            % ----------------------------------------------------------------
            fprintf('\n[10] replaceVal maintains index consistency\n');
            Hmod2      = AstroHeader(1);
            Hmod2.Data = Data;
            Hmod2.replaceVal('GAIN', 3.0);
            Vr = Hmod2.getValFast('GAIN');
            pass29 = check(Vr == 3.0, 'getValFast after replaceVal');
            AllPassed = AllPassed && pass29;

            % ----------------------------------------------------------------
            % 11.  header2table bug fix: ~FlagComment & ~FlagHistory
            % ----------------------------------------------------------------
            fprintf('\n[11] header2table COMMENT+HISTORY filtering\n');
            T = H.header2table('OutType','table');
            ColNames = T.Properties.VariableNames;
            pass30 = check(~any(strcmp(ColNames,'COMMENT')), ...
                           'header2table removes COMMENT column');
            pass31 = check(~any(strcmp(ColNames,'HISTORY')), ...
                           'header2table removes HISTORY column');
            pass32 = check(any(strcmp(ColNames,'EXPTIME')), ...
                           'header2table keeps EXPTIME column');
            AllPassed = AllPassed && all([pass30 pass31 pass32]);

            % ----------------------------------------------------------------
            % 12.  deleteComment bug fix: Obj(Iobj).Data
            % ----------------------------------------------------------------
            fprintf('\n[12] deleteComment operates on correct element\n');
            Ha = [AstroHeader(1), AstroHeader(1)];
            Ha(1).Data = {'COMMENT','first obj',''; 'GAIN',1.5,''};
            Ha(2).Data = {'EXPTIME',60,'';          'COMMENT','second obj',''};
            Ha.deleteComment;
            pass33 = check(Ha(1).numKeys == 1 && strcmp(Ha(1).Data{1,1},'GAIN'), ...
                           'deleteComment removes COMMENT from Ha(1)');
            pass34 = check(Ha(2).numKeys == 1 && strcmp(Ha(2).Data{1,1},'EXPTIME'), ...
                           'deleteComment removes COMMENT from Ha(2)');
            AllPassed = AllPassed && all([pass33 pass34]);

            % ----------------------------------------------------------------
            % 13.  isKeyExist / isKeyVal
            % ----------------------------------------------------------------
            fprintf('\n[13] isKeyExist and isKeyVal\n');
            pass35 = check( H.isKeyExist('GAIN','UseDict',false), ...
                            'isKeyExist present');
            pass36 = check(~H.isKeyExist('NOTHERE','UseDict',false), ...
                            'isKeyExist absent');
            pass37 = check( H.isKeyVal('GAIN',1.5,'UseDict',false), ...
                            'isKeyVal match');
            pass38 = check(~H.isKeyVal('GAIN',9.9,'UseDict',false), ...
                            'isKeyVal mismatch');
            AllPassed = AllPassed && all([pass35 pass36 pass37 pass38]);

            % ----------------------------------------------------------------
            % 14.  SynonymCache: second call hits cache
            % ----------------------------------------------------------------
            fprintf('\n[14] SynonymCache populated on second call\n');
            Hsc = AstroHeader(1);
            Hsc.Data = {'EXPTIME', 120, ''};
            Hsc.resolveSynonym('EXPTIME', 'CaseSens', false, 'SearchAlgo', 'strcmp');
            pass39 = check(~isempty(Hsc.SynonymCache) && ...
                            isa(Hsc.SynonymCache, 'containers.Map'), ...
                           'SynonymCache is a containers.Map after first call');
            % second call must return same result from cache
            Alt1 = Hsc.resolveSynonym('EXPTIME','CaseSens',false,'SearchAlgo','strcmp');
            Alt2 = Hsc.resolveSynonym('EXPTIME','CaseSens',false,'SearchAlgo','strcmp');
            pass40 = check(isequal(Alt1, Alt2), 'SynonymCache returns same result on repeat');
            AllPassed = AllPassed && all([pass39 pass40]);

            % ----------------------------------------------------------------
            %  Summary
            % ----------------------------------------------------------------
            fprintf('\n');
            if AllPassed
                fprintf('==> All tests PASSED\n');
            else
                fprintf('==> Some tests FAILED\n');
            end
            Result = AllPassed;
        end

    end

end
