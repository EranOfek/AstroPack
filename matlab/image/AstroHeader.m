% AstroHeader Class - A container for astronomical image/tables headers
% Properties:
%   Data - a 3 columns cell array of [key,val,comment]
%   Key  - A structure array containing on the fly-generated header
%   File 
%   HDU
%   KeyDict - Dictionary of key names synonyms
%   ValDict - Dictionary of key values (IMTYPE) synonyms
%   CommentDict - Dictionary of default comments
%   TimeDict - Dictionary of time keywords and their conversion functions.
%   IsKeyUpToDate - Is Ket structure up to date.
% Functionality:
%   read - read as a single/multiple headers from file/s into an Header object
%   show - dipslay default
%   createBasicHeader (static) - Create an AstroHeader object with a basic header
%   funUnary - funUnary for AstroHeader - modify header and add history
%   getVal - get a single keyword value where the keyword appears first in a dictionary.
%   getStructKey - Get multiple  keys from multiple headers and store in a structure array
%   getCellKey - Get multiple  keys from multiple headers and store in a cell array
%   insertDefaultComments - Insert/replace default comments for keys using the header comments dictionary
%   deleteKey - Delete keywords from header by exact keyword name
%   insertKey - Insert key/val/comment to headers
%   replaceVal - Replace a keyword value in headers (no dictionary in key
%   isKeyVal - Check if a single keyword value equal to some value.
%   isKeyExist - Check if a single keyword value equal to some value.
%   isImType - Check if header IMTYPE keyword value equal some type
%   julday - Calculate mid exposure JD and ExpTime for AstroHeader object
%   groupByKeyVal - Group a set of AstroHeaders by their unique keyword values.
%   unitTest (static) - unitTest for AstroHeader class
%

classdef AstroHeader < Component
    % Properties
    properties (SetAccess = public)
        Data(:,3) cell            = cell(0,3);
        Key struct                = struct(); 
        
        File                      = '';
        HDU                       = ''; % HDU or dataset
        
        KeyDict Dictionary        % Initialization is done in constructor
        ValDict Dictionary        % Initialization is done in constructor
        CommentDict Dictionary    % Initialization is done in constructor
        TimeDict Dictionary       % Initialization is done in constructor
    end
    properties (Hidden, SetAccess=private)
        IsKeyUpToDate(1,1) logical    = true; % this is used by get.Key in order to avoid reformatting the cell array into a structure everytime.
    end
    
    properties (Constant, Hidden)
        ColKey     = 1;
        ColVal     = 2;
        ColComment = 3;
    end
    
%         filename        
%         configPath = "";
%         data
%         lines
%         userData
%         
%         inputImagePath
%         inputImageExt
    
    

    methods
        % Constructor    
        function Obj = AstroHeader(FileNames,HDU,Args)
            % Construct AstroHeader object and populate it with headers
            % Input  : - Either a vector of the the size of the empty
            %            AstroHeader object (e.g., [2 2]).
            %            OR a file name with wild cards or regular
            %            expression from which multiple files will be
            %            searched.
            %            OR a cell array of strings array of file names.
            %            Default is 1.
            %          - The index of HDU from which to read the header.
            %            This can be a vector or a scalar.
            %            Default is 1.
            %          * ...,key,val,...
            %            'UseRegExp' - Logical indicating if to use regexp (true)
            %                   or wild cards (false). Default is false.
            % Output : - An AstroHeader object with populated headers.
            % Author : Eran Ofek (Mar 2021)
            % Example: 
            
            arguments
                FileNames      = 1;   % name or array size
                HDU            = 1;
                Args.UseRegExp(1,1) logical = false;
            end
            
            if isnumeric(FileNames)
                % create an empty AstroHeader object
                List = cell(FileNames);
                
            elseif iscell(FileNames) || isstring(FileNames)
                % User provided a list of file names
                List = FileNames;
            else
                % read file names
                List = io.files.filelist(FileNames,Args.UseRegExp);
            end
            
                
            Nh = numel(List);
            for Ih=1:1:Nh
                Obj(Ih).File = List{Ih};
            end
            Obj = reshape(Obj,size(List));
            
            Nhdu = numel(HDU);
            % read files
            for Ih=1:1:Nh
                if ~isempty(Obj(Ih).File)
                    Ihdu = min(Ih,Nhdu);
                    Obj(Ih).Data = FITS.readHeader1(Obj(Ih).File,HDU(Ihdu));
                end
                Obj(Ih).KeyDict     = Dictionary('DictName','Header.Synonyms.KeyNames');
                Obj(Ih).ValDict     = Dictionary('DictName','Header.Synonyms.KeyVal.IMTYPE');
                Obj(Ih).CommentDict = Dictionary('DictName','Header.Comments.Default');
                Obj(Ih).TimeDict    = Dictionary('DictName','Header.Time.KeyNames');
                Obj(Ih).TimeDict    = string2funHandle(Obj(Ih).TimeDict);
            end
               
        end
    end
    
    methods % setters/getters
        function KeyS=get.Key(Obj)
            % getter for Key, generate key structure array if needed
            
            Iobj = 1;
            if Obj(Iobj).IsKeyUpToDate
                % Key structure is up to date
                KeyS = Obj(Iobj).Key; % rucusive????
            else

                % read cell to struct again
                KeyS = imUtil.headerCell.cellhead2struct(Obj(Iobj).Data);
                Obj(Iobj).Key = KeyS;
                Obj(Iobj).IsKeyUpToDate = true;
            end
            % return structure array of keys
            KeyS(Iobj) = KeyS;
            
        end
        
        function set.Data(Obj,HeaderCell)
            % setter for the header data / set IsKeyUpToDate to false
           
            Obj.Data = HeaderCell; 
            Obj.IsKeyUpToDate = false;
            
        end
    end
    
    
    methods % read/write
        function Obj=read(Obj,Args)
            % read as a single/multiple headers from file/s into an Header object
            % Input  : - An Header object
            %          * ...,key,val,... or ,key=val,...
            %            'FileName' - File name or a cell array of file
            %                   names. Default is to use Header.File
            %                   property.
            %            'HDU' - HDU (scalae or vector) or dataset. Default
            %                   is to use Header.HDU.
            %            'Type' - File type. 'fits'|'hdf5'|['auto'].
            %                   'auto' will attempt automatic
            %                   identificatin.
            % Examples: H=Header;
            % H.read('FileName',{'File1.fits','File2.fits'});  % read two headers in default HDU.
            % H.read('FileName','File1.fits','HDU',[1 2]); % read 2 HDUs from a single file
            
            arguments
                Obj
                Args.FileName      = Obj.File;
                Args.HDU           = Obj.HDU;
                Args.Type char {mustBeMember(Args.Type,{'auto','fits','fit','FITS','FIT','fit.gz','fits.gz','hdf5','h5','hd5'})} = 'auto';
            end
            
            if ~iscell(Args.FileName)
                Args.FileName = {Args.FileName};
            end
            Nfile = numel(Args.FileName);
            Nhdu  = numel(Args.HDU);
            Nmax  = max(Nfile,Nhdu);
            
            switch Args.Type
                case 'auto'
                    FileParts = split(FileName,'.');
                    Args.Type = FileParts{end};
            end
            
            switch lower(Args.Type)
                case {'fits','fit','fit.gz','fits.gz'}
                    % read FITS file
                    FO = FITS;
                    for Imax=1:1:Nmax
                        Ih    = min(Nhdu,Imax);
                        Ifile = min(Nfile,Imax);
                        Obj(Ifile).Data = FO.readHeader(Args.FileName{Ifile},Args.HDU(Ih));
                        Obj(Ifile).File = Args.FileName{Ifile};
                        Obj(Ifile).HDU  = Args.HDU(Ih);
                    end
                case {'hdf5','h5','hd5'}
                    % read hdf5 file
                    error('Read Header from HDF5 file is not available yet');
                otherwise
                    error('Unknown file Type option');
            end
            
            
            
        end
        
        
        function show(Obj)
            % Display all headers in an AstroHeader object
           
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                disp(Obj(Iobj).Data);
            end
        end
    end
    
    methods (Static) % static methods
        function Obj = createBasicHeader(Size,varargin)
            % Create an AstroHeader object with a basic header
            % Input  : - Size of AstroHeader object (e.g., [2 2]). Default
            %            is 1.
            %          * Either a cell array of {Key, Val} or
            %            {Key, Val, Comment} to replace, or add keywords,
            %            or pairs of header key,val arguments.
            % Output : - An AstroHeader object with the basic populated
            %            header.
            % Author : Eran Ofek (Apr 2021)
            % Example: HH = AstroHeader.createBasicHeader
            %          HH = AstroHeader.createBasicHeader(1,{'WINDDIR',11;'M_STAT','ok';'NEW',1});
            %          HH = AstroHeader.createBasicHeader(1,{'WINDDIR',11,'aa';'M_STAT','ok','jj'});
            %          HH = AstroHeader.createBasicHeader([1 2],'WINDDIR',11,'M_STAT','ok','NEW',1);   
           
            if nargin==0
                Size = [1 1];
            end
            
            Narg = numel(varargin);
            if Narg==1
                Key     = varargin{1}(:,1);
                Val     = varargin{1}(:,2);
                if size(varargin{1},2)>2
                    Comment = varargin{1}(:,3);
                else
                    Comment = [];
                end
            else
                if mod(Narg,2)~=0
                    error('Number of input arguments must be even (key,val)');
                end
                
                Key = varargin(1:2:end);
                Val = varargin(2:2:end);
                Comment = [];
            end
            
            CellHeader  = {'SIMPLE',true,'';...
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
                         
            %
            
            
            Obj = AstroHeader(Size);
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                Obj(Iobj).HDU  = 1;
                Obj(Iobj).File = ''; 
                Obj(Iobj).Data = CellHeader;
            end
            
            Obj.replaceVal(Key,Val,'Comment',Comment);
            
        end
        
    end
    
    
    methods  % functions for internal use
        %
        
        function Dict = getDictionary(Args)
            %
            
        end
        
    end
    
    methods % funUnary/Binary/Stack/Transform
        function Result = funUnary(Obj, Operator, Args)
            % funUnary for AstroHeader - modify header and add history
            % This is a self explenatory function usually for internal use
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
                if nargout>0
                    Args.CreateNewObj = true;
                else
                    Args.CreateNewObj = false;
                end
            end
            
            if Args.CreateNewObj
                Result = Obj.copyObject;
            else
                Result = Obj;
            end
            
            if Args.UpdateHeader
                Nobj = numel(Obj);
                if ~isempty(Args.NewUnits)
                    Result = replaceVal(Obj ,Args.UnitsKey, Args.NewUnits, Args.replaceValArgs{:});
                end
                
                if ~isempty(Args.ReplaceKeys)
                    Result = replaceVal(Obj ,Args.ReplaceKeys, Args.ReplaceVals, Args.replaceValArgs{:});
                end
                if ~isempty(Args.InsertKeys)
                    Result = insertKey(Obj ,ArgsInsertKeys, Args.insertKeyArgs{:});
                end
                if Args.AddHistory
                    HistoryLine = {'HISTORY', sprintf('funUnary with operator: %s',func2str(Operator)),''};
                    Result = insertKey(Obj ,HistoryLine, Args.insertKeyArgs{:});
                end
            end
            
        end
        
    end
    
    methods  % getVal, etc.
        function [Val, Key, Comment, Nfound] = getVal(Obj, KeySynonym, Args)
            % get a single keyword value where the keyword appears first in a dictionary.
            % Input  : - A single element AstroHeader object
            %          - Either a single character array, or a cell array
            %            of character arrays. If a single char array, then 
            %            search for its first occurence with or without
            %            dictionary.
            %            If a cell array, then this will override the
            %            dictionary, and the cell array will be regarded as
            %            a dictionary.
            %          * ...,key,val,...
            %            'UseDict' - Indicating if to use dictionary or to
            %                   perform an exact search. Default is true.
            %            'CaseSens' - Default is true.
            %            'SearchAlgo' - ['strcmp'] | 'regexp'.
            %                   or 'last' match.
            %            'Fill' - Fill value for the keyword Val, in case that the
            %                   key is not found. Default is NaN (comment will be
            %                   '').
            %            'Val2Num' - Attempt to convert the value to numeric.
            %                   Default is true.
            %            'Occur'        - For each synonym return the
            %                   ['first'] | 'last'.
            %            'KeyDict' - An optional keyword dictionary (a s
            %                   tructure) that will override the object
            %                   dictionary.
            %            'IsInputAlt' - If true, then the input keyword
            %                   will be assumed to be in the list of
            %                   alternate names. If false, then this must
            %                   be the primary key name in the dictionary.
            %                   For example, if you would like to search
            %                   by 'AEXPTIME' use true.
            %                   Default is false.
            %            'ReadCCDSEC' - If true will attempt to convert a
            %                   CCDSEC-like string (i.e., '[xmin xmax ymin ymax]')
            %                   into a 4-elements vector.
            % Output : - Value
            %          - Keyword name
            %          - Comment
            %          - Nfound
            % Author : Eran Ofek (Mar 2021)
            % Example: H=AstroHeader('WFPC2ASSNu5780205bx.fits');
            %          [Val, Key, Comment, Nfound] = getVal(H, 'EXPTIME')
            %          [Val, Key, Comment, Nfound] = getVal(H, 'AEXPTIME','IsInputAlt',true)
            %          [Val, Key, Comment, Nfound] = getVal(H, 'AEXPTIME'); % return NaN
            %          [Val, Key, Comment, Nfound] = getVal(H, {'BB','EXPTIME','AA'})
            %          [Val, Key, Comment, Nfound] = getVal(H, 'EXPTIME','UseDict',false)
            %          [Val, Key, Comment, Nfound] = getVal(H, 'AEXPTIME','UseDict',false)
            
            arguments
                Obj(1,1)
                KeySynonym
                Args.UseDict(1,1) logical              = true;
                Args.CaseSens(1,1) logical             = true;
                Args.SearchAlgo char                   = 'strcmp';
                Args.Fill                              = NaN;
                Args.Val2Num(1,1) logical              = true;
                Args.Occur                             = 'first';
                Args.KeyDict                           = [];  % a structure of dictionary
                Args.IsInputAlt(1,1) logical           = false;
                Args.ReadCCDSEC(1,1) logical           = false;
            end
            
            if ischar(KeySynonym)
                KeySynonym = {KeySynonym};
            end
            Nsyn = numel(KeySynonym);
            
            if Args.UseDict && Nsyn==1
                if isempty(Args.KeyDict)
                    % use Obj dictionary
                    Dict = Obj.KeyDict;
                else
                    Dict = Args.KeyDict;
                end
                if Args.IsInputAlt
                    [Key,AltConv,Alt,~] = searchAlt(Dict, KeySynonym, 'CaseSens',Args.CaseSens, 'SearchAlgo',Args.SearchAlgo);
                else
                    [Alt, AltConv] = searchKey(Dict, KeySynonym, 'CaseSens',Args.CaseSens, 'SearchAlgo',Args.SearchAlgo);
                end
                if isempty(Alt)
                    Alt = KeySynonym;
                end
            else
                Alt = KeySynonym;
            end
                
            [Val, Key, Comment, Nfound] = imUtil.headerCell.getValBySynonym(Obj.Data, Alt, ...
                                                          'CaseSens',Args.CaseSens,...
                                                          'SearchAlgo',Args.SearchAlgo,...
                                                          'Fill',Args.Fill,...
                                                          'Val2Num',Args.Val2Num,...
                                                          'Occur',Args.Occur,...
                                                          'ColKey',Obj.ColKey,...
                                                          'ColVal',Obj.ColVal,...
                                                          'ColComment',Obj.ColComment);
            
            if Args.ReadCCDSEC && ischar(Val)
                % Convert a CCDSEC like argument to vector
                CCDSEC = str2double(regexp(Val,'\[|\]|\s','split'));
                Val    = CCDSEC(2:end-1);
            end
        end
              
        function [Result ,ResultC, IK] = getStructKey(Obj,ExactKeys,Args)
            % Get multiple  keys from multiple headers and store in a structure array
            %       The keyword search can be exact (UseDict=false), or
            %       using a keywords dictionary (UseDict=true).
            % Input  : - An AstroHeader object (multiple elements supported)
            %          - A cell array of keyword names. These are exact
            %            keyword names without a dictionary interpretation.
            %          * ...,key,val,...
            %            'UseDict' - Indicating if to use dictionary or to
            %                   perform an exact search. Default is true.
            %            'CaseSens' - Default is true.
            %            'SearchAlgo' - ['strcmp'] | 'regexp'.
            %                   or 'last' match.
            %            'Fill' - Fill value for the keyword Val, in case that the
            %                   key is not found. Default is NaN (comment will be
            %                   '').
            %            'Val2Num' - Attempt to convert the value to numeric.
            %                   Default is true.
            %            'IsInputAlt' - If true, then the input keyword
            %                   will be assumed to be in the list of
            %                   alternate names. If false, then this must
            %                   be the primary key name in the dictionary.
            %                   For example, if you would like to search
            %                   by 'AEXPTIME' use true.
            %                   Default is false.
            %            'KeyDict' - An optional keyword dictionary (a s
            %                   tructure) that will override the object
            %                   dictionary.
            % Output : - A structure array, where number of elements equal
            %            to the number of elements in the AstroHeader.
            %            For each requested keyword name there is a
            %            corresponding field name, which value is the
            %            keyword value.
            %          - The same, but for the comments.
            %          - A cell array (number of elements equal to the number of keys)
            %            in which each cell contains the indices of the found keys in
            %            the cell-header. This is only for the last elemnt in
            %            the AstroHeader.
            % Author: Eran Ofek  (Apr 2021)
            % Example: H=AstroHeader('WFPC2ASSNu5780205bx.fits');
            %          [Result,C] = getStructKey(H, {'EXPTIME'})
            %          [Result,C] = getStructKey(H, {'EXPTIME','A'})
            %          [Result,C] = getStructKey(H, {'EXPTIME','A'},'UseDict',false)
            
            
            arguments
                Obj
                ExactKeys
                Args.UseDict(1,1) logical                                       = true;
                Args.CaseSens(1,1) logical                                      = true;
                Args.SearchAlgo char  {mustBeMember(Args.SearchAlgo,{'strcmp','regexp'})} = 'strcmp'; 
                Args.Fill                                                       = NaN;
                Args.Val2Num(1,1) logical                                       = true;
                Args.IsInputAlt(1,1) logical                                    = true;
                Args.KeyDict                                                    = [];
            end
            
            if ischar(ExactKeys)
                ExactKeys = {ExactKeys};
            end
            
            Nkey = numel(ExactKeys);
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                if isempty(Args.KeyDict)
                    % use Obj dictionary
                    Dict = Obj(Iobj).KeyDict;
                else
                    Dict = Args.KeyDict;
                end
                if Args.UseDict    
                    for Ikey=1:1:Nkey
                        if Args.IsInputAlt
                            [Key,AltConv,Alt,~] = searchAlt(Dict, ExactKeys{Ikey}, 'CaseSens',Args.CaseSens, 'SearchAlgo',Args.SearchAlgo);
                        else
                            [Alt, AltConv] = searchKey(Dict, ExactKeys{Ikey}, 'CaseSens',Args.CaseSens, 'SearchAlgo',Args.SearchAlgo);
                        end
                                              
                        if isempty(Alt)
                            Alt = ExactKeys{Ikey};
                        end
                        
                        [Val, Key, Comment, Nfound] = imUtil.headerCell.getValBySynonym(Obj(Iobj).Data, Alt);
                        Result(Iobj).(ExactKeys{Ikey}) = Val;
                        if nargout>1
                            ResultC(Iobj).(ExactKeys{Ikey}) = Comment;
                        end
                    end
                else
                    [SC, ~, ~, IK] = imUtil.headerCell.getByKey(Obj(Iobj).Data, ExactKeys, ...
                                                                'ReturnN',1,...
                                                                'CaseSens',Args.CaseSens,...
                                                                'Fill',Args.Fill,...
                                                                'Col',1,...
                                                                'Val2Num',Args.Val2Num);
                    %
                    Result(Iobj) = cell2struct(SC(:,Obj(Iobj).ColVal), SC(:,Obj(Iobj).ColKey), 1);
                    if nargout>1
                        ResultC(Iobj) = cell2struct(SC(:,Obj(Iobj).ColComment), SC(:,Obj(Iobj).ColKey), 1);
                    end
                end
                                      
            end
            
            
%             classdef StrSearchAlgo < uint32
%                 enumeration
%                     strcmp(0)
%                     regexp(1)
%                 end
%             end
%             
        end
        
        function [ResultVal, IK] = getCellKey(Obj,ExactKeys,Args)
            % Get multiple  keys from multiple headers and store in a cell array
            %       The keyword search can be exact (UseDict=false), or
            %       using a keywords dictionary (UseDict=true).
            % Input  : - An AstroHeader object (multiple elements supported)
            %          - A cell array of keyword names. These are exact
            %            keyword names without a dictionary interpretation.
            %          * ...,key,val,...
            %            'UseDict' - Indicating if to use dictionary or to
            %                   perform an exact search. Default is true.
            %            'CaseSens' - Default is true.
            %            'SearchAlgo' - ['strcmp'] | 'regexp'.
            %                   or 'last' match.
            %            'Fill' - Fill value for the keyword Val, in case that the
            %                   key is not found. Default is NaN (comment will be
            %                   '').
            %            'Val2Num' - Attempt to convert the value to numeric.
            %                   Default is true.
            %            'IsInputAlt' - If true, then the input keyword
            %                   will be assumed to be in the list of
            %                   alternate names. If false, then this must
            %                   be the primary key name in the dictionary.
            %                   For example, if you would like to search
            %                   by 'AEXPTIME' use true.
            %                   Default is false.
            %            'KeyDict' - An optional keyword dictionary (a s
            %                   tructure) that will override the object
            %                   dictionary.
            % Output : - A cell array of keyword values. Line per
            %            AstroHeader element, rows per keyword.
            %          - A cell array (number of elements equal to the number of keys)
            %            in which each cell contains the indices of the found keys in
            %            the cell-header. This is only for the last elemnt in
            %            the AstroHeader.
            % Author: Eran Ofek  (Apr 2021)
            % Example: H=AstroHeader('WFPC2ASSNu5780205bx.fits');
            %          [Result,IK] = getCellKey([H,H], {'EXPTIME','bb'},'UseDict',false)
            %          [Result,IK] = getCellKey([H,H], {'EXPTIME','bb'})
            %          [Result,IK] = getCellKey([H,H], {'AEXPTIME','bb'})
            
            arguments
                Obj
                ExactKeys
                Args.UseDict(1,1) logical                                       = true;
                Args.CaseSens(1,1) logical                                      = true;
                Args.SearchAlgo char  {mustBeMember(Args.SearchAlgo,{'strcmp','regexp'})} = 'strcmp'; 
                Args.Fill                                                       = NaN;
                Args.Val2Num(1,1) logical                                       = true;
                Args.IsInputAlt(1,1) logical                                    = true;
                Args.KeyDict                                                    = [];
            end
            
            if ischar(ExactKeys)
                ExactKeys = {ExactKeys};
            end
            
            IK     = {};
            Nkey   = numel(ExactKeys);
            Nobj   = numel(Obj);
            ResultVal = cell(Nobj, Nkey);
            for Iobj=1:1:Nobj
                if isempty(Args.KeyDict)
                    % use Obj dictionary
                    Dict = Obj(Iobj).KeyDict;
                else
                    Dict = Args.KeyDict;
                end
                if Args.UseDict    
                    for Ikey=1:1:Nkey
                        if Args.IsInputAlt
                            [Key,AltConv,Alt,~] = searchAlt(Dict, ExactKeys{Ikey}, 'CaseSens',Args.CaseSens, 'SearchAlgo',Args.SearchAlgo);
                        else
                            [Alt, AltConv] = searchKey(Dict, ExactKeys{Ikey}, 'CaseSens',Args.CaseSens, 'SearchAlgo',Args.SearchAlgo);
                        end
                                              
                        if isempty(Alt)
                            Alt = ExactKeys{Ikey};
                        end
                        
                        [Val, Key, Comment, Nfound] = imUtil.headerCell.getValBySynonym(Obj(Iobj).Data, Alt);
                        ResultVal{Iobj, Ikey} = Val;
                        
                    end
                else
                    [SC, ~, ~, IK] = imUtil.headerCell.getByKey(Obj(Iobj).Data, ExactKeys, ...
                                                                'ReturnN',1,...
                                                                'CaseSens',Args.CaseSens,...
                                                                'Fill',Args.Fill,...
                                                                'Col',1,...
                                                                'Val2Num',Args.Val2Num);
                    %
                    ResultVal(Iobj, :) = SC(:,Obj(Iobj).ColVal).';
                    
                end
                                      
            end
        
        end
        
        
        
        function Obj = insertDefaultComments(Obj,Args)
            % Insert/replace default comments for keys using the header comments dictionary
            % Input  : - An AstroHeader object (multiple elements supported)
            %          * ...,key,val,...
            %            'CaseSens' - Default is true.
            %            'SearchAlgo' - ['strcmp'] | 'regexp'.
            %                   or 'last' match.
            %            'IsInputAlt' - If true then will look for the
            %                   keyword in the alternate names.
            %                   Default is true.
            %            'Occur' - Which keyword appearnce to select
            %                   ['first'] | 'lsat'.
            % Output : - An AstroHeader with the comments populated from
            %            the CommentDict dictionary.
            % Author : Eran Ofek (Apr 2021)
            % Example: H=AstroHeader('WFPC2ASSNu5780205bx.fits');
            %          insertDefaultComments(H)
            
            arguments
                Obj
                Args.CaseSens(1,1) logical                             = true;
                Args.SearchAlgo char  {mustBeMember(Args.SearchAlgo,{'strcmp','regexp'})} = 'strcmp'; 
                Args.IsInputAlt(1,1) logical                           = true;
                Args.Occur                                             = 'first';
                
            end
           
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
               
                DictKeyNames = fieldnames(Obj(Iobj).CommentDict.Dict);
                NdictKeys    = numel(DictKeyNames);
                for IdictKeys=1:1:NdictKeys
                    if Args.IsInputAlt
                        [Key,AltConv,Alt,~] = searchAlt(Obj(Iobj).KeyDict, DictKeyNames{IdictKeys}, 'CaseSens',Args.CaseSens, 'SearchAlgo',Args.SearchAlgo);
                        if isempty(Alt)
                            Alt = DictKeyNames(IdictKeys);
                        end
                    else
                        Alt = DictKeyNames(IdictKeys);
                    end
                      
                    if ~isempty(Alt)
                        % search for Alt in the header
                        CleanCell = Obj(Iobj).Data(:, Obj(Iobj).ColKey);
                        FlagNOK = cellfun(@isnumeric, Obj(Iobj).Data(:,Obj(Iobj).ColKey));
                        [CleanCell{FlagNOK}] = deal('');
                        
                        Flag = ismember(CleanCell, Alt);
                        Ind  = find(Flag, 1, Args.Occur);

                        if ~isempty(Ind)
                            KeyName = Obj(Iobj).Data{Ind, Obj(Iobj).ColKey};
                            try
                                Obj(Iobj).Data{Ind, Obj.ColComment} = Obj(Iobj).CommentDict.Dict.(KeyName){1};
                            catch
                                Obj(Iobj).Data{Ind, Obj.ColComment} = Obj(Iobj).CommentDict.Dict.(Key){1};
                            end
                        end
                        
                    end
                end
            end
                    
        end
        
%         function Obj = fixKeys(Obj,Args)
%             % Change the name of header keywords according to their main
%             % name in the keyword synonymns dictionary
%             % Input  : -
%             % Output : -
%             % Author : Eran Ofek (Apr 2021)
%             % Example: 
%             
%             
%             
%         end
        
        function Obj = deleteKey(Obj,ExactKeys,Args)
            % Delete keywords from header by exact keyword name
            % Input  : - An AstroHeader object (multiple elements are
            %            supported).
            %          - A char array or a cell array of chars of keyword
            %            names to delete from all the headers.
            %          * ...,key,val,...
            %            'CaseSens' - Default is true.
            %            'UseRegExp' - Use regexp (true) or strcmp (false).
            %                   Default is true.
            % Example: H=AstroHeader('WFPC2ASSNu5780205bx.fits');
            %          deleteKey(H,{'EXPTIME','A','COMMENT'})
            %          deleteKey(H,{'EXPTIME','A','SKYSUB\d'}) % use regexp
            
            arguments
                Obj
                ExactKeys
                Args.CaseSens(1,1) logical    = true;
                Args.UseRegExp(1,1) logical   = true;
            end
            
            if ischar(ExactKeys)
                ExactKeys = {ExactKeys};
            end
            Nkeys = numel(ExactKeys);
            
            searchFun = tools.string.stringSearchFun(Args.UseRegExp, Args.CaseSens);
            
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                Nrow = size(Obj(Iobj).Data,1);
                Flag = false(Nrow,1);
                for Ikeys=1:1:Nkeys
                    % in principle can use ismember (but no regexp)
                    NewFlag = searchFun( Obj(Iobj).Data(:,Obj(Iobj).ColKey), ExactKeys{Ikeys});
                    Flag = Flag | NewFlag(:);
                end
                % remove keywords
                Obj(Iobj).Data = Obj(Iobj).Data(~Flag,:);
            end
                 
        end
        
        function Obj = insertKey(Obj, KeyValComment, Pos)
            % Insert key/val/comment to headers
            % Input  : - An AstroHeader object (multi. elements supported)
            %          - Either a key name, or a cell array of
            %          [Key,Val,Comment], or [Key,Val].
            % Output : - An AstroHeader object with the new key/vals.
            % Author : Eran Ofek (Apr 2021)
            % Example: H=AstroHeader('WFPC2ASSNu5780205bx.fits');
            %          H.insertKey('stam')
            %          H.insertKey({'A','','';'B','',''},'end-1')
            
            arguments
                Obj
                KeyValComment
                Pos                  = 'end-1';
            end
        
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                Obj(Iobj).Data = imUtil.headerCell.insertKey(Obj(Iobj).Data, KeyValComment, Pos);
            end
            
        end
        
        
        function Obj = replaceVal(Obj,Key,Val,Args)
            % Replace a keyword value in headers (no dictionary in key
            % search).
            % Input  : - An AstroHeader object (multi elements supported).
            %          - A key name or a cell array of key names.
            %          - A vector or a cell array of values, corresponding to the key
            %            names.
            %          * ...,key,val,... or ...,key=val',... list
            %            'SearchAlgo' - search using: ['strcmp'] | 'regexp'
            %            'CaseSens' - Default is true.
            %            'RepVal'   - Replace value. Default is true.
            %            'Comment'  - A cell array of optional comments.
            %                   If empty, then do not replace comment. Default is [].
            %            'NewKey' - A cell array of new keys to replace the old keys.
            %                   If empty, then do not replace keys.
            %                   Default is {}.
            %            'AddKey' - Add key if doens't exist. Default is true.
            %            'AddPos' - Position in which to add key if doesn't exist.
            %                   Default is 'end-1'.
            %            'ColKey' - Column index of keys. Default is 1.
            %            'ColVal' - Column index of values. Default is 2.
            %            'ColComment' - Column index of comments. Default is 3.
            % Output : - A new cell array with the replaced keys/values.
            % Example: H=AstroHeader('WFPC2ASSNu5780205bx.fits');
            %          H.replaceVal({'COMMENT'},{''});
            
            arguments
                Obj
                Key
                Val cell
                Args.SearchAlgo char  {mustBeMember(Args.SearchAlgo,{'strcmp','regexp'})} = 'strcmp'; 
                Args.CaseSens(1,1) logical                    = true;
                Args.RepVal(1,1) logical                      = true;
                Args.Comment                                  = [];
                Args.NewKey                                   = {};
                Args.AddKey(1,1) logical                      = true;
                Args.AddPos                                   = 'end';
                Args.ColKey(1,1) uint8                        = 1;
                Args.ColVal(1,1) uint8                        = 2;
                Args.ColComment(1,1) uint8                    = 3;
            end

            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                Obj(Iobj).Data = imUtil.headerCell.replaceKey(Obj(Iobj).Data, Key, Val,...
                                                              'SearchAlgo',Args.SearchAlgo,...
                                                              'CaseSens',Args.CaseSens,...
                                                              'RepVal',Args.RepVal,...
                                                              'Comment',Args.Comment,...
                                                              'NewKey',Args.NewKey,...
                                                              'AddKey',Args.AddKey,...
                                                              'AddPos',Args.AddPos,...
                                                              'ColKey',Obj(Iobj).ColKey,...
                                                              'ColVal',Obj(Iobj).ColVal,...
                                                              'ColComment',Obj(Iobj).ColComment);
                                                              
            end
            
        end
        
%         function search(Obj,Val,Args)
%             %
%             arguments
%                 Obj         
%                 Val                 
%                 Args.Column                  = 1;
%                 Args.CaseSens(1,1) logical   = true;
%             end
%         end
        
        
        function Result = isKeyVal(Obj, Key, Val, Args)
            % Check if a single keyword value equal to some value.
            % Input  : - An AstroHeader object (multi elements supported).
            %          - A single header keyword name.
            %          - A value (string or char array) to compare to the
            %            header keyword value.
            %          * ...,key,val,...
            %            'NumericTol' - Tolerance for numerical comparison.
            %                   Default is 1e-8.
            %            'KeyCaseSens' - Keyword search is case sensetive. Default is true.
            %            'ValCaseSens' - Value comparison is case sensetive. Default is false.
            %            'UseDict' - Indicating if to use dictionary or to
            %                   perform an exact search. Default is true.
            %            'SearchAlgo' - ['strcmp'] | 'regexp'.
            %                   or 'last' match.
            %            'Fill' - Fill value for the keyword Val, in case that the
            %                   key is not found. Default is NaN (comment will be
            %                   '').
            %            'Val2Num' - Attempt to convert the value to numeric.
            %                   Default is true.
            %            'Occur'        - For each synonym return the
            %                   ['first'] | 'last'.
            %            'KeyDict' - An optional keyword dictionary (a s
            %                   tructure) that will override the object
            %                   dictionary.
            %            'IsInputAlt' - If true, then the input keyword
            %                   will be assumed to be in the list of
            %                   alternate names. If false, then this must
            %                   be the primary key name in the dictionary.
            %                   For example, if you would like to search
            %                   by 'AEXPTIME' use true.
            %                   Default is false.
            % Output : - An array of logical (size like input object),
            %            indicating for each object element if the keyword 
            %            value is equal to the specified value.
            % Author : Eran Ofek (Apr 2021)
            % Example: H=AstroHeader('WFPC2ASSNu5780205bx.fits');
            %          isKeyVal([H, H],'EXPTIME',300)
            %          isKeyVal([H;H], 'KSPOTS','off')
            %          isKeyVal([H;H], 'KSPOTS','off','ValCaseSens',true)
            
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
                Args.KeyDict                           = [];  % a structure of dictionary
                Args.IsInputAlt(1,1) logical           = false;
            end
            
            searchFun = tools.string.stringSearchFun(false, Args.ValCaseSens);
            
            Nobj = numel(Obj);
            Result = false(size(Obj));
            for Iobj=1:1:Nobj
                [KeyVal, Key, ~, Nfound]    = getVal(Obj(Iobj), Key, ...
                                                     'UseDict',Args.UseDict,...
                                                     'CaseSens',Args.KeyCaseSens,...
                                                     'SearchAlgo',Args.SearchAlgo,...
                                                     'Fill',Args.Fill,...
                                                     'Val2Num',Args.Val2Num,...
                                                     'Occur',Args.Occur,...
                                                     'KeyDict',Args.KeyDict,...
                                                     'IsInputAlt',Args.IsInputAlt);
                %
                if Nfound==0
                    Result(Iobj) = false;
                else
                    if ischar(Val) || ischar(KeyVal)
                        
                        Result(Iobj) = searchFun(Val, KeyVal);
                    else
                        if isnan(Val) && isnan(KeyVal)
                            Result(Iobj) = true;
                        else
                            Result(Iobj) = abs(Val-KeyVal)<Args.NumericTol;
                        end
                    end
                end                
            end
            
            
            
        
        end
        
        function Result = isKeyExist(Obj, Key, Args)
            % Check if a single keyword value equal to some value.
            % Input  : - An AstroHeader object (multi elements supported).
            %          - A single header keyword name.
            %            header keyword value.
            %          * ...,key,val,...
            %            'CaseSens' - Keyword search is case sensetive. Default is true.
            %            'UseDict' - Indicating if to use dictionary or to
            %                   perform an exact search. Default is true.
            %            'SearchAlgo' - ['strcmp'] | 'regexp'.
            %                   or 'last' match.
            %            'Fill' - Fill value for the keyword Val, in case that the
            %                   key is not found. Default is NaN (comment will be
            %                   '').
            %            'Val2Num' - Attempt to convert the value to numeric.
            %                   Default is true.
            %            'Occur'        - For each synonym return the
            %                   ['first'] | 'last'.
            %            'KeyDict' - An optional keyword dictionary (a s
            %                   tructure) that will override the object
            %                   dictionary.
            %            'IsInputAlt' - If true, then the input keyword
            %                   will be assumed to be in the list of
            %                   alternate names. If false, then this must
            %                   be the primary key name in the dictionary.
            %                   For example, if you would like to search
            %                   by 'AEXPTIME' use true.
            %                   Default is false.
            % Output : - An array of logical (size like input object),
            %            indicating for each object element if the keyword 
            %            value is equal to the specified value.
            % Author : Eran Ofek (Apr 2021)
            % Example: H=AstroHeader('WFPC2ASSNu5780205bx.fits');
            %          isKeyExist([H, H],'EXPTIME')
            %          isKeyExist([H, H],'AEXPTIME')
            %          isKeyExist([H; H],'AEXPTIME','IsInputAlt',true)
            %          isKeyExist([H, H],'aaa')
            
            arguments
                Obj
                Key
                Args.CaseSens(1,1) logical             = true;
                Args.UseDict(1,1) logical              = true;
                Args.SearchAlgo char                   = 'strcmp';
                Args.Fill                              = NaN;
                Args.Val2Num(1,1) logical              = true;
                Args.Occur                             = 'first';
                Args.KeyDict                           = [];  % a structure of dictionary
                Args.IsInputAlt(1,1) logical           = false;
            end
                        
            Nobj = numel(Obj);
            Result = true(size(Obj));
            for Iobj=1:1:Nobj
                [~, ~, ~, Nfound]           = getVal(Obj(Iobj), Key, ...
                                                     'UseDict',Args.UseDict,...
                                                     'CaseSens',Args.CaseSens,...
                                                     'SearchAlgo',Args.SearchAlgo,...
                                                     'Fill',Args.Fill,...
                                                     'Val2Num',Args.Val2Num,...
                                                     'Occur',Args.Occur,...
                                                     'KeyDict',Args.KeyDict,...
                                                     'IsInputAlt',Args.IsInputAlt);
                %
                if Nfound==0
                    Result(Iobj) = false;
                end
            end
        end
        
        function Flag = isImType(Obj, ImTypeVal, Args)
            % Check if header IMTYPE keyword value equal some type
            % Input  : - An AstroHeader object.
            %          - IMTYPE type to check (e.g., 'bias').
            %          * ...,key,val,...
            %            'UseDict' - Indicating if to use dictionary or to
            %                   perform an exact search. Default is true.
            %            'CaseSens' - Default is true.
            %            'SearchAlgo' - ['strcmp'] | 'regexp'.
            %                   or 'last' match.
            %            'IsInputAlt' - If true, then the input keyword
            %                   will be assumed to be in the list of
            %                   alternate names. If false, then this must
            %                   be the primary key name in the dictionary.
            %                   For example, if you would like to search
            %                   by 'AEXPTIME' use true.
            %                   Default is false.
            %            'KeyDict' - An optional keyword dictionary (a s
            %                   tructure) that will override the object
            %                   dictionary.
            % Output : - An array of logicals (one per AstroHeader element)
            %            indicating if the IMTYPE value equal the requested
            %            value.
            % Author : Eran Ofek (Apr 2021)
            % Example: H=AstroHeader('*.fits');
            %          Ans = isImType(H, 'bias')
            
            arguments
                Obj
                ImTypeVal
                Args.ImTypeKeyName                                   = 'IMTYPE';
                Args.UseDict(1,1) logical                            = true;
                Args.CaseSens(1,1) logical                           = true;
                Args.SearchAlgo                                      = 'strcmp'; 
                Args.IsInputAlt(1,1) logical                         = true;
                Args.KeyDict                                         = [];
            end
            
            %
            [KeyVal] = getStructKey(Obj, Args.ImTypeKeyName,...
                                         'UseDict',Args.UseDict,...
                                         'CaseSens',Args.CaseSens,...
                                         'SearchAlgo',Args.SearchAlgo,...
                                         'Fill',NaN,...
                                         'Val2Num',false,...
                                         'IsInputAlt',Args.IsInputAlt,...
                                         'KeyDict',Args.KeyDict);
            %
            % got here
            FN = fieldnames(KeyVal);
            ListVal = {KeyVal.(FN{1})};  % cell array of IMTYPE values
            
            [Key,~,AllAlt]= searchAlt(Obj(1).ValDict,ImTypeVal, 'CaseSens',Args.CaseSens, 'SearchAlgo',Args.SearchAlgo);
            if ~iscellstr(ListVal)
                % remove NaN values
                IsNaN = tools.cell.isnan_cell(ListVal);
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
            %   the exposure. This is done by retrieving the relevank
            %   header keywords (default in
            %   config/Header.Time.KeyNames.yml).
            %   Each keyword is associated with a conversion formulae.
            % Input  : - AstroHeader object (multi elements supported).
            %          * ...,key,val,...
            %            'ExpTimeKey' - Exposure time header keyword.
            %                   Default is 'EXPTIME'.
            %            'FunTimeKeys' - A structure array (Dictionary) of
            %                   time keyword names and their conversion
            %                   formulae.
            %                   E.g., Args.FunTimeKeys.MJD       = @(Time,Exp) convert.time(Time,'MJD','JD') + 0.5.*Exp./SEC_IN_DAY;
            %                   If empty, then will attempt to use TimeDict
            %                   dictionary stored in the AStroHeader. If th
            %                   TimeDict is empty, then will use internal
            %                   function defaults.
            %            'UseDict' - Indicating if to use dictionary or to
            %                   perform an exact search. Default is true.
            %            'CaseSens' - Default is true.
            %            'SearchAlgo' - ['strcmp'] | 'regexp'.
            %                   or 'last' match.
            %            'Fill' - Fill value for the keyword Val, in case that the
            %                   key is not found. Default is NaN (comment will be
            %                   '').
            %            'Val2Num' - Attempt to convert the value to numeric.
            %                   Default is true.
            %            'IsInputAlt' - If true, then the input keyword
            %                   will be assumed to be in the list of
            %                   alternate names. If false, then this must
            %                   be the primary key name in the dictionary.
            %                   For example, if you would like to search
            %                   by 'AEXPTIME' use true.
            %                   Default is false.
            %            'KeyDict' - An optional keyword dictionary (a s
            %                   tructure) that will override the object
            %                   dictionary.
            % Outout : - Matrix of mid exposure time JD, for each
            %            AstroHeader element. NaN if can not calculate JD.
            %          - Matrix of exposure times (usually seconds), for
            %            each AstroHeader element. NaN if not found.
            % Example: H=AstroHeader('WFPC2ASSNu5780205bx.fits');
            %          [JD,ET] = julday(H)
            %          [JD,ET] = julday([H;H])
            
            arguments
                Obj
                Args.ExpTimeKey                        = 'EXPTIME';
                Args.FunTimeKeys cell                  = {};
                %Args.TreatBug2000(1,1) logical                                  = true;
                Args.UseDict(1,1) logical                                       = true;
                Args.CaseSens(1,1) logical                                      = true;
                Args.SearchAlgo char  {mustBeMember(Args.SearchAlgo,{'strcmp','regexp'})} = 'strcmp'; 
                Args.Fill                                                       = NaN;
                Args.Val2Num(1,1) logical                                       = true;
                Args.IsInputAlt(1,1) logical                                    = true;
                Args.KeyDict                                                    = [];
                
            end
            SEC_IN_DAY = 86400;
            
            if isempty(Args.FunTimeKeys)
                % attempt loading from dictionary
                if isempty(Obj(1).TimeDict.FieldNames)
                    % set up to default values
                    Args.FunTimeKeys.Dict.MIDJD     = @(Time,Exp) Time;
                    Args.FunTimeKeys.Dicr.MIDMJD    = @(Time,Exp) convert.time(Time,'MJD','JD');
                    Args.FunTimeKeys.Dict.JD        = @(Time,Exp) Time + 0.5.*Exp./SEC_IN_DAY;
                    Args.FunTimeKeys.Dict.MJD       = @(Time,Exp) convert.time(Time,'MJD','JD') + 0.5.*Exp./SEC_IN_DAY;
                    Args.FunTimeKeys.Dict.DATEOBS   = @(Time,Exp) convert.time(Time,'StrDate','JD') + 0.5.*Exp./SEC_IN_DAY;
                    Args.FunTimeKeys.Dict.TIMEOBS   = @(Time,Exp) convert.time(Time,'StrDate','JD') + 0.5.*Exp./SEC_IN_DAY;
                    Args.FunTimeKeys.Dict.DATE      = @(Time,Exp) convert.time(Time,'StrDate','JD') + 0.5.*Exp./SEC_IN_DAY;

                else
                    % use dictionary
                    Args.FunTimeKeys = Obj(1).TimeDict;
                end
            end
                                  
            
            TimeKeys = fieldnames(Args.FunTimeKeys.Dict);
            NtimeKeys = numel(TimeKeys);
            
            StTime    = getStructKey(Obj, TimeKeys);
            StExp     = getStructKey(Obj, Args.ExpTimeKey);
            
            
            MidJD   = nan(size(Obj));
            ExpTime = nan(size(Obj));
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                ExpTime(Iobj)  = StExp(Iobj).(Args.ExpTimeKey);
                Found = false;
                Ikey = 0;
                while ~Found && Ikey<NtimeKeys 
                    Ikey = Ikey + 1;
                    T  = StTime(Iobj).(TimeKeys{Ikey});
                    if ~isnan(T)
                        if iscell(Args.FunTimeKeys.Dict.(TimeKeys{Ikey}))
                            JD = Args.FunTimeKeys.Dict.(TimeKeys{Ikey}){1}(T, ExpTime(Iobj));
                        else
                            JD = Args.FunTimeKeys.Dict.(TimeKeys{Ikey})(T, ExpTime(Iobj));
                        end
                        if ~isnan(JD)
%                             if Args.TreatBug2000
%                                 if JD<celestial.time.julday([1 1 100])
%                                     % assume that year is given with two
%                                     % digits
%                                     DD = celestial.time.jd2date(JD);
%                                     JD = celestial.time.julday([DD(1) DD(2) DD(3)+1900]);
%                                 end
%                             end
                            MidJD(Iobj) = JD;
                        end
                    end
                end
            end
        end
        
        % findGroups
        function Groups = groupByKeyVal(Obj, Keys, Args)
            % Group a set of AstroHeaders by their unique keyword values.
            %   e.g., look for all images with the same EXPTIME and put
            %   them in different groups according to the EXPTIME value.
            % Input  : - An AstroHeader object (multi elements supported).
            %          - A cell array of header keywords by which to group
            %            the headers.
            %          * ...,key,val,...
            %            'UseDict' - Indicating if to use dictionary or to
            %                   perform an exact search. Default is true.
            %            'CaseSens' - Default is true.
            %            'SearchAlgo' - ['strcmp'] | 'regexp'.
            %                   or 'last' match.
            %            'Fill' - Fill value for the keyword Val, in case that the
            %                   key is not found. Default is NaN (comment will be
            %                   '').
            %            'Val2Num' - Attempt to convert the value to numeric.
            %                   Default is true.
            %            'IsInputAlt' - If true, then the input keyword
            %                   will be assumed to be in the list of
            %                   alternate names. If false, then this must
            %                   be the primary key name in the dictionary.
            %                   For example, if you would like to search
            %                   by 'AEXPTIME' use true.
            %                   Default is false.
            %            'KeyDict' - An optional keyword dictionary (a s
            %                   tructure) that will override the object
            %                   dictionary.
            % Output : - Structure of groups. Each structure represent a group of rows
            %            in the input cell array which have equal values.
            %            The structure contains the following fields:
            %            .Conntent  - The row of values defines the group.
            %            .ptr       - The indices of the rows that belong to the
            %                         group.
            % Example: H=AstroHeader('WFPC2ASSNu5780205bx.fits');
            %          Groups = groupByKeyVal([H,H],{'IMTYPE','FILTER1','EXPTIME'})
            
            arguments
                Obj
                Keys cell        = {};  % e.g., {'IMTYPE','EXPTIME','FILTER'}
                Args.UseDict(1,1) logical                                       = true;
                Args.CaseSens(1,1) logical                                      = true;
                Args.SearchAlgo char  {mustBeMember(Args.SearchAlgo,{'strcmp','regexp'})} = 'strcmp'; 
                Args.Fill                                                       = NaN;
                Args.Val2Num(1,1) logical                                       = true;
                Args.IsInputAlt(1,1) logical                                    = true;
                Args.KeyDict                                                    = [];
            end
            
            % need a version of getStructKey for cells
            CellVal = getCellKey(Obj, Keys, 'UseDict',Args.UseDict,...
                                            'CaseSens',Args.CaseSens,...
                                            'SearchAlgo',Args.SearchAlgo,...
                                            'Fill',Args.Fill,...
                                            'Val2Num',Args.Val2Num,...
                                            'IsInputAlt',Args.IsInputAlt,...
                                            'KeyDict',Args.KeyDict);
            
            Groups = tools.cell.cell_find_groups(CellVal);
            
        end
        
        % getObsCoo
        
        % getCoo
        
    end
    
    
    % Unit test
    methods(Static)
        function Result = unitTest()
            % unitTest for AstroHeader
            % Example: Result = AstroHeader.unitTest

            DataSampleDir = tools.os.getTestDataDir;
            PWD = pwd;
            cd(DataSampleDir);
            

            % construct an empty AstroHeader
            A = AstroHeader([2 2]);
            % read headers to AstroHeader, on construction
            H = AstroHeader('*.fits',1);
            H(1).Key.NAXIS
            H(2).Key.DATE
            
            H=AstroHeader('WFPC2ASSNu5780205bx.fits');
            [Val, Key, Comment, Nfound] = getVal(H, 'EXPTIME');
            [Val, Key, Comment, Nfound] = getVal(H, 'AEXPTIME','IsInputAlt',true);
            [Val, Key, Comment, Nfound] = getVal(H, 'AEXPTIME'); % return NaN
            [Val, Key, Comment, Nfound] = getVal(H, {'BB','EXPTIME','AA'});
            [Val, Key, Comment, Nfound] = getVal(H, 'EXPTIME','UseDict',false);
            [Val, Key, Comment, Nfound] = getVal(H, 'AEXPTIME','UseDict',false);

            H=AstroHeader('WFPC2ASSNu5780205bx.fits');
            [Result,C] = getStructKey(H, {'EXPTIME'});
            [Result,C] = getStructKey(H, {'EXPTIME','A'});
            [Result,C] = getStructKey(H, {'EXPTIME','A'},'UseDict',false);

            H = AstroHeader('*.fits',1);
            [Result,C] = getStructKey(H, {'EXPTIME','A'});
            [Result,IK] = getCellKey(H, {'EXPTIME','bb'},'UseDict',false);
            [Result,IK] = getCellKey(H, {'EXPTIME','bb'});
            [Result,IK] = getCellKey(H, {'AEXPTIME','bb'});

            H=AstroHeader('WFPC2ASSNu5780205bx.fits');
            insertDefaultComments(H);
            
            H=AstroHeader('WFPC2ASSNu5780205bx.fits');
            deleteKey(H,{'EXPTIME','A','COMMENT'});
            deleteKey(H,{'EXPTIME','A','SKYSUB\d'}); % use regexp
            if ~isnan(getVal(H,'COMMENT'))
                error('Key value should be NaN');
            end

            H=AstroHeader('WFPC2ASSNu5780205bx.fits');
            H.insertKey('stam');
            H.insertKey({'A','','';'B','',''},'end-1');
            show(H);
            
            H=AstroHeader('WFPC2ASSNu5780205bx.fits');
            H.replaceVal({'COMMENT'},{''});
            
            H=AstroHeader('WFPC2ASSNu5780205bx.fits');
            isKeyVal([H, H],'EXPTIME',300);
            isKeyVal([H;H], 'KSPOTS','off');
            isKeyVal([H;H], 'KSPOTS','off','ValCaseSens',true);

            H=AstroHeader('WFPC2ASSNu5780205bx.fits');
            isKeyExist([H, H],'EXPTIME');
            isKeyExist([H, H],'AEXPTIME');
            isKeyExist([H; H],'AEXPTIME','IsInputAlt',true);
            isKeyExist([H, H],'aaa');
            
            H=AstroHeader('WFPC2ASSNu5780205bx.fits');
            [JD,ET] = julday(H);
            [JD,ET] = julday([H;H]);

            H=AstroHeader('WFPC2ASSNu5780205bx.fits');
            Groups = groupByKeyVal([H,H],{'IMTYPE','FILTER1','EXPTIME'});
            
            H = AstroHeader('*.fits',1);
            Groups = groupByKeyVal([H,H],{'IMTYPE','FILTER1','EXPTIME'});
            
            
            cd(PWD);

            Result = true;
            
        end
    end    
        
end


