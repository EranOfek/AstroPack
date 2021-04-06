% @Chen

% Configuration base Class
% Package: 
% Description:
%--------------------------------------------------------------------------

classdef AstroHeader < handle %< Component
    % Properties
    properties (SetAccess = public)
        Data(:,3) cell            = cell(0,3);
        Key struct                = struct(); 
        
        File                      = '';
        HDU                       = ''; % HDU or dataset
        
        KeyDict Dictionary        = Dictionary('Name','Header.Synonyms.KeyNames');
        ValDict Dictionary        = Dictionary('Name','Header.Synonyms.KeyVal.IMTYPE');
        CommentDict Dictionary    = Dictionary('Name','Header.Comments.Default');
    end
    properties (Hidden, SetAccess=private)
        IsKeyUpToDate(1,1) logical    = true;
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
            %            'Method' - Files extraction method 'wild' |
            %                   'regexp'. See io.files.filelist.
            % Output : - An AstroHeader object with populated headers.
            % Author : Eran Ofek (Mar 20201)
            % Example: 
            
            arguments
                FileNames      = 1;   % name or array size
                HDU            = 1;
                Args.Method    = 'wild';
            end
            
            if isnumeric(FileNames)
                % create an empty AstroHeader object
                List = cell(FileNames);
                
            elseif iscell(FileNames) || isstring(FileNames)
                % User provided a list of file names
                List = FileNames;
            else
                % read file names
                List = io.files.filelist(FileNames,Args.Method);
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
        
        
        function disp(Obj)
            % Display all headers in an AstroHeader object
           
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                disp(Obj(Iobj).Data);
            end
        end
    end
    
    methods  % functions for internal use
        %
        
        function Dict = getDictionary(Args)
            %
            
        end
        
    end
    
    methods 
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
            %            'UseDict'
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
            else
                Alt = KeySynonym;
            end
                
            [Val, Key, Comment, Nfound] = imUtil.headerCell.getValBySynonym(Obj.Data, Alt, ...
                                                          'CaseSens',Args.CaseSens,...
                                                          'SearchAlgo',Args.SearchAlgo,...
                                                          'Fill',Args.Fill,...
                                                          'Val2Num',Args.Val2Num,...
                                                          'Occur',Args.Occur);
            
        end
        
        
        function [SubCell,FlagExist,IndFound,IndKey] = getSubHeaderByKey(Cell,Key,Args)
            %
            
        end
        
        
        
        
        function [Val, Key, Comment, Nfound] = keyValByynonyms(Obj, KeySynonym, Args)
            % Return the first key in the list that appears in the header.
           
            arguments
                Obj(1,1)
                KeySynonym
                Args.CaseSens(1,1) logical           = true;
                Args.SearchAlgo char {mustBeMember(Args.SearchAlgo,{'strcmp','regexp'})} = 'strcmp';
                Args.Occur                           = 'first';  % 'first' | 'last'
                Args.Fill                            = NaN;
                Args.Val2Num(1,1) logical            = true;
            end
            
            if ischar(KeySynonym)
                KeySynonym = {KeySynonym};
            end
            
            Nsyn   = numel(KeySynonym);
            Flag   = ismember(Obj.Data(:,Obj.ColKey), KeySynonym);
            Nfound = sum(Flag);
            Ind    = find(Flag, 1, Args.Occur);
            if isempty(Ind)
                Val     = Args.Fill;
                Key     = KeySynonym{1};
                Comment = '';
            else
                Key     = Obj.Data(Ind, Obj.ColKey);
                Val     = Obj.Data(Ind, Obj.ColVal);
                Comment = Obj.Data(Ind, Obj.ColComment);
            end
            
            % convert to number
            if Args.Val2Num
                ValNum  = str2double(Val);
                if isnan(ValNum) && ~strcmpi(Val,'nan')
                    % string
                    % do nothing
                else
                    % number
                    Val = ValNum;
                end
            end
        end
        
        
        function [Val,Key,Comment]=keyVal(Obj,KeySynonym,Args)
            % get a the value for a single header keyword
            % 
            
            arguments
                Obj
                KeySynonym                        = '';
                Args.CaseSens(1,1) logical        = true;
                Args.SearchAlgo char {mustBeMember(Args.SearchAlgo,{'strcmp','regexp'})} = 'strcmp';
                Args.IsInputAlt(1,1) logical      = true;
                Args.Fill                         = NaN;
                Args.Val2Num(1,1) logical         = true;
                Args.ReturnN                      = 1;
                Args.UseDict(1,1) logical         = true;
                Args.ApplyConversion(1,1) logical = true;
            end
            
            
            if numel(Obj)>1
                error('Use mkeyVal for Header object with multiple entries or multiple keys');
            end
            
            % I need the dictionary in order to continue
            if Args.UseDict
                
                
                if Args.IsInputAlt
                    [Key,AltConv,Alt,~] = searchAlt(Obj.KeyDict, KeySynonym, 'CaseSens',Args.CaseSens, 'SearchAlgo',Args.SearchAlgo);
                else
                    [Alt, AltConv] = searchKey(Obj.KeyDict, KeySynonym, 'CaseSens',Args.CaseSens, 'SearchAlgo',Args.SearchAlgo);
                end
                if isempty(Alt)
                    % unknown synonym - use original key
                    Alt     = {KeySynonym};
                end
            else
                Alt     = {KeySynonym};
                AltConv = [];
            end
            
            [SubCell,FlagExist,IndFound,IndKey] = imUtil.headerCell.getVal(Obj.Data,...
                                                                           Alt,...
                                                                           'CaseSens',Args.CaseSens,...
                                                                           'SearchAlgo',Args.SearchAlgo,...
                                                                           'Fill',Args.Fill,...
                                                                           'Val2Num',Args.Val2Num,...
                                                                           'ReturnN',Args.ReturnN);
            %cellfun(@isnan,SubCell(:,2)
            if any(FlagExist)
                Ind = find(FlagExist,1,'first');
                Val     = SubCell{Ind,2};
                Key     = SubCell{Ind,1};
                Comment = SubCell{Ind,3};
            else
                % not found
                Key = KeySynonym;
                Val = Args.Fill;
                Comment = '';
            end
            
            if Args.ApplyConversion && ~isempty(AltConv)
                Val = AltConv(Val);
            end
            
        end
        
        function mkeyVal(Obj,KeySynonym,Args)
            %
            
            arguments
                Obj
                KeySynonym  {mustBeA(KeySynonym,{'char','cell'})}  = '';
                Args.CaseSens(1,1) logical    = true;
                Args.NotExist char  {mustBeMember(Args.NotExist,{'NaN','fail'})} = 'NaN';
                Args.Val2Num(1,1) logical     = true;
                Args.UseDict(1,1) logical     = true;
                Args.OutType char {musBeMember(Args.OutType,{'cell','Header'})} = 'cell';
            end
            
        end
        
        function Result=deleteKey(Obj,KeySynonym,Args)
            %
           
            arguments
                Obj
                KeySynonym  {mustBeA(KeySynonym,{'char','cell'})}  = '';
                Args.CaseSens(1,1) logical    = true;
                Args.UseDict(1,1) logical     = true;
            end
            
        end
        
        function insertKey(Obj,KeyValComment,Args)
            %
            
            arguments
                Obj
                KeyValComment  {mustBeA(KeyValComment,{'char','cell'})}  = '';
                Args.Pos(1,1) double {mustBePositive(Args.Pos)}       = Inf;
            end
        
        end
        
        
        function replaceVal(Obj,Key,NewVal,Args)
            %
            arguments
                Obj
                Key    {mustBeA(Key,{'char','cell'})} = '';
                NewVal                                     = ''; 
                Args.NewComment                            = '';
                Args.NotExist char {mustBeMemeber(Args.NotExist,{'add','fail'})} = 'add'; 
            end
        end
        
        function search(Obj,Val,Args)
            %
            arguments
                Obj         
                Val                 
                Args.Column                  = 1;
                Args.CaseSens(1,1) logical   = true;
            end
        end
        
        
        function isKeyVal(Obj,Key,Val,Args)
            %
            arguments
                Obj
                Key    {mustBeA(Key,{'char','cell'})} = '';
                Val                                        = [];
                Args.LogicalOperator char {mustBeMemeber(Args.LogicalOperator,{'and','or'})} = 'and'; 
                Args.CaseSens(1,1) logical                      = true;
            end
        
        end
        
        % isKeyExist
        
        % julday
        
        % findGroups
        
        % getObsCoo
        
        % getCoo
        
    end
    
    
    % Unit test
    methods(Static)
        function Result = unitTest()
            %
            
            % construct an empty AstroHeader
            A=AstroHeader([2 2]);
            % read headers to AstroHeader, on construction
            A=AstroHeader('*.fits',1);
            
            
            
            fprintf("Started\n");
            conf = Config("c:/temp/conf.txt")
            val = conf.getValue("key1");
            disp(val);           
            num = conf.getNum("key3");
            disp(num);
            result = true;
        end
    end    
        
end


