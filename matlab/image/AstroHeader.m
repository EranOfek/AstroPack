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
                                                          'Occur',Args.Occur,...
                                                          'ColKey',Obj.ColKey,...
                                                          'ColVal',Obj.ColVal,...
                                                          'ColComment',Obj.ColComment);
            
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
                            Obj(Iobj).Data{Ind, Obj.ColComment} = Obj(Iobj).CommentDict.Dict.(KeyName){1};
                        end
                        
                    end
                end
            end
                    
        end
        
        function Obj = fixKeys(Obj,Args)
            % Change the name of header keywords according to their main
            % name in the keyword synonymns dictionary
            % Input  : -
            % Output : -
            % Author : Eran Ofek (Apr 2021)
            % Example: 
            
            
            
        end
        
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
        
        function insertKey(Obj, KeyValComment, Pos)
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
        
        
        function replaceVal(Obj,Key,Val,Args)
            % Replace a keyword value in headers (no dictionary in key
            % search).
            % Input  : - An AstroHeader object (multi elements supported).
            %          - A key name or a cell array of key names.
            %          - A vector or a cell array of values, corresponding to the key
            %            names.
            %          * ...,key,val,... or ...,key=val',... list
            %            'SearchAlgo' - search using: ['strcmp'] | 'regexp'
            %            'CaseSens' - Default is true.
            %            'DelDup'   - Remove duplicate keys. Default is true.
            %            'RepVal'   - Replace value. Default is true.
            %            'Comment'  - A cell array of optional comments.
            %                   If empty, then do not replace comment. Default is [].
            %            'NewKey' - A cell array of new keys to replace the old keys.
            %                   If empty, then do not replace keys.
            %                   Default is {}.
            %            'ColKey' - Column index of keys. Default is 1.
            %            'ColVal' - Column index of values. Default is 2.
            %            'ColComment' - Column index of comments. Default is 3.
            % Output : - A new cell array with the replaced keys/values.
            % Example: H=AstroHeader('WFPC2ASSNu5780205bx.fits');
            %          H.replaceVal({'COMMENT'},{''},'DelDup',false);
            
            arguments
                Obj
                Key
                Val cell
                Args.SearchAlgo char  {mustBeMember(Args.SearchAlgo,{'strcmp','regexp'})} = 'strcmp'; 
                Args.CaseSens(1,1) logical                    = true;
                Args.DelDup(1,1) logical                      = true;
                Args.RepVal(1,1) logical                      = true;
                Args.Comment                                  = [];
                Args.NewKey                                   = {};
                Args.ColKey(1,1) uint8                        = 1;
                Args.ColVal(1,1) uint8                        = 2;
                Args.ColComment(1,1) uint8                    = 3;
            end

            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                Obj(Iobj).Data = imUtil.headerCell.replaceKey(Obj(Iobj).Data, Key, Val,...
                                                              'SearchAlgo',Args.SearchAlgo,...
                                                              'CaseSens',Args.CaseSens,...
                                                              'DelDup',Args.DelDup,...
                                                              'RepVal',Args.RepVal,...
                                                              'Comment',Args.Comment,...
                                                              'NewKey',Args.NewKey,...
                                                              'ColKey',Obj(Iobj).ColKey,...
                                                              'ColVal',Obj(Iobj).ColVal,...
                                                              'ColComment',Obj(Iobj).ColComment);
                                                              
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


