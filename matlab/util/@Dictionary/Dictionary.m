% A Dictionary class
%
% Author: Eran Ofek (March 2021)
% Example: D=Dictionary; D.unitTest

% #functions (autogen)
% Dictionary -
% delete - destructor
% get.FieldNames - Return field names for dictionary
% get.LastNamePart - getter for the last name part in the name
% get.NameParts - getter for NameParts - split names by '.'
% getDict -
% searchAlt - Return the key name from an alternate name in a dictionary
% searchKey - Return alternate names of a specific key in a single dictionary
% set.DictName - set Dictionary name and load it
% string2funHandle - convert dictionary items thaatom&t start with '@' to a function handle
% #/functions (autogen)
%

classdef Dictionary < Component
    
    properties (Dependent, SetAccess = private)
        NameParts
        LastNamePart char         = '';
        FieldNames cell           = {};
    end
    
    properties
        DictName char             = '';   % Dictionary name - e.g., 'HeaderKeySynonyms'
        %Family char               = '';   % Dictionary Family
        Dict(1,1) struct          = struct(); % Primary = {list of alternate names}
        Conversion
    end
   
    
    methods % constructor
        function Obj = Dictionary(Args)
            %
           
            arguments
                Args.DictName      = [];
            end
            
            Obj.DictName = Args.DictName;
            
            if isempty(Obj.DictName)
                % No dictionary
            else
                if isempty(fieldnames(Obj.Config.Data))
                    Args.DictName
                    %Obj.Config = Configuration;
                    Obj.Config.loadConfig();
                end
                Obj.Dict = eval(sprintf('Obj.Config.Data.%s',Args.DictName));
            end
        end
        
        
        function delete(Obj)
            % destructor
            Obj.DictName = '';
            Obj.Dict     = struct();
        end
        
    end
    
    methods % setters/getters
        function set.DictName(Obj,Val)
            % set Dictionary name and load it
            
            % search the dictionary
            Obj.DictName = Val;
            %Obj.Dict =
            %Obj.Conversion =
        end
        
        
        function Result = get.NameParts(Obj)
            % getter for NameParts - split names by '.'
            Result = strsplit(Obj.DictName,'.');
        end
        
        
        function Result = get.LastNamePart(Obj)
            % getter for the last name part in the name
            
            Result = Obj.NameParts{end};
        end
        
        
        function Result = get.FieldNames(Obj)
            % Return field names for dictionary
            
            Result = fieldnames(Obj.Dict);
        end
    end
    
    
    methods % basic functions
        function [Alt, AltConv] = searchKey(Obj, Key, Args)
            % Return alternate names of a specific key in a single dictionary
            % Input  : - Dictionary object with a single element
            %          - A single key name.
            %          * ...,key,val,...
            %            'CaseSens' - Default is true.
            %            'SearchAlgo' - ['strcmp'] | 'regexp'.
            % Output : - A cell array of alternate names
            %            If key is not found then return [].
            % Example: Alt=searchKey(Obj,'TYPE')
          
            arguments
                Obj(1,1)
                Key char
                Args.CaseSens(1,1) logical            = true;
                Args.SearchAlgo char    {mustBeMember(Args.SearchAlgo,{'strcmp','regexp'})} = 'strcmp';
            end
            
            FN  = fieldnames(Obj.Dict);
            switch Args.SearchAlgo
                case 'strcmp'
                    if Args.CaseSens
                        Flag = strcmp(FN,Key);
                    else
                        Flag = strcmpi(FN,Key);
                    end
                case 'regexp'
                    if Args.CaseSens
                        Flag = regexp(FN,Key,'match');
                    else
                        Flag = regexp(lower(FN),lower(Key),'match');
                    end
                otherwise
                    error('Unknown SearchAlgo option');
            end
            
            Ind  = find(Flag);
            if numel(Ind)>1
                error('More than one Key was found');
            elseif numel(Ind)==0
                Alt = [];
                AltConv = [];
            else
                Alt = Obj.Dict.(FN{Ind});
                if nargout > 1
                    if isfield(Obj.Conversion,FN{Ind})
                        AltConv = Obj.Conversion.(FN{Ind});
                    else
                        AltConv = {};
                    end
                end
                
            end
        end
            
            
        function [Key, AltConv, AllAlt, FlagKey] = searchAlt(Obj, Alt, Args)
            % Return the key name from an alternate name in a dictionary
            % Input  : - A single element dictionary object.
            %          - A string of name to search in the alternate names.
            %          * ...,key,val,...
            %            'CaseSens' - Default is true.
            %            'SearchAlgo' - ['strcmp'] | 'regexp'.
            % Output : - The key name in which the alternate name was
            %            found. Empty if not found.
            % Author: Eran Ofek (March 2021)
            % Example: [a,i]=D.searchAlt('AEXPTIME')
            
            arguments
                Obj(1,1)
                Alt
                Args.CaseSens(1,1) logical            = true;
                Args.SearchAlgo char    {mustBeMember(Args.SearchAlgo,{'strcmp','regexp'})} = 'strcmp';
            end
            
            Idic = 1;
            FN = fieldnames(Obj(Idic).Dict);
            Nfn = numel(FN);
            FlagKey = false(Nfn,1);
            Key = [];
            AltConv = [];
            AllAlt = {};
            for Ifn=1:1:Nfn
                switch Args.SearchAlgo
                    case 'strcmp'
                        if Args.CaseSens
                            Flag = strcmp(Obj(Idic).Dict.(FN{Ifn}),Alt);
                        else
                            Flag = strcmpi(Obj(Idic).Dict.(FN{Ifn}),Alt);
                        end
                    case 'regexp'
                        if Args.CaseSens
                            Flag = regexp(Obj(Idic).Dict.(FN{Ifn}),Alt,'match');
                        else
                            Flag = regexp(lower(Obj(Idic).Dict.(FN{Ifn})),lower(Alt),'match');
                        end

                    otherwise
                        error('Unknown SearchAlgo option');
                end
                                                
                FlagKey(Ifn) = any(Flag);
                if any(FlagKey(Ifn))
                    Key = FN{FlagKey};
                    AllAlt = Obj(Idic).Dict.(Key);
                    if isfield(Obj.Conversion,FN{Ifn})
                        AltConv = Obj(Idic).Conversion.(FN{Ifn}){Flag};
                    else
                        AltConv = {};
                    end
                end
            end
        end
        
        
        function Obj = string2funHandle(Obj)
            % convert dictionary items thaatom&t start with '@' to a function
            % handle
           
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                FN  = fieldnames(Obj(Iobj).Dict);
                Nfn = numel(FN);
                for Ifn=1:1:Nfn
                    Cell  = Obj(Iobj).Dict.(FN{Ifn});
                    if iscellstr(Cell)
                        Ncell = numel(Cell);
                        for Icell=1:1:Ncell
                            if strcmp(Cell{Icell}(1),'@')
                                Obj(Iobj).Dict.(FN{Ifn}){Icell} = str2func(Cell{Icell});
                            end
                        end
                    end
                end
            end
        end
        
    end
    
    
    methods(Static)
        
        function Result = getDict(DictName)
            persistent Map
            if isempty(Map)
                Map = ComponentMap('Name', 'DictionaryFromConfig');
            end
            
            % Set default database type
            if isempty(DictName)
                DictName = 'default';
            end
            
            % Search in map
            Comp = Map.find(DictName);
            if isempty(Comp)
                % Not found, create
                Comp = Dictionary('DictName', DictName);
                Comp.MapKey = DictName;
                Map.add(Comp);
            else
                % Already exist
            end
            Result = Comp;
        end
    end
    
    
    methods(Static) % unitTest
        Result = unitTest(Obj)
            % Dictionary unit test
            
   end
        
end
    
