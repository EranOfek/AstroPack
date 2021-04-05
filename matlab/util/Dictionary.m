% A Dictionary class
% 
% Author: Eran Ofek (March 2021)
% Example: D=Dictionary; D.unitTest

classdef Dictionary < Component
%     properties (Dependent)
%         Family char              % Dictionary Family
%     end
    properties
        Name char                 = '';   % Dictionary name - e.g., 'HeaderKeySynonyms'
        Family char               = '';   % Dictionary Family
        Dict(1,1) struct          = struct(); % Primary = {list of alternate names}
        Conversion(1,1) struct    = struct();
    end
   
    methods % constructor
        function Obj=Dictionary(Args)
            % Dictionary constructor
            % Input  : Dictionary file name to load. Default is ''.
            
            arguments
                Args.Name char                 % search by Dictionary name
                Args.Family char               % search bt Dictionary family
                Args.NewDict             = []; 
                Args.NewConversion       = [];
            end
           
            if ~isempty(Args.NewDict)
                % set a new dictionary
                if isempty(Args.Name) || isempty(Args.Family)
                    error('When setting a new Dictionary name and family must be provided');
                end
                Obj.Name       = Args.Name;
                Obj.Family     = Args.Family;
                Obj.Dict       = Args.NewDict;
                Obj.Conversion = Args.NewConversion;
            else
                % search existing dictionary
                
                % stab:
                switch Args.Name
                    case 'Header.KeyNames.Synonyms'
                        
                        St.EXPTIME = {'AEXPTIME', 'EXPTIME','EXPOSURE'};
                        St.IMTYPE  = {'IMTYPE', 'IMGTYPE','IMAGETYP'};
                        St.READNOI = {'READNOI', 'READNOIS','RN'};
                        St.OBSLON  = {'OBSLON', 'LON','GEOLON','GEODLON','LONG'};
                        St.OBSLAT  = {'OBSLAT', 'LAT','GEOLAT','GEODLAT'};
                        Obj.Name   = 'Header.KeyNames.Synonyms';
                        Obj.Dict   = St;
                        Obj.Conversion = [];
                    case 'Header.KeyVal.IMTYPE.Synonyms'
                        St.Bias    = {'Bias'};
                        St.Dark    = {'Dark'};
                        St.Flat    = {'Flat','twfalt','domeflat'};
                        St.Science = {'Science','sci'};
                        St.Focus   = {'Focus','foc'};
                        St.Arc     = {'Arc'};
                        St.Fringe  = {'Fringe'};
                    case 'Header.Comments.Default'
                        
                        
                        
                    otherwise
                        error('Unknown Dictionary name');
                end
                        
                %
                
                %Obj = Dictionary.read('Name',Args.Name,'Family',Args.Family);
            end
            
        end
    end
    
    methods % setters/getters
        function set.Name(Obj,Val)
            % set Dictionary name and load it
            
            % search the dictionary
            Obj.Name = Val;
            %Obj.Dict = 
            %Obj.Conversion = 
        end
    end
    
    
    methods % basic functions
        function [Alt,AltConv]=searchKey(Obj,Key,Args)
            % Retun alternate names of a specific key in a single dictionary
            % Input  : - Dictionary object with a single element
            %          - A single key name.
            %          * ...,key,val,...
            %            'CaseSens' - Default is true.
            %            'SearchType' - ['strcmp'] | 'regexp'.
            % Output : - A cell array of alternate names
            %            If key is not found then return [].
            % Example: Alt=searchKey(Obj,'TYPE')
          
            arguments
                Obj(1,1)
                Key char
                Args.CaseSens(1,1) logical            = true;
                Args.SearchType char    {mustBeMember(Args.SearchType,{'strcmp','regexp'})} = 'strcmp';
            end
            
            FN  = fieldnames(Obj.Dict);
            switch Args.SearchType
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
                    error('Unknown SearchType option');
            end
            
            Ind  = find(Flag);
            if numel(Ind)>1
                error('More than one Key was found');
            elseif numel(Ind)==0
                Alt = [];
                AltConv = [];
            else
                Alt = Obj.Dict.(FN{Ind});
                if nargout>1
                    if isfield(Obj.Conversion,FN{Ind})
                        AltConv = Obj.Conversion.(FN{Ind});
                    else
                        AltConv = {};
                    end
                end
                
            end
        end
            
            
            
            
        function [Key,AltConv,AllAlt,FlagKey]=searchAlt(Obj,Alt,Args)
            % Return the key name from an alternate name in a dictionary
            % Input  : - A single element dictionary object.
            %          - A string of name to search in the alternate names.
            %          * ...,key,val,...
            %            'CaseSens' - Default is true.
            %            'SearchType' - ['strcmp'] | 'regexp'.
            % Output : - The key name in which the alternate name was
            %            found. Empty if not found.
            % Author: Eran Ofek (March 2021)
            % Example: [a,i]=D.searchAlt('AEXPTIME')
            
            arguments
                Obj(1,1)
                Alt char
                Args.CaseSens(1,1) logical            = true;
                Args.SearchType char    {mustBeMember(Args.SearchType,{'strcmp','regexp'})} = 'strcmp';
            end
            
            Idic = 1;
            FN = fieldnames(Obj(Idic).Dict);
            Nfn = numel(FN);
            FlagKey = false(Nfn,1);
            Key = [];
            AltConv = [];
            for Ifn=1:1:Nfn
                switch Args.SearchType
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
                        error('Unknown SearchType option');
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
        
    end
    
    methods % unitTest
        function unitTest(Obj)
            %
            
            St.EXPTIME = {'AEXPTIME','EXPTIME','EXPOSURE'};
            St.IMTYPE  = {'IMTYPE','TYPE','IMGTYPE','IMAGETYP'};
            Conv.EXPTIME = {@(x) x, @(x) x, @(x) x};
            Obj.Dict = St;
            Obj.Conversion = Conv;
            
            
            [Alt,AltConv] = Obj.searchKey('EXPTIME')
            [Key,AltConv,AllAlt,FlagKey] = Obj.searchAlt('AEXPTIME')
            
        end
   end
        
end
    
