
classdef Dictionary < handle
    properties
        Name char            = '';   % Dictionary name - e.g., 'HeaderKeySynonyms'
        Dic struct           = struct();
        Conversion struct    = struct();
        
    end
   
    methods % constructor
        function Obj=Dictionary(DictionaryName)
            % 
            
            arguments
                DictionaryName char    = '';
            end
           
            Obj.Name = DictionaryName;
        end
    end
    
    methods % setters/getters
        function set.Name(Obj,Val)
            % set Dictionary name and load it
            
            % search the dictionary
            Obj.Name = Val;
            %Obj.Dic = 
            %Obj.Conversion = 
        end
    end
    
    methods % basic functions
        function Alt=searchKey(Obj,Key,Args)
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
            
            FN  = fieldnames(Obj.Dic);
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
            else
                Alt = Obj.Dic.(FN{Ind});
            end
        end
            
            
            
            
        function [Key,FlagKey]=searchAlt(Obj,Alt,Args)
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
            FN = fieldnames(Obj(Idic).Dic);
            Nfn = numel(FN);
            FlagKey = false(Nfn,1);
            for Ifn=1:1:Nfn
                switch Args.SearchType
                    case 'strcmp'
                        if Args.CaseSens
                            Flag = strcmp(Obj(Idic).Dic.(FN{Ifn}),Alt);
                        else
                            Flag = strcmpi(Obj(Idic).Dic.(FN{Ifn}),Alt);
                        end
                    case 'regexp'
                        if Args.CaseSens
                            Flag = regexp(Obj(Idic).Dic.(FN{Ifn}),Alt,'match');
                        else
                            Flag = regexp(lower(Obj(Idic).Dic.(FN{Ifn})),lower(Alt),'match');
                        end

                    otherwise
                        error('Unknown SearchType option');
                end
                
                FlagKey(Ifn) = any(Flag);
                if any(FlagKey)
                    Key = FN{FlagKey};
                else
                    Key = [];
                end
            end
        end
        
    end
    
    methods % unitTest
        function unitTest(Obj)
            %
            
            St.EXPTIME = {'AEXPTIME','EXPTIME','EXPOSURE'};
            St.IMTYPE  = {'IMTYPE','TYPE','IMGTYPE','IMAGETYP'};
            Obj.Dic = St;
            
            
            D.searchKey('EXPTIME')
            D.searchAlt('AEXPTIME')
            
        end
    end
        
    
        
end
    
