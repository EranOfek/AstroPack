function [Cat] = getCat(Obj, Args)
    % Get only the AstroCatalaog from AstroImage or AstroCatalog and populate JD
    % Input  : - Astrocatalog or AstroImage object.
    %          * ...,key,val,... 
    %            'JD' - Optional JD to inset to the JD feild in the
    %                   AstroCatalog. If empty, try to obtain from other
    %                   metadata.
    %                   Default is [].
    %            'CreateNewObj' - Create a new AstroCatlog object.
    %                   Default is true.
    % Output : - An Astrocatalog object
    % Author : Eran Ofek (2023 Dec) 
    % Example: 

    arguments
        Obj              % AstroCatalog|AstroImage
        
        Args.JD                   = []; % if given, then override JD
        Args.CreateNewObj logical = false;
    end
    
    Nobj = numel(Obj);
    for Iobj=1:1:Nobj
        
    if isa(Obj(Iobj), 'AstroImage')
        Cat(Iobj) = Obj(Iobj).CatData;
        if isempty(Args.JD)
            Cat(Iobj).JD = Obj(Iobj).julday;
        end        
    else
        % assume Obj is AstroCatalog
        Cat(Iobj) = Obj(Iobj);
    end
    
    if ~isempty(Args.JD)
        Cat(Iobj).JD = Args.JD;
    end
    

end
