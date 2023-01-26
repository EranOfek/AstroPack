function [SubSt,Found]=recursiveSearchField(St, FieldName, Found)
    % Search recursively and return a field name in structure.
    % Input  : - A structure.
    %          - Field name to search.
    % Output : - The sub structure containing the requested field.
    % Author : Eran Ofek (Jan 2023)
    % St.zz = 1; SubSt=tools.struct.recursiveSearchField(St, 'zz')
   
    arguments
        St(1,1)
        FieldName
        Found     = false;
    end
    
    if Found
        SubSt = St;
    else
        List = fieldnames(St);
        Ind  = find(strcmp(List, FieldName));
        
        
        if (numel(Ind))==1
            SubSt = St.(FieldName);
            Found = true;
        else
            Nlist = numel(List);
            Found=false;
            for Ilist=1:1:Nlist
                [SubSt,Found] = tools.struct.recursiveSearchField(St.(List{Ilist}), FieldName, Found);
                if Found
                    break;
                end
            end
        end
    end
    
end

