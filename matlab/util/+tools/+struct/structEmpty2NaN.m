function St = structEmpty2NaN(St, Field)
    % Replace empty fields in struct array with NaN
    % Input  : - Structure array.
    %          - Cell array of field naes, or a field name on which to go
    %            over. If empty, go over all fields.
    %            Default is empty.
    % Output : - A structure array in which the empty fields were replaced
    %            with NaN.
    % Author : Eran Ofek (Nov 2022)
    % Example: B(1).A=[1]; B(2).A=[]; B = tools.struct.structEmpty2NaN(B)
    
    arguments
        St
        Field    = {}
    end
    
    if isempty(Field)
        Field = fieldnames(St);
    else
        if ischar(Field)
            Field = {Field};
        end
    end
    
    Nf = numel(Field);
    N  = numel(St);
    for I=1:1:N
       for If=1:1:Nf
           if isempty(St(I).(Field{If}))
               St(I).(Field{If}) = NaN;
           end
       end
    end
    
end
