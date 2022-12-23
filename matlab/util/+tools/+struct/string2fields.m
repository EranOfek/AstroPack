function [SubSt,Failed]=string2fields(St, StringField)
    % Construct a sub structure from string of several fields
    %   E.g., if you want to select A.B.C.D from structure A, where 'B.C.D'
    %   is given as a string.
    % Input  : - A structure.
    %          - A string if fields seperated by dots.
    % Output : - Sub structure.
    %          - A logical indicating if the searched field failed.
    % Author : Eran Ofek (Dec 2022)
    % Example: A.B.C.D = 1; S=tools.struct.string2fields(A,'B.C.D');
    %          S=tools.struct.string2fields(A,'B.C');
    %          [S,F]=tools.struct.string2fields(A,'B.T');
    
    Sp = split(StringField,'.');
    
    SubSt = St;
    Failed = false;
    for I=1:1:numel(Sp)
        if isfield(SubSt, Sp{I})
            SubSt = SubSt.(Sp{I});
        else
            Failed = true;
            break;
        end
    end
    if Failed
        SubSt = [];
    end
    
end
    