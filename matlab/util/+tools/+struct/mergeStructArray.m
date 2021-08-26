function MergedStruct = mergeStructArray(StructArray)
    % Merge each field in a structure array into the same field in a single element structure.
    %   The merging is done along the first dimension.
    % Input  : - A structure array.
    % Output : - A single element structure, with the same fields as the
    %            input structure array.
    % Author : Eran Ofek (Aug 2021)
    % Example: A.A=ones(2,1,3); A(2).A=2.*ones(2,1,3); A(3).A=3.*ones(3,1,3);
    %          MergedStruct = tools.struct.mergeStructArray(A);
    
    arguments
        StructArray struct
    end
   
    N            = numel(StructArray);
    Fields       = fieldnames(StructArray);
    Nf           = numel(Fields);
    MergedStruct = StructArray(1);
    for I=2:1:N
        for If=1:1:Nf
            MergedStruct.(Fields{If}) = [MergedStruct.(Fields{If}); StructArray(I).(Fields{If})];
        end
    end
    
end
