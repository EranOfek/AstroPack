function [St] = structarray2struct(StAr)
    % Convert a struct array containing scalars into a structure of arrays
    % Input  : - Structure array in which each field contains a scalar.
    % Output : - A structure in which the same field contains an array with
    %            the same shape as the structure array.
    % Author : Eran Ofek (2026 Mar) 
    % Example: St=tools.struct.structarray2struct(StAr);


    FN = fieldnames(StAr);
    Nf = numel(FN);

    SizeSt = size(StAr);
    for If=1:1:Nf
        St.(FN{If}) = [StAr.(FN{If})];
        St.(FN{If}) = reshape(St.(FN{If}), SizeSt);
    end

end
