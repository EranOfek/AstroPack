function [Result] = notIndexVector(Len, NotInd)
    % Given a vector length and a list of indices, return the list of the complement indices
    %   I.e., indices that are not inthe list of indices.
    % Input  : - Vector length.
    %          - Vector of indices.
    % Output : - Vector of complemetary indices.
    % Author : Eran Ofek (2025 Apr) 
    % Example: tools.array.notIndexVector(5,[1 3]) % should return 2,4,5

    IndAll = (1:1:Len).';
    [~,Result] = setdiff(IndAll, NotInd(:), 'rows','stable');

end
