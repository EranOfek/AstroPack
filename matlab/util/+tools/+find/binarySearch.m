function Pos=binarySearch(Vec, Val)
    % call the Avi Ziskind binarySearch function, but clean all the NaNs prior to the operation
    % Input  : - A sorted vector.
    %          - A vector of values to search.
    % Output : - A vector of positions at which the values where found
    % Example:
    
    Pos = binarySearch(Vec(~isnan(Vec)), Val);
end