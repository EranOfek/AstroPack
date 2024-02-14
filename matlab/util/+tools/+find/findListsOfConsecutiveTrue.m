function [Result] = findListsOfConsecutiveTrue(Vector)
    % Given a vector of logicals, return a cell array of all lists of consecutive true
    % found in the vector
    % Input  : - A vector of logicals. If not logicals, then the vector
    %            will be cast into logicals.
    % Output : - A cella array in which each element contains a vector of
    %            indices of one consecutive list of true.
    % Author : Eran Ofek (2024 Feb) 
    % Example: V=[1 1 1 1 0 1 1 1 1 1 ];
    %          [Result] = tools.find.findListsOfConsecutiveTrue(V)

    
    Vector = logical(Vector);
    
    Vector = [false; Vector(:); false];
    DiffV  = diff(Vector);
    AllStart = find(DiffV==1);
    AllEnd   = find(DiffV==-1) - 1;

    N = numel(AllStart);
    if N~=numel(AllEnd)
        error('Number of start and ends are not equal');
    end

    Result = cell(N,1);
    for I=1:1:N
        Result{I} = [AllStart(I):1:AllEnd(I)];
    end


end
