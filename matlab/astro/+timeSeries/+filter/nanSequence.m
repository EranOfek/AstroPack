function [Result] = nanSequence(M, Args)
    % One line description
    %     Optional detailed description
    % Input  : - 
    %          - 
    %          * ...,key,val,... 
    % Output : - 
    % Author : Eran Ofek (2025 Apr) 
    % Example: 

    arguments
        M
        Args.Dim                 = 1;
        Args.B                 = [];
    end


    if Args.Dim==2
        M = M.';
    end

    [Nepoch, Nsrc ] = size(M);

    InanM = isnan(M);
    DN = diff(InanM);

    Nnan  = sum(InanM);
    Nedges = sum(abs(DN));
    FirstAndLastIsNaN = isnan(M(1,:)) & isnan(M(end,:));
    
    % interpretation:
    % Nedges = 2 & FirstAndLastIsNaN=false -> a block of NaNs of length Nnan inside the sequence
    % Nedges = 2 & FirstAndLastIsNaN=true -> a block of not NaN, while the edges are NaN
    % Nedges = 1 -> A block of NaNs near one edge
    % Otherwise: NaN/not NaN mix.


end
