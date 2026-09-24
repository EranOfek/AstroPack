function [Nnan, Nedges, FirstAndLastIsNaN] = nanSequence(M, Args)
    % Identify and classify NaNs sequence in an array of time series.
    %   A NaN sequence is a continous list of NaN in the series.
    % Input  : - An array of time series (default epoch dimension is 1).
    %          * ...,key,val,... 
    %            'Dim' - Dimension of time axis. Default is 1.
    % Output : - Nnan - number of NaNs in each sequence.
    %          - Nedges: number of NaN edges in the data.
    %          - FirstAndLastIsNaN: True if both first entry and last entry
    %            in each sequence are NaN.
    %            Interpretation:
    %            If: edges = 2 & FirstAndLastIsNaN=false
    %               then: a block of NaNs of length Nnan inside the sequence
    %            If Nedges = 2 & FirstAndLastIsNaN=true
    %               then: a block of not NaN, while the edges are NaN
    %            If Nedges = 1
    %               then: A block of NaNs near one edge
    %            Otherwise: multiple blocks of NaN.
    %          
    % Author : Eran Ofek (2025 Apr) 
    % Example: M=rand(20,2)
    %          [Nnan, Nedges, FirstAndLastIsNaN] = timeSeries.filter.nanSequence(M)

    arguments
        M
        Args.Dim                 = 1;
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
