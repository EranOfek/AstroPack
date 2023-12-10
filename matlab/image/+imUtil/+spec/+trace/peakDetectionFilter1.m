function [SN, Peaks] = peakDetectionFilter1(Array, Dim, Args)
    % Detect 1-D local maxima in columns or rows using 1-D filtering.
    %   Use filtering (e.g., with a Gaussian) to find local maxima in
    %   vectors (columns or rows in a matrix) above some threshold (in
    %   units of sigma).
    %   The filtering is done simolutanosly to all columns/rows.
    % Input  : - A vector or a 2-D array.
    %          - Dimension in which to search for local maxima.
    %            Default is 1.
    %          * ...,key,val,...
    %            'Threshold' - Detection threshold in units of noise.
    %                   Default is 5.
    %            'ThresholdSum' - Threshold in units of noise. For each
    %                   local maxima above this threshold the S/N along the
    %                   other dimension will be added sqrt(sum(SN^2)).
    %                   Default is 3.
    %            'Filter' - A vector cotaining the filter. If scalar then
    %                   this is the sigma-width of a Gaussian filter.
    %                   If empty, then do not filter.
    %                   Default is 2.
    %            'FilterLen' - Filter half-length in units of sigma-width
    %                   (relevant when Filter is scalar).
    %                   Default is 4.
    %            'GlobalStd' - A logical indicating if to use the local
    %                   std, or the std of the colums/rows. The std is used
    %                   for the noise estimation. Default is false.
    % Output : - A matrix of S/N per pixel in the array.
    %          - A structure containing the followinf fields:
    %            .Flag - An array of flags indicating if a local max. is
    %                   found above threshold in each pix in the input array.
    %            .SumSN - A vector of sqrt(sum(SN^2)) for pixels with
    %                   S/N>ThresholdSum.
    % Author : Eran Ofek (May 2023)
    % Example: R=randn(100,100); R(30,:)=R(30,:)+10;
    %          [SN, Peak]=imUtil.spec.trace.peakDetectionFilter1(R);
    
    
    arguments
        Array
        Dim                     = 1;
        Args.Threshold          = 5;
        Args.ThresholdSum       = 3;
        Args.Filter             = 2;  % sigma-width
        Args.FilterLen          = 4;  % in sigma-units
        Args.GlobalStd logical  = false;
    end
    
    if Dim==2
        Array = Array.';
    elseif Dim==1
        % do nothing
    else
        error('Dim must be 1 or 2');
    end
    
    
    
    if numel(Args.Filter)==1
        % assume a gaussian
        FiltSize = ceil(Args.Filter.*Args.FilterLen);
        Filter   = normpdf((-FiltSize:1:FiltSize),0,Args.Filter).';
    else
        Filter   = Args.Filter;
    end
    
    Norm     = sqrt(sum(Filter.^2));
    
    if Args.GlobalStd
        StdArray = tools.math.stat.rstd(Array(:));
    else
        % calc std per column/wavelength
        StdArray = tools.math.stat.rstd(Array, 1);
    end
    
    
    % filter
    if isempty(Filter)
        FiltArray = Array;
    else
        FiltArray = conv2(Array, Filter, 'same');
    end
    
    StdFiltArray = StdArray .* Norm;
    
    SN  = FiltArray./StdFiltArray;
    
    if nargout>1
        % find local maxima
        Peaks.Flag = (SN > Args.Threshold).*islocalmax(SN, 1);
        SN_T  = SN.*(SN > Args.ThresholdSum);
        Peaks.SumSN = sqrt(sum(SN_T.^2, 2));
        %Peaks.PeakSN  SN(Result.PeakInd,:);
    end
        
end
