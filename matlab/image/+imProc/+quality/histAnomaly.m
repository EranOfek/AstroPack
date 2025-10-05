function [Result] = histAnomaly(AI, varargin)
    % Search for anaomlies in image histogram
    %   Specifically, search for image histogram which is clearly bi-modal.
    %   This can be due to e.g., electronic noise.
    %   Looks for peaks above RelPeakHeight, from those chose the one with
    %   max dist. If dist is in range of RangeDistPeaks then image is bad.
    % Input  : - An AstroImage obect
    %          * ...,key,val,... 
    %            'CCDSEC' - CCDSEC in which to calculate the histogram.
    %                   If empty, use all image. Default is [].
    %            'Dilute' - Dilute factor to data. Default is 1.
    %            'HistEdges' - Histogram edges.
    %                   Default is (-0.5:5:5000.5)
    %            'RelPeakHeight' - Select peak with height relative to
    %                   maximum are larger than this value.
    %                   Default is 0.2
    %            'RangeDistPeaks' - Distance range of peaks that will
    %                   define a bad image.
    % Output : - An array of logicals indicating if the bi-modal anomaly was detected
    %            in image. If true, then the image is bad.
    % Author : Eran Ofek (2025 Mar) 
    % Example: R=imUtil.image.histAnomaly(Image)

    Size    = size(AI);
    Result  = true(Size);

    Nim = numel(AI);
    for Iim=1:1:Nim
        [Result(Iim)] = imUtil.image.histAnomaly(AI(Iim).ImageData.Image, varargin{:});
    end
    
end
