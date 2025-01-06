function [Result] = traceByCollapse(AI, varargin)
    % Find, fit and linazrize traces from spectra in an AstroImage, and output a SpecTrace object.
    %   This function is performing the following steps:
    %   1. Measure global background and variance
    %   2. Filter the 2D images with a short-line kernel for trace.
    %      This include two kernels one with the instrumental PSF and the
    %      second which is a delta function (for CR removal).
    %   3. Optionally mask bad positions in the detector
    %   4. Detect peaks in 2D filtered image.
    %   5. Collapse filtered image and find locak maxima (initial trace
    %      positions).
    %   6. For each possible trace, calculate moments along the trace and
    %      fit it with smooth polynomial.
    %   7. For each trace, extract a linarized version of the trace.
    %   Comments: The user can choose if to find all traces or only
    %   significant traces near predefined/predicted positions.
    %   The trace is calculated using several methods (see information in
    %   output section).
    %
    % Input  : - An AstroImage or a 2D array.
    %          * ...,key,val,...
    %            'WaveDim' - Dim of spatial coordinate. Default is 1.
    %            'TraceLineKernel' - 
    %            'PSFsigma' - 
    %            'Back' - An optional background image (or scalar).
    %                   If empty, then calculate a global background using:
    %                   tools.math.stat.rmean
    %                   Back and Var are used for the filtering step.
    %                   Default is [].
    %            'Var' - An optional variance image (or scalar).
    %                   If empty, then assume equal to the background image
    %                   (i.e., assuming the image noise followin Poisson
    %                   distribution).
    %                   Default is [].
    %            'Threshold' - Threshold used in:
    %                   imUtil.spec.trace.peakDetectionFilter1
    %                    Default is 12.
    %            'ThresholdSum' - ThresholdSum used in:
    %                   imUtil.spec.trace.peakDetectionFilter1
    %                   This is the threshold in units of noise. For each
    %                   local maxima above this threshold the S/N along the
    %                   other dimension will be added sqrt(sum(SN^2)).
    %                   Default is 3.
    %
    %            'GlobalStd' - ThresholdSum used in:
    %                   imUtil.spec.trace.peakDetectionFilter1
    %                   A logical indicating if to use the local
    %                   std, or the std of the colums/rows. The std is used
    %                   for the noise estimation. Default is false.
    %            'GoodMask' - An optional array of logicals indicating if
    %                   to use the pixel (true) or not (false) in the trace
    %                   finding. If empty, will use all pixels.
    %            'IgnoreWaveRangePos' - A two column matrix of
    %                   [Xstart Xend]. Pixels in these ranges will be set
    %                   with GoodMask values of false (will not be used).
    %                   Default is [].
    %
    %            'ExpectedPos' - A vector of spatial positions of expected
    %                   traces. If given then only peaks found near the
    %                   expected position (within +/- ExpectedPosErr), will
    %                   be selected. If empty, will returm all tarces
    %                   found. Default is [].
    %            'ExpectedPosErr' - Error of the positions listed in
    %                   ExpectedPos. Default is 3 pix.
    %            'Moments1dArgs' - Additional arguments to pass to:
    %                   imUtil.spec.trace.moment1d
    %                   Default is {}.
    %            'UseWeightedMom' - Return and fit the Weighted moments
    %                   (rather than the moments).
    %                   Default is true.
    %
    %            'LinTraceHalfWidth' - Half width of linearzied trace
    %                   cutout (HalfWidth argument of
    %                   imUtil.spec.trace.linearizeTrace).
    %                   Default is 50.
    %            'linearizeTraceArgs' - Cell array of additional parameters
    %                   to pass to: imUtil.spec.trace.linearizeTrace
    %            'ExtractShift' - When extracting the linarize spectrum,
    %                   this parameter indicate the shift to apply in the spatial
    %                   direction before extraction.
    %                   This maybe useful when you are interested in
    %                   extracting the sky near the trace.
    %                   Default is 0.
    %
    %            'Field1', 'Field2' - This are the fields in the output
    %                   from which the linezrized trace is generated.
    %                   Default are: 'FitMomFilt', 'FitY'.
    %
    % Output : - A SpecTrace object with the populated traces.
    %
    % Author : Eran Ofek (2025 Jan)
    % Example: Tr=imProc.spec.trace.traceByCollapse(AI);
    
   
    if isnumeric(AI)
        AI = AstroImage({AI});
    end
    
    Nim = numel(AI);
    Result = SpecTrace(size(AI));
    for Iim=1:1:Nim
        Output = imUtil.spec.trace.traceByCollapse(AI(Iim).Image, varargin{:});
        Ntrace = numel(Output);
        
        % populate the SpecTrace object
        Result(Iim).WaveDim         = Output(1).WaveDim;        
        
        Result(Iim).ExpectedSpatPos = [Output.ExpectedPos];
        Result(Iim).SN              = [Output.SN];
        Result(Iim).LinTraceImage   = {Output.LinTraceImage};
        Result(Iim).LinTracePos     = [Output.LinTracePos];
        
        for Itrace=1:1:Ntrace
            Result(Iim) = Result(Iim).setTrace([], 'X', Output(Itrace).BestFit,...
                                                    'WavePix',Output(Itrace).PosPix,...
                                                    'Intensity',Output(Itrace).Intensity,...
                                                   'X2',Output(Itrace).ResMomFilt.X2W);
        end
        
    end

end
