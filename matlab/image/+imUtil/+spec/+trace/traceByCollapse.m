function [Result, SN, SN1, ResCollapse, PeakDet]  = traceByCollapse(Array, Args)
    % Find, fit and linazrize traces from spectra in an image
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
    % Input  : - A 2D array.
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
    %            'Field1', 'Field2' - This are the fields in the output
    %                   from which the linezrized trace is generated.
    %                   Default are: 'FitMomFilt', 'FitY'.
    %
    % Output : - A structure array with element per trace, and the
    %            following fields:
    %            .Pos - The mean measured spatial position of the trace.
    %            .ExpectedPos - The expected spatial position of the trace.
    %                   Empty if no expected position.
    %            .SN - Integrated S/N of trace.
    %            .ResMomFilt - Output of imUtil.spec.trace.moment1d
    %                   measured on the filtered image.
    %            .ResMomUnFilt - Output of imUtil.spec.trace.moment1d
    %                   measured on the un-filtered (original) image.
    %            .FitMomFilt - Output of imUtil.spec.trace.fitTrace
    %                   measired on the filtered image.
    %            .FitMomUnFilt - Output of imUtil.spec.trace.fitTrace
    %                   measired on the un-filtered (original) image.
    %            .LinTraceImage - Trace cutout image in which the trace is
    %                   linear along the X (wavelength) direction.
    %            .LinTracePos - The position of the trace center in LinTraceImage
    %
    %          - S/N image.
    %          - S/N image with delta function kernel.
    %          - Collapseed S/N.
    %          - Detected peaks information (output of:
    %            imUtil.spec.trace.collapse).
    %
    % Author : Eran Ofek (Dec 2024)
    % Example: RR=imUtil.spec.trace.traceByCollapse(Array);
    
    arguments
        Array
        Args.WaveDim           = 1;   % Dim of spatial coordinate
        Args.TraceLineKernel   = [100 3 0 0];  % [Length, Width, Angle, Gap, [sigma]]
        Args.PSFsigma          = 3;
        Args.Back              = [];
        Args.Var               = [];
        %Args.BackArgs cell     = {}; %'VarFun',@var, 'VarFunPar',{[],'all','omitnan'}};

        
        Args.Threshold          = 12;  % integrated 
        Args.ThresholdSum       = 3;
        Args.GlobalStd logical  = false;
                
        Args.GoodMask           = [];
        Args.IgnoreWaveRangePos = []; %[0 30];
        
        Args.ExpectedPos        = []; %[333, 500]; %[];
        Args.ExpectedPosErr     = 3;
        
        Args.Moments1dArgs      = {};
        Args.UseWeightedMom logical = true;
        
        Args.LinTraceHalfWidth      = 50;
        Args.linearizeTraceArgs     = {};
        
        Args.Field1                 = 'FitMomFilt';
        Args.Field2                 = 'FitY';
    end
    
    if Args.UseWeightedMom
        MomField = 'X1W';
    else
        MomField = 'X1';
    end
    
    if Args.WaveDim==2
        Array = Array.';
    end
    Dim = 1;
    
    % Estimate background and variance
    if isempty(Args.Back) || isempty(Args.Var)
        % this is problematic - a different back sub approac is needed
        %[Back,Var]=imUtil.background.background(Array, Args.BackArgs{:});
        
        Back = tools.math.stat.rmean(Array(:),1,[0 0.3]);
        Var  = Back;
        
    else
        Back = Args.Back;
        Var  = Args.Var;
    end
    
    
    % Build trace kenel
    StampSize       = Args.TraceLineKernel(1);
    if (StampSize.*0.5)==floor(StampSize.*0.5)
        % even number
        StampSize = StampSize + 1;
    end
    StampSize = [StampSize StampSize];
    
    LineKernel      = imUtil.kernel2.line([Args.TraceLineKernel, Args.PSFsigma], StampSize);
    LineDeltaKernel = imUtil.kernel2.line([Args.TraceLineKernel, 0.1],           StampSize);
    
    
    
    % filter image with trace kernel
    [SN,Flux,FiltImage,FiltImageVar,Info] = imUtil.filter.filter2_sn(Array, Back, Var, LineKernel);
    % S/N for delta functions
    [SN1] = imUtil.filter.filter2_sn(Array, Back, Var, LineDeltaKernel);
    
    SNclean = SN.*(SN>SN1);
    
    % The options are:
    % 1. Very bright/faint traces
    %    collapse SNp
    %    find local maxima
    %    Clean peaks (some edge effects)
    %    choose local maxima that are near predicted position (optional)
    %    For each local maxima
    %        Extract peaks position within local max region
    %        fitTrace to positions
    %    
    % 2. faint broken traces using trace templates
    %    Skip this function and use trace templates
    
    
    GoodMask = imUtil.mask.maskByPos(Array, Args.GoodMask);
    
    % find local max in filtered image    
    [SNp, Peaks] = imUtil.spec.trace.peakDetectionFilter1(SNclean, Dim, 'Filter',[],...
                                                                      'Threshold',Args.Threshold,...
                                                                      'ThresholdSum',Args.ThresholdSum,...
                                                                      'GlobalStd',Args.GlobalStd,...
                                                                      'GoodMask',GoodMask);

    
       
    SNp(~GoodMask) = NaN;
    [ResCollapse,PeakDet] = imUtil.spec.trace.collapse(SNp, 'Threshold',Args.Threshold);
    
    
    if isempty(Args.ExpectedPos)
        % return all possible traces
        Npos = numel(PeakDet.PeakPos);
        Args.ExpectedPos = PeakDet.PeakPos(:).';
        
    else
        % return only traces consistent with ExpectedPos
        Npos = numel(Args.ExpectedPos);
    end
    
    Diff = PeakDet.PeakPos - Args.ExpectedPos(:).';
    [MinDist,MinInd] = min(abs(Diff),[],1);
    FlagFound = MinDist<Args.ExpectedPosErr;

    Result = struct('Pos',cell(Npos,1), 'ExpectedPos',cell(Npos,1), 'SN',cell(Npos,1),...
                    'ResMomFilt',cell(Npos,1), 'ResMomUnFilt',cell(Npos,1),...
                    'FitMomFilt',cell(Npos,1), 'FitMomUnFilt',cell(Npos,1),...
                    'LinTraceImage',cell(Npos,1), 'LinTracePos',cell(Npos,1),...
                    'WaveDim',cell(Npos,1), 'BestFit',cell(Npos,1));

    for Ipos=1:1:Npos
        Result(Ipos).ExpectedPos = Args.ExpectedPos(Ipos);
        if FlagFound(Ipos)
            IposMin  = MinInd(Ipos);
            Result(Ipos).Pos         = PeakDet.PeakPos(IposMin);
            Result(Ipos).SN          = PeakDet.PeakSN(IposMin);

            % fit the traces:
            Result(Ipos).ResMomFilt   = imUtil.spec.trace.moment1d(SN, PeakDet.PeakPos(IposMin), 'Dim',1, Args.Moments1dArgs{:});
            Result(Ipos).ResMomUnFilt = imUtil.spec.trace.moment1d(Array, PeakDet.PeakPos(IposMin), 'Dim',1, Args.Moments1dArgs{:});

            Result(Ipos).FitMomFilt   = imUtil.spec.trace.fitTrace([],Result(Ipos).ResMomFilt.(MomField)(:));
            Result(Ipos).FitMomUnFilt = imUtil.spec.trace.fitTrace([],Result(Ipos).ResMomUnFilt.(MomField)(:));
            
            Result(Ipos).BestFit      = Result(Ipos).(Args.Field1).(Args.Field2);
            Result(Ipos).PosPix       = (1:1:numel(Result(Ipos).BestFit)).';
            [Result(Ipos).LinTraceImage, Result(Ipos).LinTracePos] = imUtil.spec.trace.linearizeTrace(Array,Result(Ipos).(Args.Field1).(Args.Field2),...
                                    'DimWave',2,...
                                    'HalfWidth',Args.LinTraceHalfWidth,...
                                    Args.linearizeTraceArgs{:});

            Result(Ipos).WaveDim = Args.WaveDim;
        end
    end
            
end