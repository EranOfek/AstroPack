function Result = trace(Array, Args)
    %
    
    arguments
        Array
        Args.Dim               = 1;   % Dim of spatial coordinate
        Args.TraceLineKernel   = [100 1 0 0];  % [Length, Width, Angle, Gap, [sigma]]
        Args.PSFsigma          = 2;
        Args.Back              = [];
        Args.Var               = [];
        Args.BackArgs cell     = {};

        
        Args.Threshold          = 5;
        Args.ThresholdSum       = 3;
        Args.GlobalStd logical  = false;
    end
    
    if Args.Dim==2
        Array = Array.';
    end
    Dim = 1;
    
    % Build trace kenel
    LineKernel = imUtil.kernel2.line([Args.TraceLineKernel, Args.PSFsigma]);
    
    % Estimate background and variance
    if isempty(Args.Back) || isempty(Args.Var)
        [Back,Var]=background(Image, Args.BackArgs{:});
    else
        Back = Args.Back;
        Var  = Args.Var;
    end
    
    % filter image with trace kernel
    [SN,Flux,FiltImage,FiltImageVar,Info] = imUtil.filter.filter2_sn(Array, Back, Var, LineKernel);
    
    % find local max in filtered image    
    [SNp, Peaks] = specUtil.trace.peakDetectionFilter1(Array, Dim, 'Filter',[], 'Threshold',Args.Threshold, 'ThresholdSum',Args.ThresholdSum, 'GlobalStd',Args.GlobalStd);
    
    
    % select traces - collapse? 
    
    % fit trace
    
    % extract the trace and background
    
end