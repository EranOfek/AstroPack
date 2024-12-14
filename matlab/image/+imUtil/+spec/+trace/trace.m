function Result = trace(Array, Args)
    %
    
    arguments
        Array
        Args.Dim               = 1;   % Dim of spatial coordinate
        Args.TraceLineKernel   = [100 3 0 0];  % [Length, Width, Angle, Gap, [sigma]]
        Args.PSFsigma          = 3;
        Args.Back              = [];
        Args.Var               = [];
        Args.BackArgs cell     = {}; %'VarFun',@var, 'VarFunPar',{[],'all','omitnan'}};

        
        Args.Threshold          = 10;  % integrated 
        Args.ThresholdSum       = 3;
        Args.GlobalStd logical  = false;
        
        Args.CollapseOption     = 'sn';
        Args.MinSN              = 8;
        Args.ApproxPos          = 332;  % [];
        
        Args.GoodMask           = [];
        Args.IgnoreWaveRangePos = [0 30];
        
        Args.ExpectedPos        = [333, 500]; %[];
        Args.ExpectedPosErr     = 3;
        
        Args.Moments1dArgs      = {};
        Args.UseWeightedMom logical = true;
    end
    
    if Args.UseWeightedMom
        MomField = 'X1W';
    else
        MomField = 'X1';
    end
    
    if Args.Dim==2
        Array = Array.';
    end
    Dim = 1;
    
    % Build trace kenel
    StampSize       = Args.TraceLineKernel(1);
    if (StampSize.*0.5)==floor(StampSize.*0.5)
        % even number
        StampSize = StampSize + 1;
    end
    StampSize = [StampSize StampSize];
    
    LineKernel      = imUtil.kernel2.line([Args.TraceLineKernel, Args.PSFsigma], StampSize);
    LineDeltaKernel = imUtil.kernel2.line([Args.TraceLineKernel, 0.1],           StampSize);
    
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
    [ResCollapse,PeakDet] = imUtil.spec.trace.collapse(SNp);
    
    
    
    
    
    PeakDet.PeakPos
    PeakDet.PeakSN
    
    if isempty(Args.ExpectedPos)
        % return all possible traces
        
    else
        % return only traces consistent with ExpectedPos
        Npos = numel(Args.ExpectedPos);
        
        Diff = PeakDet.PeakPos - Args.ExpectedPos(:).';
        [MinDist,MinInd] = min(abs(Diff),[],1);
        FlagFound = MinDist<Args.ExpectedPosErr;
        
        Result = struct('Pos',cell(Npos,1), 'ExpectedPos',cell(Npos,1), 'SN',cell(Npos,1),...
                        'ResMomFilt',cell(Npos,1), 'ResMomUnFilt',cell(Npos,1),...
                        'FitMomFilt',cell(Npos,1), 'FitMomUnFilt',cell(Npos,1));
        
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
                
            end
        end
        
    end
    'a'
    
    
    % extract the trace and background
    
end