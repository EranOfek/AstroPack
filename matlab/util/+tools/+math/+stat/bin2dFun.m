function varargout = bin2dFun(X, Y, Val, Args)
    % 2-D binning and apply functions to bins
    % Input  : - Vector of X coordinates.
    %          - Vector of Y coordinates.
    %          - Vector of values corresponding to X/Y positions.
    %          * ...,key,val,...
    %            'Fun' - A cell array of functions, to applay to the values
    %                   found in each bin. Each function is corresponding to an
    %                   output argument.
    %                   Default is {@numel, @mean, @median, @std};
    %            'FunArgs' - A cell array of cell arrays of additional arguments to
    %                   pass to each function.
    %                   Default is {{}, {'all','omitnan'}, {'all','omitnan'}, {[],'all','omitnan'}}.
    %            'RangeX' - [min, max] of X coordinates in which to
    %                   calculate the bins. If empty, then use the min and
    %                   max of the X coordinates. Default is [].
    %            'RangeY' - [min, max] of Y coordinates in which to
    %                   calculate the bins. If empty, then use the min and
    %                   max of the Y coordinates. Default is [].
    %            'StepX' - Bin size in X axis. Default is 1.
    %            'StepY' - Bin siez in Y axis. Default is 1.
    %            'Step' - Bin size in X and Y axes. If not empty, then
    %                   overriding both StepX and StepY. Default is [].
    %            'Nbin' - Number of bins in each axis [X Y], or [X]. If given, overide
    %                   all step options. Default is [].
    % Output : * One argument per Function. Each argument is a 2D matrix,
    %            corresponding to applying the function on the values in the bins.
    % Author : Eran Ofek (Jul 2021)
    % Example:
    % M=tools.math.stat.bin2dFun(rand(1000,1),rand(1000,1),rand(1000,1),'StepX',0.1,'StepY',0.1);
    % [M,~,S]=tools.math.stat.bin2dFun(rand(1000,1),rand(1000,1),rand(1000,1),'Step',0.1);
    
    arguments
        X
        Y
        Val
        Args.Fun cell      = {@numel, @mean, @median, @std};
        Args.FunArgs cell  = {{}, {'all','omitnan'}, {'all','omitnan'}, {[],'all','omitnan'}};
        Args.RangeX        = [];
        Args.RangeY        = [];
        Args.StepX         = 1;
        Args.StepY         = 1;
        Args.Step          = [];
        Args.Nbin          = [];
    end
    
    
    if isempty(Args.RangeX)
        Args.RangeX = [min(X), max(X)];
    end
    if isempty(Args.RangeY)
        Args.RangeY = [min(Y), max(Y)];
    end
    
    if isempty(Args.Nbin)
        if ~isempty(Args.Step)
            Args.StepX = Args.Step;
            Args.StepY = Args.Step;
        end
    else
        % select Step according to number of bins
        Args.Nbin  = Args.Nbin(:).'.*ones(1,2);
        Args.StepX = (Args.RangeX(2) - Args.RangeX(1))./Args.Nbin(1);
        Args.StepY = (Args.RangeY(2) - Args.RangeY(1))./Args.Nbin(2);
    end
    
    VecX = (Args.RangeX(1):Args.StepX:Args.RangeX(2));
    VecY = (Args.RangeY(1):Args.StepY:Args.RangeY(2));
    Nx   = numel(VecX) - 1;
    Ny   = numel(VecY) - 1;
    
    Nfun = numel(Args.Fun);
    if nargout>Nfun
        error('nargout is larger than number of functions');
    end
    
    varargout = cell(1,nargout);
    for Ifun=1:1:nargout
        varargout{Ifun} = zeros(Ny, Nx);
    end
    
    for Ix=1:1:Nx
        for Iy=1:1:Ny
            Flag = X>VecX(Ix) & X<=VecX(Ix+1) & Y>VecY(Iy) & Y<=VecY(Iy+1);
            ValI = Val(Flag);
            for Ifun=1:1:nargout
                varargout{Ifun}(Iy,Ix) = Args.Fun{Ifun}(ValI, Args.FunArgs{Ifun}{:});
            end
        end
    end
        
end