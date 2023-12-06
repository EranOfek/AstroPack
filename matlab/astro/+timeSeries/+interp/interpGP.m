function [NewY, NewT] = interpGP(T, Y, NewT, Args)
    % Interpolate a time series using Gaussian processes
    % Input  : - Vector of times.
    %          - Vector of Y(t).
    %          - Vector of new times in which to predict/interpolate the
    %            time series. If [], then use [min(T):1:max(T)]'.
    %            If Scalar, then use: [min(T):Scalar:max(T)]'.
    %            Default is [].
    %          * ...,key,val,...
    %            'fitrgpArgs' - A cell array of additional arguments to
    %                   pass to fitrgp.
    %                   Default is {'Basis','linear','FitMethod','exact','PredictMethod','exact'}
    % Output : - Vector of New Y.
    %          - Vector of New T.
    % Author : Eran Ofek (Mar 2022)
    % Example: X=sort(rand(1000,1).*100);
    % Y=sin(X);
    % [NewY, NewT] = timeSeries.interp.interpGP(X,Y,0.01);
    
    arguments
        T
        Y
        NewT                      = [];
        Args.fitrgpArgs cell      = {'Basis','linear','FitMethod','exact','PredictMethod','exact'}; %,'KernelFunction','squaredexponential'};
    end
    
    switch numel(NewT)
        case 0
            NewT = (min(T):1:max(T)).';
        case 1
            NewT = (min(T):NewT:max(T)).';
        otherwise
            % use NewT as is
    end
    
    GP   = fitrgp(T,Y, Args.fitrgpArgs{:});
    NewY = GP.predict(NewT);
    
end
    
