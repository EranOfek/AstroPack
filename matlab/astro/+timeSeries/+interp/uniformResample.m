function [IntVal, NewT] = uniformResample(T, Val, Args)
    % Uniform resampling of a non-evenly spaced time series.
    % Description: Uniform resampling of a non-evenly spaced time series.
    % Input  : - A column vector of times.
    %          - A matrix of values to resample, one column per property.
    %          * Arbitrary number of pairs of arguments: ...,keyword,value,...
    %            where keyword are one of the followings:
    %            'DeltaT' - If a scalar than this is the time step (in the tim
    %                       units) of the equally spaced resampled time series.
    %                       If a vector than this is the vector of time at
    %                       which to resample the light curve (not necesserly
    %                       evenly spaced). Default is 1.
    %            'InterpMethod' - A cell array of interpolation methods with
    %                       which to resample the light curve. A method per
    %                       column in the second input argument. If a single
    %                       method is provided than it will be applied to all
    %                       columns.
    %                       In addition to the interp1 methods, also
    %                       'interpGP' is available (call:
    %                       timeSeries.interp.interpGP).
    %                       Default is 'linear'.
    % Output : - A matrix of resampled light curves (one per column).
    %          - A vector of the resampled times.
    % Author : Eran Ofek (Aug 2017)
    %    URL : http://weizmann.ac.il/home/eofek/matlab/
    % Example: [IntVal,NewT]=timeSeries.interp.uniformResample(rand(100,1).*100,rand(100,1))

    arguments
        T
        Val
        Args.DeltaT                = 1;
        Args.InterpMethod          = 'linear';
        Args.fitrgpArgs cell       = {'Basis','linear','FitMethod','exact','PredictMethod','exact'}; %,'KernelFunction','squaredexponential'};
    end

    % check if already uniform
    UniqueDT = unique(diff(T));
    if numel(UniqueDT)==1 && (UniqueDT(1)-Args.DeltaDT)<eps
        % Data is already sampled at requested DeltaT
        % skip function
    else
        % 
        if (~iscell(Args.InterpMethod))
            Args.InterpMethod = {Args.InterpMethod};
        end
        NInt = numel(Args.InterpMethod);
        
        Ncol = size(Val,2);
        % sort
        [~,SI] = sort(T);
        Val    = Val(SI,:);
        T      = T(SI);

        % set new resampling grid
        if (numel(Args.DeltaT)>1)
            NewT = Args.DeltaT(:);
        else
            NewT = (T(1):Args.DeltaT:T(end)).';
        end

        IntVal = zeros(numel(NewT),Ncol);
        for Icol=1:1:Ncol
            % resample
            Ic = min(NInt,Icol);
            switch lower(Args.InterpMethod{Ic})
                case {'linear','nearest','next','previous','spline','pchip','cubic','v5cubic'}
                    % use interp1
                    IntVal(:,Icol) = interp1(T,Val(:,Icol),NewT,Args.InterpMethod{Ic});
                case {'interpgp'}
                    IntVal(:,Icol) = timeSeries.interp.interpGP(T, Val(:,Icol), NewT, 'fitrgpArgs',Args.fitrgpArgs);
                otherwise
                    error('Uknown InterpMethod option');
            end
        end
    end
end
