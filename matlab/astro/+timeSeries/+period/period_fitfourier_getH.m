function [Hpoly, Hharm, Hconst, Stat] = period_fitfourier_getH(Time,Frequency,Args)
    % Construct a design matrix for time series, harmonies, polynom and constants to use for fitting a periodicity
    %       The fitted series is:
    %       Y = A_ + A_*T + A_*T^3,... A_*sin(2*pi*f*T*H(1)) +
    %           A_*sin(2*pi*f*T*H(2)) + ...
    %           A_*cos(2*pi*f*T*H(1)) +
    %           A_*cos(2*pi*f*T*H(2)) + ...
    %           Const_1*Flag1 + ...
    %       Here Const is a fitted additive constant to selected data
    %       points sorted by index.
    %       See period_fitfourier.m for the period fitting function.
    % Input : - Time - One column containing the time series
    %         - Frequency - a single value to prepare the design matrix with.
    %         * ...,key,val,...
    %           'Harmon' - Row vector of Harmonies to fit, e.g. [1 2 3].
    %                   Default is [1 2].
    %           'PolyN' - Row vector of polynomials degree to fit.
    %                   If empty, do not add polynomials. Default is [0].
    %           'Const' - This parameter allows to fit a different additive constant
    %                   for each group of data. The grousp are specifoed by
    %                   indexes of 1 to N. If empty, then do not fit such a
    %                   constant. Default is [].
    %
    % Output : - (Hpoly) polynum component of the design matrix
    %          - (Hharm) harmonies component of the design matrix
    %          - (Hconst) constatnt component of the design matrix
    %          - Structure with the following fields:
    %            .Npar
    %            .Dof
    % Author : David Polishook (2024 Oct)
    % Example: [Hpoly, Hharm, Hconst, Stat] = timeSeries.period.period_fitfourier_getH(1:1:100,5)

    arguments
        Time
        Frequency
        Args.Harmon = [1 2];
        Args.PolyN = [0];
        Args.Const = [];
    end

    Time = Time(:);

    Args.PolyN = Args.PolyN(:).'; % make a row vector
    
    N = numel(Time);
    % construct the independent constant matrix
    if numel(Args.Const)==N
        % apply a different additive constant for each group specified by running index
        % in Args.Const
        MaxInd = max(Args.Const);
        Hconst = zeros(N,MaxInd);
        for Imi=1:1:MaxInd
            Hconst(Args.Const==Imi, Imi) = 1;
        end
    else
        MaxInd = 0;
        Hconst = [];
    end

    % Construct polynoms matrix
    Hpoly = Time.^(Args.PolyN);
    % Construct harmonies matrix
    Hharm = [sin(2.*pi.*Frequency.*(Time.*Args.Harmon)),...
             cos(2.*pi.*Frequency.*(Time.*Args.Harmon))];
    % Keep statistics structure
    Stat.Npar = numel(Args.PolyN) + length(Args.Harmon).*2 + MaxInd;
    Stat.Dof = N - Stat.Npar;
end
