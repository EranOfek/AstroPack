function [Result] = std_mad(X, Type, Dim)
    % Std using the mean absolute deviation (mad) function
    %   Note this is much faster then using mad, due to the omitnan option.
    % Input  : - Array of data
    %          - Type: 0 use mean absolute deviation
    %            1 use median absolute deviation.
    %            Default is 0.
    %          - Dimension along to calculate mad. Default is 1.
    % Output : - Robust std based on mad * 1.253
    % Author : Eran Ofek (2024 Jan) 
    % Example: tools.math.stat.std_mad(randn(1e6,1));

    arguments
        X
        Type   = 0;
        Dim    = 1;
    end

    if Type==0
        Result = 1.253.*mean(abs(X - mean(X,Dim, 'omitnan')),Dim, 'omitnan');
    else
        Result = 1.4826.*median(abs(X - mean(X,Dim, 'omitnan')),Dim, 'omitnan');
    end

end
