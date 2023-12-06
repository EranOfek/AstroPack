function [Val, Std, Err, N] = sgolayInterp(T, Y, ProjectedT, TimeFrame, Args)
    % Savitzky-Golay filter and interpolation for non equally spaced data.
    % Input  : - Vector of times.
    %          - Vector of measurments.
    %          - Vector of times in which to project the measurments.
    %          - Half the time frame (window) in which to fit the data
    %            points.
    %          * ...,key,val,...
    %            'Order' - Polynomial order. Default is 1.
    %            'MinNpoint' - Minumum number of points in time frame.
    %                   If not enough points put NaN.
    % Output : - Vector of values at projected times.
    %          - Vector of std.
    %          - Vector of errors (Std/sqrt(N)).
    %          - Vector of number of fitted points.
    % Author : Eran Ofek (Dec 2021)
    % Example: [Val, Std, Err] = timeSeries.detrend.sgolayInterp(T, Y, ProjectedT, TimeFrame);
   
    arguments
        T                     % [] for uniformaly distributed
        Y
        ProjectedT
        TimeFrame
        Args.Order        = 1;
        Args.MinNpoint    = 3;
    end
    
    if isempty(T)
        error('Empty T is not yet supported');
    else
        
        Nt = numel(ProjectedT);
        
        Val = nan(Nt,1);
        Std = nan(Nt,1);
        Err = nan(Nt,1);
        
        for It=1:1:Nt-1
            Dt       = T-ProjectedT(It);
            Flag     = abs(T-ProjectedT(It))<TimeFrame;
            DtF      = Dt(Flag);
            N(It)    = numel(DtF);
            
            if N(It)>Args.MinNpoint
                H   = DtF.^(0:1:Args.Order);
                Par = H\Y(Flag);
                Resid = Y(Flag) - H*Par;
                Val(It)   = polyval(flipud(Par),0);
                Std(It)   = std(Resid);
                Err(It)   = Std(It)./sqrt(N(It));
            end
        end
    end     
    
end
