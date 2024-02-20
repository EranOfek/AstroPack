function [TS, Sig, AIC] = processStatMap(TSMap, XVec, YVec, DoF, Args)
    %{
    For a given set of (x,y) coordinates on a test static map, finds the
      peak values within a square centered on each pair of given coordinates.
      Then converts peak TS values into gaussian significance assuming
      the TS values follow chi2 distribution of a given degrees of freedom.
    Input : - TSMap (Matrix containing the test statistic).
            - XVec (Vector containing the x query coordinates).
            - YVec (Vector containing the y query coordinates).
            - dist (Distance specifying the length of the search square in 
              which to search for the TS peak in). The side length of the square 
              equals to 2*dist+1.
            - DoF (Degrees of freedom of the chi2 distribution attributed to
              the test statistic values).
            * ...,key,val,...
              'HalfSizeTS' - Distance specifying the length of the search square in 
                     which to search for the TS peak in. The side length of 
                     the square equals to 2*HalfSizeTS+1. Default is 5.
    Output :- TS (Vector containing the TS peak values attributed to the (x,y) 
              query coordinates).
            - Sig (Vector containing the gaussian significance derived from 
              the TS peak values).
            - AIC (Vector containing the AIC score derived from the TS peak
              values).
    Author : Ruslan Konno (Feb 2024)
    Example: DoF = 1; TSMap = chi2rnd(DoF,100,100);
             XVec = randi(100,10,1); YVec = randi(100,10,1);
             [TS, Sig, AIC] = imUtil.properSub.processStatMap(TSMap, XVec, YVec, DoF);
    %}

    arguments
        TSMap
        XVec
        YVec
        DoF

        Args.HalfSizeTS = 5;
    end

    % check if TS map exists and is a matrix
    if isempty(TSMap)
        error("Test statistic map does not exist or is empty.");
    elseif ~ismatrix(TSMap)
        error("Test statistic map should be a matrix.");
    end
    
    NTrans = numel(XVec);
    
    % return empty results if no transients are found
    if NTrans == 0
        TS = [];
        Sig = [];
        AIC = [];
        return;
    end

    % pad TS map with zeros to account for on-edge transient queries
    TSMap = padarray(TSMap, [Args.HalfSizeTS, Args.HalfSizeTS]);

    % construct query indices relative to the search square center position
    % account for zero padded matrix by adding dist
    XRel = 0:Args.HalfSizeTS*2;
    YRel = 0:Args.HalfSizeTS*2;

    % construct and fill TS vector
    TS = zeros(NTrans,1);
    for n=1:NTrans
        % query all positions within search square centered on original 
        % (x,y) coordinates
        XQuery = XVec(n)+XRel;
        YQuery = YVec(n)+YRel;
        TS0 = TSMap(YQuery,XQuery);
        % take only the maximum TS value within search square
        TS(n) = max(TS0, [],'all');
    end
    
    % convert TS values to gaussian significance
    PVal = chi2cdf(TS, DoF, 'upper');
    Sig = -norminv(PVal);
    Sig(isinf(Sig)) = nan;

    AIC = TS - 2*DoF;
end