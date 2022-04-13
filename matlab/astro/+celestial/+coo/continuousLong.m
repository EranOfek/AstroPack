function LongMat = continuousLong(LongMat, Dim)
    % Given a matrix of longitude, make the longitude in columns or rows continuous.
    %   I.e., add/subtract 2.*pi from longitude such that all the values in the same
    %   column/row are not containing jumps at 0 or 2.*pi.
    % Input  : - Matrix of longitudes [radians].
    %          - Dimension along to make the lists continuous.
    %            Default is 1.
    % Output : - Matrix of continuous longitudes [radians].
    % Author : Eran Ofek (Apr 2022)
    % Example: celestial.coo.continuousLong([1 1 2 359; 4 5 6 7; 359 1 2 3],2);
    
    arguments
        LongMat
        Dim       = 1;
    end
    
    HalfPi = pi.*0.5;
    
    FlagHalf = LongMat>pi;
    
    MeanLong = mean(LongMat, Dim, 'default', 'omitnan');
    MinLong  = min(LongMat,[], Dim, 'omitnan');
    MaxLong  = max(LongMat,[], Dim, 'omitnan');
    
    FlagL = MinLong<HalfPi;
    FlagG = MaxLong>(2.*pi - HalfPi);
    
    Flag1  = FlagL & FlagG & MeanLong>pi;
    LongMat(Flag1 & ~FlagHalf) = LongMat(Flag1 & ~FlagHalf) + 2.*pi;
    
    Flag2  = FlagL & FlagG & MeanLong<=pi;
    LongMat(Flag2 & FlagHalf) = LongMat(Flag2 & FlagHalf) - 2.*pi;
    
end
    
    
