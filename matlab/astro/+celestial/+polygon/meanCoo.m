function [MeanRA, MeanDec] = meanCoo(RA, Dec, Args)
    % Calculate mean of RA, Dec coordinates along some dimension.
    % Input  : - RA
    %          - Dec
    %          * ...,key,val,... 
    %            'CooUnits' - Input and output units ['rad'|'deg'].
    %                   Default is 'rad'.
    %            'Dim' - Dimension along to calculate the mean.
    %                   Default is 1.
    % Output : - Mean RA
    %          - Mean Dec
    % Author : Eran Ofek (2026 Jun) 
    % Example: [MeanRA, MeanDec] = celestial.polygon.meanCoo(rand(4,3),rand(4,3))

    arguments
        RA 
        Dec
        Args.CooUnits          = 'rad';
        Args.Dim               = 1;
    end
    RAD = 180./pi;

    switch Args.CooUnits
        case 'rad'
            UnitConv = 1;
        case 'deg'
            UnitConv = RAD;
        otherwise
            error('Unknown CooUnits option');
    end

    % convert to radians
    RA  = RA./UnitConv;
    Dec = Dec./UnitConv;
    [X, Y, Z] = celestial.coo.coo2cosined(RA, Dec);
    MeanX = mean(X, Args.Dim);
    MeanY = mean(Y, Args.Dim);
    MeanZ = mean(Z, Args.Dim);

    [MeanRA, MeanDec] = celestial.coo.cosined2coo(MeanX, MeanY, MeanZ); % [rad]
    MeanRA  = MeanRA.*UnitConv;
    MeanDec = MeanDec.*UnitConv;

end
