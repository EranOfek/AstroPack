function [ModelY, X] = vShape(X, MinX, MinY, SlopeDec, SlopeInc)
    % Construct a V shape function.
    %     The function has a linear decreaing branch (SlopeDec) and a linear
    %     increasing branch (SlopeInc), the two branches are meeting at
    %     MinX. In order for the value at MinX to be above zero, ModelY  = sqrt(ModelY.^2 + MinY.^2)
    %     This function mimics the FWHM as a function of focus position for
    %     a telescope.
    % Input  : - Vector of X values at which to calculate the function.
    %          - MinX.
    %          - MinY.
    %          - SlopeDec.
    %          - SlopeInc.
    % Output : - ModelY
    %          - X
    % Author : Eran Ofek (2024 Nov) 
    % Example: [ModelY, X] = tools.math.fun.vShape(X, MinX, MinY, SlopeDec, SlopeInc)
    %          [ModelY, X] = tools.math.fun.vShape((-100:1:100), 0, 10, -1, +1)


    % Construct the V shape function
    FlagDec = X<=MinX;
    B_Dec   = -SlopeDec.*MinX;
    FlagInc = X>MinX;
    B_Inc   = -SlopeInc.*MinX;
    
    ModelY  = SlopeDec.*X + B_Dec;
    ModelY(FlagInc) = SlopeInc.*X(FlagInc) + B_Inc; 
    ModelY  = sqrt(ModelY.^2 + MinY.^2);
    
end
