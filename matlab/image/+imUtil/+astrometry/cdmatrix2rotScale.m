function [Result] = cdmatrix2rotScale(CD1_1, CD1_2, CD2_1, CD2_2)
    % Given a WCS CD matrix, return the PA, scale, and handness.
    % Input  : - CD1_1
    %          - CD1_2
    %          - CD2_1
    %          - CD2_2
    % Output : - Structure with the following fields:
    %            .PA_Y_deg
    %            .PA_X_deg
    %            .PA_deg - mean PA
    %            .ScaleX
    %            .ScaleY
    %            .Scale
    %            .Handedness
    % Author : Eran Ofek (2025 Nov) 
    % Example: [Result] = imUtil.astrometry.cdmatrix2rotScale(CD1_1, CD1_2, CD2_1, CD2_2)

   
    Result.PA_Y_deg = atan2d(CD1_2, CD2_2);       % PA of +Y, East of North
    Result.PA_X_deg = atan2d(CD1_1, CD2_1) - 90;       % PA of +X, East of North - > PA of +Y East of North
    Result.PA_deg   = 0.5.*(Result.PA_X_deg + Result.PA_Y_deg);
    Result.ScaleX   = hypot(CD1_1, CD2_1);
    Result.ScaleY   = hypot(CD1_2, CD2_2);
    Result.Scale    = 0.5.*(Result.ScaleX + Result.ScaleY);
    Result.Handedness = sign(CD1_1.*CD2_2 - CD1_2.*CD2_1);         


end
