function [Result] = cdmatrix2rotScale(CD1_1, CD1_2, CD2_1, CD2_2)
    % Given a WCS CD matrix, return the PA, scale, and handness.
    % Input  : - CD1_1 or 2x2 CD matrix
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

   
    if nargin==1
        CD1_1 = CD(1,1);
        CD2_1 = CD(2,1);
        CD1_2 = CD(1,2);
        CD2_2 = CD(2,2);
    end
        

    Result.Handedness = sign(CD1_1.*CD2_2 - CD1_2.*CD2_1);

    Result.PA_Y_deg = atan2d(CD1_2, CD2_2);       % PA of +Y, East of North
    % PA of +X, East of North -> PA of +Y East of North. The +/-90 deg
    % correction rotates the +X axis estimate onto the +Y axis, and its
    % sign must follow the Handedness: for Handedness=-1 (the common,
    % unmirrored astronomical convention) using a fixed -90 here made
    % PA_X_deg come out 180 deg away from PA_Y_deg for every CD matrix.
    Result.PA_X_deg = mod(atan2d(CD1_1, CD2_1) - 90.*Result.Handedness + 180, 360) - 180;
    % Circular mean: PA_X_deg and PA_Y_deg are angles wrapped to (-180,180],
    % so a plain arithmetic mean would be wrong by 180 deg whenever they
    % fall on opposite sides of the wrap.
    Result.PA_deg   = atan2d(sind(Result.PA_X_deg) + sind(Result.PA_Y_deg), ...
                              cosd(Result.PA_X_deg) + cosd(Result.PA_Y_deg));
    Result.ScaleX   = hypot(CD1_1, CD2_1);
    Result.ScaleY   = hypot(CD1_2, CD2_2);
    Result.Scale    = 0.5.*(Result.ScaleX + Result.ScaleY);


end
