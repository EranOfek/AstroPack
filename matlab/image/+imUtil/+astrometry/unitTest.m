function Result = unitTest()
    % unitTest for the imUtil.astrometry package
    % Example: imUtil.astrometry.unitTest

    %% imUtil.astrometry.cdmatrix2rotScale
    % For a pure-rotation (no distortion) CD matrix, PA_X_deg and PA_Y_deg
    % must agree (regardless of Handedness), and PA_deg must track the
    % applied rotation correctly all the way around the circle - including
    % relative rotations that straddle the +/-180 deg wrap.
    Scale = 1./3600;
    for HBase = [-1, 1]   % exercise both Handedness conventions
        CD1 = Scale.*[HBase, 0; 0, 1];
        St1 = imUtil.astrometry.cdmatrix2rotScale(CD1(1,1), CD1(1,2), CD1(2,1), CD1(2,2));
        if abs(mod(St1.PA_X_deg - St1.PA_Y_deg + 180, 360) - 180) > 1e-9
            error('imUtil.astrometry.cdmatrix2rotScale: PA_X_deg and PA_Y_deg disagree for a pure-rotation CD matrix (Handedness=%d)', HBase);
        end

        for Beta = -180:15:180
            Rm  = [cosd(Beta), sind(Beta); -sind(Beta), cosd(Beta)];
            CD2 = CD1*Rm;
            St2 = imUtil.astrometry.cdmatrix2rotScale(CD2(1,1), CD2(1,2), CD2(2,1), CD2(2,2));

            if abs(mod(St2.PA_X_deg - St2.PA_Y_deg + 180, 360) - 180) > 1e-9
                error('imUtil.astrometry.cdmatrix2rotScale: PA_X_deg and PA_Y_deg disagree at Beta=%g (Handedness=%d)', Beta, HBase);
            end

            % Right-multiplying CD1 by Rm(Beta) shifts PA_Y_deg by +Beta for
            % Handedness=+1, and by -Beta for Handedness=-1.
            Expected = mod(HBase.*Beta + 180, 360) - 180;
            Coded    = mod(St2.PA_deg - St1.PA_deg + 180, 360) - 180;
            if abs(mod(Expected - Coded + 180, 360) - 180) > 1e-9
                error('imUtil.astrometry.cdmatrix2rotScale: PA_deg difference wrong at Beta=%g (Handedness=%d, expected %g, got %g)', Beta, HBase, Expected, Coded);
            end
        end
    end

    Result = true;
end
