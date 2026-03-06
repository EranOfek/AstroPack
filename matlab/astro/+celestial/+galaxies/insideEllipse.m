function In = insideEllipse(TargetRa, TargetDec, Ra0, Dec0, A, B, Pa)
    % Test if sky targets fall inside on-sky ellipses (tangent-plane).
    % Input  : - An array of RA to test (radians).
    %          - An array of Dec to test (radinas).
    %          - An array/scalar of ellipses center RA.
    %          - An array/scalar of ellipses center Dec.
    %          - An array/scalar of ellipses semi major axis.
    %          - An array/scalar of ellipses semi minor axis.
    %            If empty, then the same as semi major axis.
    %            Default is [].
    %          - An array/scalar of ellipses position angle (From orth
    %            Eastward).
    %            Default is 0.
    % Output : - An array of logicals indicating if coordinates are within
    %            the ellipses.
    %            Result of (U/A).^2 + (V/B).^2 <= 1
    % Notes  :
    %          - Uses gnomonic (tangent-plane) projection about each ellipse center.
    %          - Implicit expansion (MATLAB R2016b+) supports mixed scalar/array inputs.
    % Author : ChatGPT + Eran Ofek (Mar 2026)
    % Example: In=celestial.galaxies.insideEllipse(1,1,1.001,0.999,0.01)

    arguments
        TargetRa
        TargetDec
        Ra0
        Dec0
        A
        B    = [];
        Pa   = 0;
    end

    if isempty(B)
        B = A;
    end

    % --- wrap RA difference to [-pi, pi)
    Dra = wrapToPi(TargetRa - Ra0);

    % --- gnomonic projection: X=east, Y=north on tangent plane
    S0 = sin(Dec0);  C0 = cos(Dec0);
    St = sin(TargetDec); Ct = cos(TargetDec);

    Cosc = S0.*St + C0.*Ct.*cos(Dra);

    X = (Ct.*sin(Dra)) ./ Cosc;                  % east
    Y = (C0.*St - S0.*Ct.*cos(Dra)) ./ Cosc;     % north

    % If Cosc <= 0, point is >= 90 deg from center -> gnomonic undefined.
    Bad = ~(Cosc > 0) | ~isfinite(X) | ~isfinite(Y);

    % --- rotate into ellipse frame (U along major axis, V along minor axis)
    % Pa measured from North (+Y) toward East (+X)
    Spa = sin(Pa);
    Cpa = cos(Pa);

    U = X.*Spa + Y.*Cpa;        % along major axis
    V = X.*Cpa - Y.*Spa;        % along minor axis

    % --- ellipse test
    In = (U./A).^2 + (V./B).^2 <= 1;

    % invalidate bad points
    In(Bad) = false;
end

function Y = wrapToPi(X)
%wrapToPi Wrap angle to [-pi, pi)
    Y = mod(X + pi, 2*pi) - pi;
end