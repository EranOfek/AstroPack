function [A, B, Theta] = gaussianSigma2SemiAxis(SX, SY, Rho)
    % Given a Gaussian SigmaX, SigmaY, Rho, calculate A, B, Theta
    %   See also: imUtil.psf.mom2shape
    % Input  : - SigmaX (e.g., sqrt of the 2nd moment in X)
    %          - SigmaY
    %          - Rho
    % Output : - A
    %          - B
    %          - Theta [deg]
    % Author : Eran Ofek (2026 Jan) 
    % Example: [A, B, Theta] = imUtil.psf.gaussianSigma2SemiAxis(SX, SY, Rho)


    Sum2  = SX.^2 + SY.^2;
    Diff2 = SX.^2 - SY.^2;
    Mult  = 2.*SX.*SY.*Rho;

    Delta = sqrt(Diff2.^2 + Mult.^2);

    L1 = 0.5.*(Sum2 + Delta);
    L2 = 0.5.*(Sum2 - Delta);

    % Protect against tiny negative values due to numerical roundoff
    L1 = max(L1, 0);
    L2 = max(L2, 0);

    A = sqrt(L1);
    B = sqrt(L2);

    Theta = 0.5.*atan2d(Mult, Diff2);

    % old version / incorrect
    % Sum2  = SX.^2 + SY.^2;
    % Diff2 = SX.^2 - SY.^2;
    % Mult  = 2.*SX.*SY.*Rho;
    % 
    % L1 = 0.5.*(Sum2 + sqrt(Diff2 + Mult.^2));
    % L2 = 0.5.*(Sum2 - sqrt(Diff2 + Mult.^2));
    % A  = sqrt(L1);
    % B  = sqrt(L2);
    % Theta = 0.5.*atand(Mult./Diff2);

end
