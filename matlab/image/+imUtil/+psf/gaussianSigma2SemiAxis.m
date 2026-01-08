function [A, B, Theta] = gaussianSigma2SemiAxis(SX, SY, Rho)
    % Given a Gaussian SigmaX, SigmaY, Rho, calculate A, B, Theta
    %   See also: imUtil.psf.mom2shape
    % Input  : - SigmaX
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

    L1 = 0.5.*(Sum2 + sqrt(Diff2 + Mult.^2));
    L2 = 0.5.*(Sum2 - sqrt(Diff2 + Mult.^2));
    A  = sqrt(L1);
    B  = sqrt(L2);
    Theta = 0.5.*atand(Mult./Diff2);

end
