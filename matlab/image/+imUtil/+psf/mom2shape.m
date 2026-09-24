
function [AB, El, C] = mom2shape(X2, Y2, XY)
    % Calculate shape (A, B, Theta, Elongation,...) from 2nd moments.
    % Input  : - X^2 moment.
    %          - Y^2 moment.
    %          - X*Y moment.
    % Output : - Structure with:
    %            .Theta [rad] - position angle of major axis, measured
    %                           counterclockwise from the X axis.
    %            .A           - semi-major axis.
    %            .B           - semi-minor axis.
    %          - Structure with:
    %            .Elongation  = A/B.
    %            .Ellipticity = 1 - B/A.
    %          - Structure with inverse ellipse coefficients:
    %            .CXX
    %            .CYY
    %            .CXY
    %            Such that:
    %              CXX*x.^2 + CYY*y.^2 + CXY*x.*y = 1
    %            Note that CXY is the full coefficient of x*y. If using
    %            the matrix form [x y]*[CXX CXY; CXY CYY]*[x; y],
    %            divide C.CXY by 2.
    % Notes  : - The second moment matrix is:
    %              [X2  XY
    %               XY  Y2]
    %          - A and B are the square roots of the larger and smaller
    %            eigenvalues of the moment matrix, respectively.
    %          - Uses atan2 rather than atan in order to preserve the
    %            correct quadrant.
    %          - For X2=Y2 and XY=0, Theta is arbitrary and is returned
    %            as 0.
    %          - Small negative eigenvalues caused by numerical roundoff
    %            are clipped to zero. Truly negative eigenvalues are
    %            returned as NaN.
    % Author : Eran Ofek (Jan 2022)
    % Example: [AB, El, C] = imUtil.psf.mom2shape(1,1,0);
    %          [AB, El, C] = imUtil.psf.mom2shape(1,4,0);

    Diff2 = X2 - Y2;
    Sum2  = X2 + Y2;

    R = hypot(0.5.*Diff2, XY);

    Lambda1 = 0.5.*Sum2 + R;
    Lambda2 = 0.5.*Sum2 - R;

    % Protect against tiny negative eigenvalues due to roundoff
    Tol = 10 .* eps(max(abs(Sum2), abs(R)));

    Flag1 = Lambda1 < 0 & Lambda1 > -Tol;
    Flag2 = Lambda2 < 0 & Lambda2 > -Tol;

    Lambda1(Flag1) = 0;
    Lambda2(Flag2) = 0;

    % Invalid second-moment matrices
    Lambda1(Lambda1 < 0) = NaN;
    Lambda2(Lambda2 < 0) = NaN;

    AB.Theta = 0.5.*atan2(2.*XY, Diff2);
    AB.A     = sqrt(Lambda1);
    AB.B     = sqrt(Lambda2);

    if nargout>1
        El.Elongation  = AB.A./AB.B;
        El.Ellipticity = 1 - AB.B./AB.A;

        if nargout>2
            CosT = cos(AB.Theta);
            SinT = sin(AB.Theta);

            InvA2 = 1./AB.A.^2;
            InvB2 = 1./AB.B.^2;

            C.CXX = CosT.^2.*InvA2 + SinT.^2.*InvB2;
            C.CYY = SinT.^2.*InvA2 + CosT.^2.*InvB2;
            C.CXY = 2.*CosT.*SinT.*(InvA2 - InvB2);
        end
    end

end

% 
% function [AB, El, C] = mom2shape(X2, Y2, XY)
%     % Calculate shape (A, B, Theta, Elongation,...) from 2nd moments
%     % Input  : - X^2 moment.
%     %          - Y^2 moment.
%     %          - X*Y moment.
%     % Output : - Structure with:
%     %            .Theta [rad]
%     %            .A
%     %            .B
%     %          - Structure with:
%     %            .Elongation = A/B
%     %            .Ellipticity = 1 - B/A
%     %          - Structure with:
%     %            .CXX
%     %            .CYY
%     %            .CXY
%     % Author : Eran Ofek (Jan 2022)
%     % Example: [AB, El, C] = imUtil.psf.mom2shape(1,1,0);
% 
%     Diff2 = X2 - Y2;
%     Sum2  = X2 + Y2;
% 
%     AB.Theta = 0.5.*atan(2.*XY./Diff2);
%     AB.A     = sqrt(0.5.*Sum2 + sqrt( (0.5.*Diff2).^2 + XY.^2));
%     AB.B     = sqrt(0.5.*Sum2 - sqrt( (0.5.*Diff2).^2 + XY.^2));
% 
%     if nargout>1
%         El.Elongation  = AB.A./AB.B;
%         El.Ellipticity = 1 - AB.B./AB.A;
% 
%         if nargout>2
%             C.CXX = (cos(AB.Theta)./AB.A).^2 + (sin(AB.Theta)./AB.B).^2;
%             C.CYY = (sin(AB.Theta)./AB.A).^2 + (cos(AB.Theta)./AB.B).^2;
%             C.CXY = 2.*cos(AB.Theta).*sin(AB.Theta).*(1./(AB.A.^2) - 1./(AB.B.^2));
%         end
%     end
% 
% end