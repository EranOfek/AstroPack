function [SinAng, CosAng, TanAng] = sincos(Ang)
    % Return the sin, cos, and optionally the tan of an angle
    %   This may be factor of two faster than calculating them separately.
    % Input  : - Array of angles [radians].
    % Output : - Sin of angles.
    %          - Cos of angles.
    %          - Tan of angles.
    % Author : Eran Ofek (2025 Jan) 
    % Example: tic;for i=1:1:100, [a1,b1,c1]=tools.math.fun.sincos(R);end,toc
    %          tic;for i=1:1:100, [a1,b1]=tools.math.fun.sincos(R);end,toc
    %          tic;for i=1:1:100, a1=sin(R); b1=cos(R); c1=tan(R)end,toc   

    SinAng = sin(Ang);
    CosAng = sqrt(1 - SinAng.^2);
    
    if nargout>2
        TanAng = SinAng./CosAng;
    end
    
end
