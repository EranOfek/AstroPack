function Gabor2d = gabor2d(N,M,kn,km,Args)
    %{
    Get a Gabor filter for with different wavelengths in x- and y-directions.
    Input : - N (Size of filter in x-direction).
            - M (Size of filter in y-direction).
            - kn (sinusoid wavelength in x-direction).
            - km (sinusoid wavelength in y-direction)
            * ...,key,val,...
              'SigX' - Standard deviation of the Gaussian envelope in x.
                     Default is 1.
              'SigY' - Standard deviation of the Gaussian envelope in y.
                     Default is 1.
              'Phase' - Phase offset of sinusoid. Default is 0 rad.
              'Theta' - Rotation angle of Gaussian envelope. Default is 0 rad.
    Output :- Gabor2d (Matrix containing Gabor filter).
    Author : Ruslan Konno (Apr 2024)
    Example: Gabor2d = imUtil.filter.gabor2d(11,11,2,2);
    %}

    arguments
        N
        M

        kn;
        km;

        Args.SigX = 1;
        Args.SigY = 1;

        Args.Phase = 0;
        Args.Theta = 0;
    end
    
    % Construct real space grid
    if mod(N,2)
        Ns = (-floor(N/2):floor(N/2));
    else
        Ns = (-ceil(N/2):floor(N/2)-1);
    end
    if mod(M,2)
        Ms = (-floor(M/2):floor(M/2));
    else
        Ms = (-ceil(M/2):floor(M/2)-1);
    end

    [Xx,Yy] = meshgrid(Ns,Ms);

    % Add rotation for Gaussian envelope
    Xxp = Xx*cos(Args.Theta)  - Yy*sin(Args.Theta);
    Yyp = Xx*sin(Args.Theta)  + Yy*cos(Args.Theta);

    % Wavelength to frequency

    u0 = km;
    v0 = kn;

    % Construct Gabor filter
    hGaussian = exp( -1/2*( Xxp.^2./Args.SigX.^2 + Yyp.^2./Args.SigY.^2))...
        /(2*pi*Args.SigX*Args.SigY);
    hGaborEven = hGaussian.*cos(2*pi.*(Xx ./ u0 + Yy ./ v0)+Args.Phase);
    hGaborOdd  = hGaussian.*sin(2*pi.*(Xx ./ u0 + Yy ./ v0)+Args.Phase);

    Gabor2d = complex(hGaborEven,hGaborOdd);
end