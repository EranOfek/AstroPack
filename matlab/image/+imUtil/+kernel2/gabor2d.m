function Gabor2d = gabor2d(Sigma,SizeXY,Wavelength,Args)
    %{
    Create a Gabor filter with different wavelengths in x- and y-directions.
    Input : - (Sigma) A one column or two column matrix.
            One row per Gaussian template (in the output cube).
            The columns are: (SigmaX, SigmaY).
            SigmaX is the gaussian sigma in the X direction (=FWHMx/2.35)
            If two elements are provided then the second element is
            SigmaY (sigma in the Y direction).
            If one element is given, then the default is SigmaY=SigmaX.
            If not provided then the default is 1.
            - Stamp size [X,Y]. Default is [15 15].
            - (Wavelength) A one column or two column matrix.
            One row per sinusoid wavelength template (in the output cube).
            The columns are: (WavelegnthX, WavelengthY).
            WavelengthX is the wavelength in the X direction.
            If two elements are provided then the second element is
            WavelengthY (sinusoid wavelegnth in the Y direction).
            If one element is given, then the default is WavelengthY=WavelengthX.
            If not provided then the default is 2.
            * ...,key,val,...
              'Phase' - Phase offset of sinusoid. Default is 0 rad.
              'Theta' - Rotation angle of Gaussian envelope. Default is 0 rad.
    Output :- Gabor2d (Cube containing Gabor filters).
    Author : Ruslan Konno (Apr 2024)
    Example: Gabor2d = imUtil.kernel2.gabor2d;
    %}

    arguments
        Sigma = 1;
        SizeXY = [15 15];

        Wavelength = [2 2];

        Args.Phase = 0;
        Args.Theta = 0;
    end

    % Verify sizes

    sizeWavelenth = size(Wavelength, 1);
    sizeSigma = size(Sigma, 1);
    sizePhase = size(Args.Phase, 1);
    sizeTheta = size(Args.Theta, 1);

    Sizes = [sizeWavelenth, sizeSigma, sizePhase, sizeTheta];
    SizesNonScalar = Sizes(Sizes > 1);
    Nfilter = max([max(SizesNonScalar), 1]);

    if Nfilter > 1 && any(SizesNonScalar < Nfilter)
        error([ ...
           'Any parameters among Sigma, Wavelength, Phase, and Theta, '...
           'that are matrices with more than one row have to have an equal ' ...
           'amount of rows.' ...
           ]);
    end

    % Allocate paramters for fitler bank loop
    if size(Wavelength,2) < 2
        v0 = Wavelength(:,1);
        u0 = Wavelength(:,1);
    else
        v0 = Wavelength(:,1);
        u0 = Wavelength(:,2);
    end

    if size(v0, 1) < 2
        v0 = v0 * ones(Nfilter,1);
    end
    if size(u0, 1) < 2
        u0 = u0 * ones(Nfilter,1);
    end

    if size(Sigma,2) < 2
        SigX = Sigma(:,1);
        SigY = Sigma(:,1);
    else
        SigX = Sigma(:,1);
        SigY = Sigma(:,2);
    end

    if size(SigX, 1) < 2
        SigX = SigX * ones(Nfilter,1);
    end
    if size(SigY, 1) < 2
        SigY = SigY * ones(Nfilter,1);
    end

    if sizeTheta < 2
        Theta = Args.Theta * ones(Nfilter,1);
    else
        Theta = Args.Theta(:);
    end

    if sizePhase < 2
        Phase = Args.Phase * ones(Nfilter,1);
    else
        Phase = Args.Phase(:);
    end

    % Construct real space grid

    N = SizeXY(1);
    M = SizeXY(2);

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


    % Construct Gabor filter(s)

    Gabor2d = zeros(N,M,Nfilter);
    
    for Ifilter=1:1:Nfilter
        % Rotation for Gaussian envelope
        Xxp = Xx*cos(Theta(Ifilter))  - Yy*sin(Theta(Ifilter));
        Yyp = Xx*sin(Theta(Ifilter))  + Yy*cos(Theta(Ifilter));

        % Gaussian envelope
        hGaussian = exp( -1/2*( Xxp.^2./SigX(Ifilter).^2 + ...
            Yyp.^2./SigY(Ifilter).^2))./(2.*pi.*SigX(Ifilter).*SigY(Ifilter));
        % Sinusoid
        hGaborEven = hGaussian.*cos(2*pi.*(Xx ./ u0(Ifilter) + ...
            Yy ./ v0(Ifilter))+Phase(Ifilter));
        hGaborOdd  = hGaussian.*sin(2*pi.*(Xx ./ u0(Ifilter) + ...
            Yy ./ v0(Ifilter))+Phase(Ifilter));
    
        % Create filter
        Gabor2d(:,:,Ifilter) = complex(hGaborEven,hGaborOdd);
    end
end