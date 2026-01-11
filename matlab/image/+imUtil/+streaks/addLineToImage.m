function [OutImage] = addLineToImage(Image, Coords, Intensity, PSF, Curvature, Args)
    % One line description
    %     Optional detailed description
    % Input  : - Image:     2D matrix (original image)
    %          - Coords:    A 4 column matrix, of [MinX, MaxX, MinY, MaxY],
    %            of the coordinates of the lines to add to image.
    %          - Intensity: A vector of intensities (per line) to add along
    %            each line. Default is 1.
    %          - PSF:       Point Spread Function (2D array) to convolve
    %            with the image.
    %            if empty, no convolution is done. If scalar, then this is
    %            the sigma width of the Gaussian PSF.
    %          - Curvature of line measured in units of maximum deviation
    %            from a straight line. Negative number means curved downward.
    %            Default is 0.
    %          * ...,key,val,...
    %            'Norm' - one of the following normalization options:
    %                   'None' - Default.
    %                   'LxI' - (Conserve flux) Normalize the line intensity such that its
    %                           integral equal to Length X Intensity.
    % Output : - Output image.
    % Author : Eran Ofek (2025 Jul) 
    % Example: Out=imUtil.streaks.addLineToImage(Image,[10 50 20 40],10,imUtil.kernel2.gauss(3));
    %          Out=imUtil.streaks.addLineToImage(Image,[10 50 20 40; 60 10 10 20],[10,1],[]);

    arguments
        Image
        Coords
        Intensity         = 1;
        PSF               = [];
        Curvature         = 0;
        Args.Norm         = 'none';
    end

    % Copy image to output
    OutImage = Image;

    N = size(Coords,1);
    for I=1:1:N
        % Extract coordinates
        MinX = Coords(I,1);
        MaxX = Coords(I,2);
        MinY = Coords(I,3);
        MaxY = Coords(I,4);
    
        if Curvature==0
            % Generate points along the line using linear interpolation
            NPoints = max(abs(MaxX - MinX), abs(MaxY - MinY)) + 1;
            X = round(linspace(MinX, MaxX, NPoints));
            Y = round(linspace(MinY, MaxY, NPoints));
        else
            [X,Y]=curvedLine(MinX, MaxX, MinY, MaxY, Curvature);
        end
            
        % Ensure coordinates are within bounds
        Valid = X >= 1 & X <= size(Image, 2) & Y >= 1 & Y <= size(Image, 1);
        X = X(Valid);
        Y = Y(Valid);
    
        % Add intensity to the line pixels
        Indices = sub2ind(size(Image), Y, X);

        switch lower(Args.Norm)
            case 'none'
                % do nothing
            case 'lxi'
                % noramlzie by Length X Intensity
                % conserve flux
                Length = sqrt((MinX-MaxX).^2 + (MinY-MaxY).^2);
                Brightness(Indices) = Intensity*Length./numel(Indices);

            otherwise
                error('Unknown Norm option');
        end
        OutImage(Indices) = OutImage(Indices) + Brightness(Indices);
    end

    % Convolve with PSF if provided
    if ~isempty(PSF)
        if numel(PSF)==1
            PSF = imUtil.kernel2.gauss(PSF);
        end
        OutImage = conv2(OutImage, PSF, 'same');
    end
end


function [X,Y]=curvedLine(MinX, MaxX, MinY, MaxY, Curv)
    % Inputs: MinX, MinY, MaxX, MaxY, Curv
    % Curv is max deviation from the straight line (in pixels), Curv=0 => straight
    
    Dx = MaxX - MinX;
    Dy = MaxY - MinY;
    
    NPoints = max(abs(Dx), abs(Dy)) + 1;
    
    t = linspace(0, 1, NPoints);              % parameter along the chord
    
    % Straight line (baseline)
    X0 = MinX + Dx .* t;
    Y0 = MinY + Dy .* t;
    
    % Unit normal to the chord (perpendicular direction)
    L = hypot(Dx, Dy);
    if L == 0
        X = round(X0);
        Y = round(Y0);
        return
    end
    Nx = -Dy / L;
    Ny =  Dx / L;
    
    % Parabolic profile: 0 at t=0,1 and max=1 at t=0.5
    Profile = 4 .* t .* (1 - t);              % in [0,1], peak 1 at mid
    
    % Apply curvature as offset along the normal
    Offset = Curv .* Profile;                 % max deviation = Curv
    
    X = round(X0 + Offset .* Nx);
    Y = round(Y0 + Offset .* Ny);
    
    % Optional: remove duplicates caused by rounding (keeps order)
    Keep = [true, diff(X)~=0 | diff(Y)~=0];
    X = X(Keep);
    Y = Y(Keep);
end



