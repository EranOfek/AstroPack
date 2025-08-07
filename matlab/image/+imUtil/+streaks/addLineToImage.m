function [OutImage] = addLineToImage(Image, Coords, Intensity, PSF)
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
    % Output : - Output image.
    % Author : Eran Ofek (2025 Jul) 
    % Example: Out=imUtil.streaks.addLineToImage(Image,[10 50 20 40],10,imUtil.kernel2.gauss(3));
    %          Out=imUtil.streaks.addLineToImage(Image,[10 50 20 40; 60 10 10 20],[10,1],[]);

    arguments
        Image
        Coords
        Intensity         = 1;
        PSF               = [];
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
    
        % Generate points along the line using linear interpolation
        NPoints = max(abs(MaxX - MinX), abs(MaxY - MinY)) + 1;
        X = round(linspace(MinX, MaxX, NPoints));
        Y = round(linspace(MinY, MaxY, NPoints));
    
        % Ensure coordinates are within bounds
        Valid = X >= 1 & X <= size(Image, 2) & Y >= 1 & Y <= size(Image, 1);
        X = X(Valid);
        Y = Y(Valid);
    
        % Add intensity to the line pixels
        Indices = sub2ind(size(Image), Y, X);
        OutImage(Indices) = OutImage(Indices) + Intensity(I);
    end

    % Convolve with PSF if provided
    if ~isempty(PSF)
        if numel(PSF)==1
            PSF = imUtil.kernel2.gauss(PSF);
        end
        OutImage = conv2(OutImage, PSF, 'same');
    end
end


