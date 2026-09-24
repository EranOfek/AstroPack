function [AperFlux, AperArea, AperRad]=aper_phot_cube_interp(Cube, Back, X1, Y1, Args)
    % Aperture photometry with shifted constant mask
    %   For fast version see: imUtil.sources.mex.aper_phot*
    %   Perform aperture photometry for sources present in a cube of
    %   images, where the center of each source is slightly different and
    %   provided by the user. The aperture mask is pixelaized, but it is
    %   the same for all sources. This is achived by lanczos3 interpolaing
    %   the aperture mask to the position of the source.
    % Input  : - A cube of images. The image index is in the 3rd dim.
    %          - A vector of background (per image slice). This background
    %            will be subtracted from the corresponding images.
    %          - A vector of X positions of sources (per image slice).
    %          - A vector of Y positions of sources (per image slibe).
    %          * ...,key,val,...
    %            'AperRad' - A vector of aperture radii in which to
    %                   calculate the aperture photometry.
    %                   Default is [2 4 6] pixels.
    % Output : - A matrix of aperture flux per image slice (rows) and per
    %            aperture radius (columns).
    %          - A matrix of aperture area per image slice (rows) and per
    %            aperture radius (columns).
    %          - A vector of the aperture radii used.
    % Author : Eran Ofek (Feb 2026)
    % Example: [AperPhot3, AperArea3]=aper_phot_cube_interp(Cube, Bck, X1, Y1);

    arguments
        Cube
        Back
        X1
        Y1
        Args.AperRad   = [2 4 6];
    end

    AperRad = Args.AperRad;

    [Sy, Sx, Nim] = size(Cube);

    if (Sx./2)==floor(Sx./2) || (Sy./2)==floor(Sy./2)
        error('Image stamps size must be odd');
    end

    if ~isempty(Back) && Nim>1
        if numel(Back)==1
            Back = repmat(Back,1,Nim);
        end
            
        Back = reshape(Back,[1 1 Nim]);
    end

    HalfSizeY = (Sy-1).*0.5;
    HalfSizeX = (Sx-1).*0.5;

    VecX = (-HalfSizeX:1:HalfSizeX);
    VecY = (-HalfSizeY:1:HalfSizeY).';

    MatR2    = VecX.^2 + VecY.^2;
    Naper    = numel(Args.AperRad);
    AperFlux = nan(Nim,Naper);
    AperArea = zeros(1,Naper);
    for Iaper=1:1:Naper
        Mask = single(MatR2<=(Args.AperRad(Iaper).^2));
        AperArea(Iaper) = sum(Mask, 'all');

        Mask = repmat(Mask, [1 1 Nim]);

        ShiftedMask = imUtil.trans.mex.shift_lanczos3(Mask, X1, Y1);

        if isempty(Back)
            AperFlux(:,Iaper) = sum(Cube.*ShiftedMask,[1 2],'omitnan');
        else
            AperFlux(:,Iaper) = sum((Cube-Back).*ShiftedMask,[1 2],'omitnan');
        end
    end

end