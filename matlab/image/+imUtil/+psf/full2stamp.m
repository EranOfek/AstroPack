function Stamp=full2stamp(Full, Args)
    % Given a PSF contained in a full-size image, generate a stamp of the PSF.
    % Input  : - A full-size image matrix or cube of PSFs. If a cube, then
    %            the image index must be in the 3rd dimension.
    %          * ...,key,val,...
    %            'StampHalfSize' - Output stamp half size in [X,Y].
    %                   Default is [7 7] (i.e., stamp will be 15 by 15).
    %            'IsCorner' - A logical indicating if the PSF is in the
    %                   image corner (true) or center (false) in the input
    %                   full-size image.
    %                   Default is true.
    %            'Recenter' - Recenter the PSF using 1st moment estimation.
    %                   Default is false (NOT AVAILABLE).
    %            'zeroConv' - A logical indicating if to call the imUtil.psf.psf_zeroConverge
    %                   in order to smooth the edges of the PSF.
    %                   Default is true.
    %            'zeroConvArgs' - A cell array of arguments to pass to
    %                   imUtil.psf.psf_zeroConverge
    %                   Default is {}.
    %            'Norm' - Normalize the PSF stamp by this value.
    %                   If true, then will normalize the PSF by its sum
    %                   (such that integral will be 1).
    %                   Default is true.
    % Output : - A PSF (centered) in a stamp.
    % Author : Eran Ofek (May 2023)
    % Example: Stamp=imUtil.psf.full2stamp(imUtil.kernel2.gauss(2,[101 101]), 'IsCorner',false)

    arguments
        Full   % 2D array or a cube in which the image index is the 3rd dim
        Args.StampHalfSize       = [7 7];   % [X, Y]
        Args.IsCorner logical    = true;
        Args.Recenter logical    = false;
        Args.zeroConv logical    = true;
        Args.zeroConvArgs cell   = {};
        Args.Norm                = true;
    end

    SizeFull   = size(Full);

    if Args.IsCorner
        Full = ifftshift(ifftshift(Full,1),2);
        CenterFull = floor(SizeFull.*0.5); 
    else
        CenterFull = ceil(SizeFull.*0.5); 
    end

    
    

    Stamp = Full( (CenterFull(1)-Args.StampHalfSize(2)):1:(CenterFull(1)+Args.StampHalfSize(2)), (CenterFull(2)-Args.StampHalfSize(1)):1:(CenterFull(2)+Args.StampHalfSize(1)), :);

    if Args.Recenter
        M1 = imUtil.image.moment2(Stamp, Args.StampHalfSize(1)+1, Args.StampHalfSize(2)+1);
        %M1
        error('Not yet available, because maybe not needed');
    end

    if Args.zeroConv
        Stamp = imUtil.psf.psf_zeroConverge(Stamp, Args.zeroConvArgs{:});
    end

    if islogical(Args.Norm)
        if Args.Norm
            Stamp = Stamp./sum(Stamp,[1 2]);
        end
    else
        Stamp = Stamp./Args.Norm;
    end

end