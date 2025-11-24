function [ShiftedImage] = shift_interp(Image, DX, DY, Args)
    % Stamps/images linear shift using griddedInterpolant
    % Input  : - Image or cube of images in which the image index is in the
    %            third dimension.
    %          - Vector of DX shift per image in the cube.
    %          - Vector of DY shift per image in  the cube.
    %          * ...,key,val,... 
    %            'InterpMethod' - Interpolation method. See
    %                   griddedInterpolant for options.
    %                   Default is 'linear'.
    %            'Norm' - Normalize output image to sum of unity.
    %                   Default is false.
    % Output : - Shifted images.
    % Author : Eran Ofek (2025 Nov) 
    % Example: Image=rand(10,10);
    %          ShiftedImage=imUtil.trans.shift_interp(Image,0.5,0.5);

    arguments
        Image
        DX
        DY
        Args.InterpMethod      = 'linear';
        Args.Norm              = false;
    end


    F = griddedInterpolant(Image, Args.InterpMethod, 'previous');
    [Ny, Nx, NumSrc] = size(Image);
    [X, Y] = meshgrid((1:Nx),(1:Ny));
    ShiftedImage = repmat(0,Ny,Nx,NumSrc);
    for Isrc = 1:NumSrc
        ShiftedImage(:,:,Isrc)  = F(X+DX(Isrc),Y+DY(Isrc))';
    end

    if Args.Norm
        ShiftedImage = ShiftedImage./sum(ShiftedImage,[1 2]); % renormalize
    end

end
