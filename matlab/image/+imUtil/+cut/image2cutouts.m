function [Cube, RoundX, RoundY, X, Y] = image2cutouts(Image, X, Y, MaxRadius, Args)
    % Given an image, make cutouts around selected coordinates.
    % Input  : - A 2D image, or a cube of cutouts.
    %            If a cube, then the 3rd dimension is the image index.
    %          - Vector of X positions.
    %            If empty, and the input is cube, then will set X to the
    %            center of cutout.
    %          - Vector of Y coordinates.
    %          - Radius of cutouts. Default is 12.
    %          * ...,key,val,...
    %            'mexCutout' - use imUtil.cut.mexCutout.m (true) or
    %                   imUtil.cut.find_within_radius_mat (false).
    %                   Default is true.
    %            'Circle' - If true, then will set all points outside the radius to NaN.
    %                   Default is false.
    % Output : - A cube. The 3rd dimension is the image index.
    %          - Roundex X position in cutout.
    %          - Roundex Y position in cutout.
    %          - X position.
    %          - Y position.
    % Author : Eran Ofek (Dec 2021)
    % Example: Image=rand(100,100); X=[1; 30]; Y=[1; 56];
    %          [Cube, RoundX, RoundY, X, Y] = imUtil.cut.image2cutouts(Image, X, Y);
    
    arguments
        Image
        X
        Y
        MaxRadius                 = 12;
        Args.mexCutout logical    = true;
        Args.Circle logical       = false;
    end
    

    NdimImage = ndims(Image);
    if NdimImage==2
        % Image is 2D - build a cube of 2D stamps
        %Args.mexCutout = false; %% FFU - BUG
        if Args.mexCutout
            % Note that the second argument must be a double
            [Cube] = imUtil.cut.mex.mex_cutout(Image,double([X, Y]),MaxRadius.*2+1);
            Cube   = squeeze(Cube);
            RoundX = round(X);
            RoundY = round(Y);
        else
            [Cube, RoundX, RoundY] = imUtil.cut.find_within_radius_mat(Image, X, Y, MaxRadius, Args.Circle);
        end

    elseif NdimImage==3
        % Image is already a cube of stamps
        Cube   = Image;
        if isempty(X) && isempty(Y)
            SizeCube = size(Cube);
            X        = SizeCube(2).*0.5 + 0.5;
            Y        = SizeCube(1).*0.5 + 0.5;
        end
        
        RoundX = round(X);
        RoundY = round(Y);
        [~,~,Nc] = size(Cube);
        if numel(X)==1
            RoundX = RoundX.*ones(Nc,1);
            X      = X.*ones(Nc,1);
        end
        if numel(Y)==1
            RoundY = RoundY.*ones(Nc,1);
            Y      = Y.*ones(Nc,1);
        end

    else
        error('Image number of dimensions must be 2 or 3');
    end
    
    
    
    
end