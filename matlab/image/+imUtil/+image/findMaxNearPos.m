function [Max,Dist] = findMaxNearPos(Image, X, Y, Args)
    % Given an image and list of coordinates, find max values near the list of positions.
    % Input  : - A 2D image.
    %          - Vector of X coordinates in image.
    %          - Vector of Y coordinates in image.
    %          * ...,key,val,... 
    %            'Radius' - Radius of search box around coordinates.
    %                   Default is 3 pixels.
    %            'mexCutout' - Use mex for generating cutouts.
    %                   Default is true.
    %            'ComparisonMethod' - Argument for builtin max function.
    %                   Default is 'auto'.
    %            'Circle' - Cutout circle. Default is false.
    % Output : - A vector of max values around each requested positions.
    %          - A vector of distances of max value from requested
    %            positions.
    % Author : Eran Ofek (2024 Sep) 
    % Example: [Max,Dist]=imUtil.image.findMaxNearPos(Image, X, Y, 'Radius',3)

    arguments
        Image
        X
        Y
        Args.Radius               = 3;
        Args.mexCutout logical    = true;
        Args.ComparisonMethod     = 'auto'; % 'auto'|'real'|'abs'
        Args.Circle logical       = false;
    end

    [Cube, RoundX, RoundY, X, Y] = imUtil.cut.image2cutouts(Image, X, Y, Args.Radius, 'mexCutout',Args.mexCutout);
    [Max, Ind] = max(Cube,[],[1 2],'linear', 'ComparisonMethod',Args.ComparisonMethod);
    Max = squeeze(Max);
    Ind = squeeze(Max);
    [I,J,~] = ind2sub(size(Cube), Ind);
    PosX = RoundX + J - Args.Radius-1;
    PosY = RoundY + I - Args.Radius-1;

    Dist = sqrt((PosX - X).^2 + (PosY - Y).^2);

end
