function Image = cutouts2image(Cube, Image, X, Y, Args)
    % Insert stamps in a cube into an image.
    % Input  : - A cube of stamps, where the 3rd dimension is the
    %            stamp/cutout index.
    %          - Image to which the cutouts will be inserted.
    %          - X position of the cutout centers in the new image.
    %          - Y position of the cutout centers in the new image.
    %          * ...,key,val,...
    %            'SubCubeX' - [start end] of stamps X coordinates to insert
    %                   to image. If empty, then will use [1 StampXsize].
    %                   Default is [].
    %            'SubCubeY' - Like 'SubCubeX', but for the Y coordinates.
    %                   Default is [].
    % Output : - The image with the inserted cutouts.
    % Author : Eran Ofek (Jan 2022)
    % Example: Nsrc = 4000; Cube = randn(15,15,Nsrc); Image = zeros(1700,1700);
    %          XY = floor(rand(Nsrc,2).*1700);
    %          Image = imUtil.cut.cutouts2image(Cube, Image, XY(:,1),XY(:,2));
    %          Image = imUtil.cut.cutouts2image(Cube(:,:,1), Image, 1, 1)
    
    arguments
        Cube
        Image
        X
        Y
        Args.SubCubeX = []; % [start, end]
        Args.SubCubeY = []; % [start, end]
    end
    
    if ~isempty(Args.SubCubeX) && ~isempty(Args.SubCubeY)
        Cube = Cube(Args.SubCubeY(1):Args.SubCubeY(2), Args.SubCubeX(1):Args.SubCubeX(2), :);
    end
    
    [Iy, Ix]      = size(Image);
    [Ny, Nx, Nim] = size(Cube);
    
    [CubeStartX, CubeEndX, StartX, EndX] = tools.array.indexWindow(Ix, X, Nx);
    [CubeStartY, CubeEndY, StartY, EndY] = tools.array.indexWindow(Iy, Y, Ny);
    
    for Iim=1:1:Nim
        Image(StartY(Iim):EndY(Iim), StartX(Iim):EndX(Iim)) = Cube(CubeStartY(Iim):CubeEndY(Iim), CubeStartX(Iim):CubeEndX(Iim), Iim);
    end
    
end
    
    
    