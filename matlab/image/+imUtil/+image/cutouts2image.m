function Image = cutouts2image(Cube, Image, X, Y, Args)
    %
    % Example: Nsrc = 4000; Cube = randn(15,15,Nsrc); Image = zeros(1700,1700);
    %          XY = floor(rand(Nsrc,2).*1700);
    %          Image = imUtil.image.cutouts2image(Cube, Image, XY(:,1),XY(:,2));
    %          Image = imUtil.image.cutouts2image(Cube(:,:,1), Image, 1, 1)
    
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
    % cutouts half size (good for odd size stamps)
    HNx = (Nx - 1).*0.5;
    HNy = (Ny - 1).*0.5;
    
    
    % Image positions
    StartX = X - HNx;
    StartY = Y - HNy;
    EndX   = X + HNx;
    EndY   = Y + HNy;
    
    CubeStartX = max(2  - StartX, 1);
    CubeEndX   = max(Nx,      Nx - max(Nx - EndX,   0);
    CubeStartY = max(2  - StartY, 1);
    CubeEndY   = Ny - max(Iy - EndY,   0);
    
    StartX     = max(StartX, 1);
    StartY     = max(StartY, 1);
    EndX       = min(EndX,   Ix);
    EndY       = min(EndY,   Iy);
    
    
    for Iim=1:1:Nim
        %Image(StartY(Iim):EndY(Iim), StartX(Iim):EndX(Iim)) = Cube(:,:,Iim);
        Image(StartY(Iim):EndY(Iim), StartX(Iim):EndX(Iim)) = Cube(CubeStartY:CubeEndY, CubeStartX:CubeEndX, Iim);
    end
    
    
    
    
end
    
    
    