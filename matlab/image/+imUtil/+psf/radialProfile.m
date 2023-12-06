function Result = radialProfile(Image, CenterPos, Args)
    % Calculate the radial profile around a position 
    % Input  : - A 2D image or a cube of images in which the image index is
    %            in the 3rd dimension.
    %          - A [Y, X] position around to calculate the radial profile.
    %            If empty, use image center. Default is [].
    %          * ...,key,val,...
    %            'Radius' - A radius up to which to calculate the radial
    %                   profile, or a vector of radius edges.
    %                   If empty, then set it to the smallest image dim.
    %                   Default is [].
    %            'Step' - Spep size for radial edges. Default is 1.
    % Output : - A structure array with element per image.
    %            The following fields are available:
    %            .R - radius
    %            .N - number of points in each radius bin.
    %            .MeanR - Mean radius of points in bin.
    %            .MeanV - Mean image val of points in bin.
    %            .MedV - Median image val of points in bin.
    %            .StdV - Std image val of points in bin.
    %            .Sum - Sum of flux up to radius.
    % Author : Eran Ofek (Jun 2022)
    % Example: R = imUtil.psf.radialProfile(rand(100,100));
    %          R = imUtil.psf.radialProfile(rand(100,100,3));
        
    arguments
        Image
        CenterPos          = [];
        Args.Radius        = [];  % if vector than Edges
        Args.Step          = 1;
    end
        
     
    ImSize = size(Image);
    
    if isempty(CenterPos)
        CenterPos = ImSize(1:2).*0.5;
    end
    
    if isempty(Args.Radius)
        Args.Radius = min(ImSize(1:2));
    end
    
    if numel(Args.Radius)==1
        % create vector of edges
        RadiusEdges = (0:Args.Step:Args.Radius);
    else
        RadiusEdges = Args.Radius;  % user supplied a vector of edges
    end
    
    if RadiusEdges(1)~=0
        error('Radius edges must start with 0');
    end
    Radius2Edges = RadiusEdges.^2;
    R            = (RadiusEdges(2:end) + RadiusEdges(1:end-1)).*0.5;
   
    switch numel(ImSize)
        case 2
            Nim = 1;
        case 3
            Nim = ImSize(3);
        otherwise
            error('Input image must have 2 or 3 dimensions');
    end
    
    VecX  = (1:1:ImSize(2))   - CenterPos(2);
    VecY  = (1:1:ImSize(1)).' - CenterPos(1);
    
    MatR2 = (VecX.^2 + VecY.^2);
    Nrad  = numel(RadiusEdges) - 1;
    
    
    Result = struct('R',cell(Nim,1), 'MeanR',cell(Nim,1), 'MeanV',cell(Nim,1), 'MedV',cell(Nim,1), 'StdV',cell(Nim,1), 'N',cell(Nim,1), 'Sum',cell(Nim,1));
    for Iim=1:1:Nim
        Image1 = Image(:,:,Iim);
        
        Result(Iim).R     = R;
        Result(Iim).N     = zeros(Nrad,1);
        Result(Iim).MeanR = zeros(Nrad,1);
        Result(Iim).MeanV = zeros(Nrad,1);
        Result(Iim).MedV  = zeros(Nrad,1);
        Result(Iim).StdV  = zeros(Nrad,1);
        for Irad=1:1:Nrad
            FlagR  = MatR2>=Radius2Edges(Irad) & MatR2<Radius2Edges(Irad+1);
            Flag2R = MatR2<Radius2Edges(Irad+1);
            FlagR = FlagR(:);
            
            AllVal = Image1(FlagR);
            Result(Iim).N(Irad)     = sum(FlagR);
            Result(Iim).MeanR(Irad) = mean(sqrt(MatR2(FlagR)));
            Result(Iim).MeanV(Irad) = mean(AllVal);
            Result(Iim).MedV(Irad)  = median(AllVal);
            Result(Iim).StdV(Irad)  = std(AllVal);
            Result(Iim).Sum(Irad)   = sum(Image1(Flag2R),'all');
        end
    end
            
end
