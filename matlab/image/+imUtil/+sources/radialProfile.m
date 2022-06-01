function Result = radialProfile(Image, Args)
    % Calculate the radial profile around a position 
   
    
    arguments
        Image
        Args.Radius        = [];  % if vector than Edges
        Args.Step          = 1;
    end
        
     
    ImSize = size(Image);
    
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
    
    switch numel(ImSize)
        case 2
            Nim = 1;
        case 3
            Nim = ImSize(3);
        otherwise
            error('Input image must have 2 or 3 dimensions');
    end
    
    VecX = (1:1:ImSize(2));
    VecY = (1:1:ImSize(1)).';
    
    MatR2 = VecX.^2 + VecY.^2;
    
    MatR2./RadiusEdges
    
    
    
    
end
