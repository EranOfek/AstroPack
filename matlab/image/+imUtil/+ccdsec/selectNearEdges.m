function Flag = selectNearEdges(SizeIJ, Dist)
    % construct a vector of matrix indices for pixels near the edge of the CCDSEC
    % Input  : - Image size [I, J], or CCDSEC [1 Xmax 1 Ymax].
    %          - Distance from edge to mark. Default is 5.
    % Output : - A matrix of logicals (same size as indicated in the image size)
    %            with true for pixels near the edges.
    % Author : Eran Ofek (May 2021)
    % Example: Flag = imUtil.ccdsec.selectNearEdges([10 12], 3)
   
    if numel(SizeIJ)==4
        SizeIJ = SizeIJ([2,4]);
    end
    
    Algo = 2;
    switch Algo
        case 2
            % 60 times faster compared to Algo 1
            Flag = false(SizeIJ);
            Npix = numel(Flag);
            Flag(1:SizeIJ(1).*Dist) = true;  % the pixels on the left-hand side
            Flag(Npix - SizeIJ(1).*Dist+1: Npix) = true;  % the pixels on the right-hand side
            for Id=1:1:Dist
                Flag(Id:SizeIJ(1):Npix) = true; %SizeIJ(2).*Dist) = true;
            end
            for Id=SizeIJ(1):-1:SizeIJ(1)-Dist+1
                Flag(Id:SizeIJ(1):Npix) = true; %SizeIJ(2).*Dist) = true;
            end
            
        case 1
            [MatX, MatY]    = meshgrid((1:1:SizeIJ(2)), (1:1:SizeIJ(1)));
            Flag            = false(size(MatX));
            Flag(MatX<=Dist)             = true;
            Flag(MatX>(SizeIJ(2)-Dist))  = true;
            Flag(MatY<=Dist)             = true;
            Flag(MatY>(SizeIJ(1)-Dist))  = true;
        
        otherwise
            error('Unknown Algo option');
    end
    
end