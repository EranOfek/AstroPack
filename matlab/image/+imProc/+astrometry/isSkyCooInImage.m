function [Result,MaxMinDist] = isSkyCooInImage(Obj, Alpha, Delta, Args)
    % Check if RA/Dec coordinates are within image boundries
    %   Check if all or any of the coordinates are within each one of the
    %   images (i.e., all the coordinates are checked for each image).
    % Input  : - An AstroImage object.
    %          - J2000.0 R.A.
    %          - J2000.0 Dec.
    %          * ...,key,val,...
    %            'CCDSEC' - CCDSEC. Either [xmin xmax ymin ymax] or
    %                   [xmax, ymax], or empty. If empty, use image size.
    %                   Default is [].All
    %            'Units' - Input coordinate units. Default is 'deg'.
    %            'AllCoo' - Logical indicating if to use @all (true) or
    %                   @any (false). True will check if all coordinates
    %                   are within each one of the images.
    %                   Default is true.
    % Output : - A vector of logical. Each logical indicate if all/any of
    %            the coordinates are within the image footprint.
    %          - A vector of maximum of minium distances from image
    %            boundries.
    % Author : Eran Ofek (Jan 2023)
    % Example: [a,b]=imProc.astrometry.isSkyCooInImage(Coadd, RA,Dec);
    
    arguments
        Obj AstroImage
        Alpha
        Delta
        Args.CCDSEC            = [];
        Args.Units             = 'deg';
        Args.AllCoo logical    = true;
    end

    Nobj = numel(Obj);
    
    

    if isempty(Args.CCDSEC)
        [Ny, Nx] = Obj.sizeImage;
        CCDSEC   = [ones(Nobj,1) Nx(:) ones(Nobj,1) Ny(:)];
    else
        CCDSEC   = Args.CCDSEC.*ones(Nobj,4);
    end

    MaxMinDist = nan(Nobj,1);
    Result = false(Nobj,1);
    for Iobj=1:1:Nobj
        if imProc.astrometry.isSuccessWCS(Obj(Iobj))  

            [InImage,MinDist] = isSkyCooInImage(Obj(Iobj).WCS, Alpha, Delta, CCDSEC(Iobj,:), Args.Units);
            MaxMinDist(Iobj) = max(MinDist);

            if Args.AllCoo
                Result(Iobj) = all(InImage);
            else
                Result(Iobj) = any(InImage);
            end
        end
    end

end
