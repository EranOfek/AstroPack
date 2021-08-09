function Result = astrometrySubImages(Obj, Args)
    % Solve astrometry for sub images of a single contigious image
    %       The solution is done by executing astrometryCore for a limited
    %       number of sub images, and astrometryRefine for all the rest,
    %       based on the solution from astrometryCore.
    % Input  : -
    % Output : -
    % Author : Eran Ofek (Aug 2021)
    % Example:
   
    arguments
        Obj                    % AstroImage array with catalogs
        Args.CCDSEC(:,4)
        Args.CenterXY          % [X,Y] pix Center of Full image, If empty, calculate from CCDSEC
        
        Args.CreateNewObj
    end
    
    
    N = numel(Obj);
    if N ~= size(Args.CCDSEC,1)
        error('Number of lines in CCDSEC must be equal to the number of elements in the input Obj');
    end
    
    % Center of sub images
    [SubCenterX, SubCenterY] = imUtil.ccdsec.center_ccdsec(Args.CCDSEC);
    if isempty(Args.CenterXY)
        % calculate CenterXY of full image from Args.CCDSEC
        Args.CenterXY = [0.5.*(min(Args.CCDSEC(:,1) + max(Args.CCDSEC(:,2))),...
                         0.5.*(min(Args.CCDSEC(:,3) + max(Args.CCDSEC(:,4)))];
    end
    % Distance of sub images from full image center
    DistSub = sqrt((SubCenterX - Args.CenterXY(1)).^2 + (SubCenterY - Args.CenterXY(2)).^2);
    % Sort SubImages by distance from image center (nominal position)
    [~,SI]  = sort(DistSub);
    
    for Iobj=1:1:Nobj
        % for each sub image
        
        % select sub image index, after sorting by distance of sub image
        % from full image
        Iim = SI(Iobj);
        
        
    
    
    end
    
end
    