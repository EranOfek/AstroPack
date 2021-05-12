function Flag = flag_ccdsec(SizeIJ, Section, InOut)
    % Flag the pixels within or outside a ccd section
    % Input  : - An image size [I J] for the full image.
    %          - CCDSEC [Xmin, Xmax, Ymin, Ymax]. The inside (including edge)
    %            or outside of this CCSEC will be flaged.
    %          - A logical indicating if to flag the inside (true) or
    %            outside (false) of the CCDSEC. Default is false.
    % Output : - A matrix of size like the full image size with true in/out
    %            the CCDSEC.
    % Author : Eran Ofek (May 2021)
    % Example: Flag = imUtil.ccdsec.flag_ccdsec([10 10], [2 8 3 9], false)
   
    arguments
        SizeIJ(1,2)
        Section(1,4)
        InOut(1,1) logical     = false;
    end
    
    Flag = false(SizeIJ);
    Npix = numel(Flag);
    DistX1 = Section(1) - 1;
    DistX2 = SizeIJ(2)  - Section(2);
    DistY1 = Section(3) - 1;
    DistY2 = SizeIJ(1)  - Section(4);
    
    Flag(1:SizeIJ(1).*DistX1) = true;  % the pixels on the left-hand side
    Flag(Npix - SizeIJ(1).*DistX2+1: Npix) = true;  % the pixels on the right-hand side
    for Id=1:1:DistY1
        Flag(Id:SizeIJ(1):Npix) = true; %SizeIJ(2).*Dist) = true;
    end
    for Id=SizeIJ(1):-1:SizeIJ(1)-DistY2+1
        Flag(Id:SizeIJ(1):Npix) = true; %SizeIJ(2).*Dist) = true;
    end
    
    
end