function [Lat,Npix] = latitudeRings(NSide)
    % Return a vector of the latitudes of healpix rings, and the number of pixels in each ring.
    % Input  : - NSide.
    % Output : - Vector of latirudes of rings.
    %          - Vector with the number of pixels in each ring.
    %            Note that the poles have 0 pixels.
    % Author : Eran Ofek (2024 Dec) 
    % Example: [Lat,Npix]=celestial.healpix.latitudeRings(16)

    
    % there are 4*NSide rings:
    Nring   = 4.*NSide;
    IndRing = (0:Nring-1).';  % r
    
    Zr = zeros(Nring+1,1);
    % Northern cup (0 to Nside-1)
    IR1          = (1:NSide).';
    Zr(IR1) = 1 - (((IR1-1)./NSide).^2) ./3;
    
    % equatorial region: NSide to 3*NSide-1
    IR2     = (NSide+1:3.*NSide).';
    Zr(IR2) = (Nring - 2.*(IR2-1))./Nring;
    
    % southern cup: 3*NSide to 4*NSide-1
    IR3                  = (3.*NSide+1:Nring).';
    Zr(IR3) = (((Nring - (IR3-1)) ./NSide).^2) ./3 - 1;
    
    % souther pole
    Zr(Nring+1) = -1;
    
    Lat = asin(Zr);
    
    
    if nargout>1
        Npix = zeros(Nring+1,1);
        Npix(IR1) = 4.*(IR1-1);
        Npix(IR2) = 4.*NSide;
        Npix(IR3) = 4.*(4.*NSide - (IR3-1));
        Npix(Nring+1) = 0;
    end
    
end
