function IpixNeighbors = neighbors(Ipix, Nside, Args)
    % For a given HEALPix index and resolution find indices of all the neighbors
    %     (of the same resolution) for the NESTED scheme only 
    % Input  : - the healpix index (Ipix)
    %          - Nside healpix resolution
    %          * ...,key,val,... 
    %          'IncludeSelf' - include the central pixel into the output array
    % Output : - the array of neighboring Ipix  
    % Author : A.M. Krassilchtchikov (2025 Jul) 
    % Example: INeighb = celestial.healpix.neighbors(34578,256)
    % 
    arguments
        Ipix
        Nside
        Args.IncludeSelf = false;
    end
    %
    IpixNeighbors = zeros(8,1);
    
    [X, Y, F] = celestial.healpix.ipix2xyf(Ipix, Nside);
    
    IpixNeighbors(1) = celestial.healpix.xyf2ipix(X+1, Y+1, F, Nside);
    IpixNeighbors(2) = celestial.healpix.xyf2ipix(X+1, Y-1, F, Nside);
    IpixNeighbors(3) = celestial.healpix.xyf2ipix(X-1, Y+1, F, Nside);
    IpixNeighbors(4) = celestial.healpix.xyf2ipix(X-1, Y-1, F, Nside);
    IpixNeighbors(5) = celestial.healpix.xyf2ipix(X, Y-1, F, Nside);
    IpixNeighbors(6) = celestial.healpix.xyf2ipix(X, Y+1, F, Nside);
    IpixNeighbors(7) = celestial.healpix.xyf2ipix(X+1, Y, F, Nside);
    IpixNeighbors(8) = celestial.healpix.xyf2ipix(X-1, Y, F, Nside);
    
    if Args.IncludeSelf
        IpixNeighbors(9) = Ipix; 
    end
end
