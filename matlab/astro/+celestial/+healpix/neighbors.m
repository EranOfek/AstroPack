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
        Args.IncludeSelf = false; % include the central pixel into the output array
    end
    %
    IpixNeighbors = zeros(8,1);
    
    [X, Y, F] = celestial.healpix.ipix2xyf(Ipix, Nside);
    
    IpixNeighbors(1) = celestial.healpix.xyf2ipix(X+1, Y, F, Nside);
    IpixNeighbors(2) = celestial.healpix.xyf2ipix(X, Y+1, F, Nside);
    IpixNeighbors(3) = celestial.healpix.xyf2ipix(X+1, Y+1, F, Nside);    
    
    % need a special section for RA < size(pixel)
    
    if X > 0 && Y > 0
        IpixNeighbors(4) = celestial.healpix.xyf2ipix(X, Y-1, F, Nside);
        IpixNeighbors(5) = celestial.healpix.xyf2ipix(X-1, Y, F, Nside);    
        IpixNeighbors(6) = celestial.healpix.xyf2ipix(X+1, Y-1, F, Nside);
        IpixNeighbors(7) = celestial.healpix.xyf2ipix(X-1, Y+1, F, Nside);
        IpixNeighbors(8) = celestial.healpix.xyf2ipix(X-1, Y-1, F, Nside);
    elseif X > 0 && Y == 0
        IpixNeighbors(4) = celestial.healpix.xyf2ipix(X-1, Y,   F, Nside);  
        IpixNeighbors(5) = celestial.healpix.xyf2ipix(X-1, Y+1, F, Nside);
        IpixNeighbors(6) = celestial.healpix.xyf2ipix(X-1, Y,   F+1, Nside);
        IpixNeighbors(7) = celestial.healpix.xyf2ipix(X-1, Y+1, F+1, Nside);
        IpixNeighbors(8) = celestial.healpix.xyf2ipix(X-1, Y+2, F+1, Nside);                    
    elseif X == 0 && Y > 0
        IpixNeighbors(4) = celestial.healpix.xyf2ipix(X+1, Y-1, F, Nside);     
        IpixNeighbors(5) = celestial.healpix.xyf2ipix(X,   Y-1, F, Nside);             
        IpixNeighbors(6) = celestial.healpix.xyf2ipix(X+1, Y-1, F-1, Nside);
        IpixNeighbors(7) = celestial.healpix.xyf2ipix(X+2, Y-1, F-1, Nside);         
        IpixNeighbors(8) = celestial.healpix.xyf2ipix(X,   Y-1, F-1, Nside);     
    else % X == 0 && Y == 0
        IpixNeighbors(4) = celestial.healpix.xyf2ipix(X,   Y,   F-1, Nside);
        IpixNeighbors(5) = celestial.healpix.xyf2ipix(X+1, Y,   F-1, Nside);
        IpixNeighbors(6) = celestial.healpix.xyf2ipix(X,   Y+1, F+1, Nside);        
        IpixNeighbors(7) = celestial.healpix.xyf2ipix(X,   Y,   F+1, Nside);                    
        IpixNeighbors(8) = celestial.healpix.xyf2ipix(X,   Y,   F+2, Nside);        
    end
    
    if Args.IncludeSelf
        IpixNeighbors(9) = Ipix; 
    end
end

% tests in astropy:
% print( hp.xyf2pix(128, 0, 1, 9, nest=True) )
% print(hp.get_all_neighbours(128,147458,None,nest=True))
% [131073 131076 147464 147465 147459 147457 147456 131072]
% print(hp.pix2xyf(128, 131073, nest=True))
% print(hp.pix2xyf(128, 131076, nest=True))
% ...
