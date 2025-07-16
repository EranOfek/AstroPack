function [Result] = buildRef(RefGrid, DB, Args)
    % given a grid of reference images, build them from proc images
    %     employs proc/coadd image DB 
    % Input  : - a grid of reference images (number, RA1-RA4, Dec1-Dec4) 
    %          - a DB object
    %          * ...,key,val,... 
    % Output : - reference image files written to disk and ref_images table filled in the DB
    % Author : A.M. Krassilchtchikov (2025 Jul) 
    % Example: load('~/LAST_RefIm_Grid_v2.mat'); D = db.Db; ...
    %          pipeline.last.reference.buildRef(LAST_RefIm_Grid,D);
    arguments
        RefGrid
        DB                                       
        Args.NsideLow = 2^8; 
        Args.RefTable = 'ref_images_v4';     
    end
    % 
    RAD = 180/pi;
    % loop over the ref. image grid
    Nref = height(RefGrid);
    for Iref = 1:Nref
        
        % 0. build the ref polygon to be covered and find the healpix coverage
        
        P0 = [RefGrid.RA1(Iref), RefGrid.RA1(Iref); RefGrid.RA2(Iref), RefGrid.Dec2(Iref); ...
              RefGrid.RA3(Iref), RefGrid.Dec3(Iref); RefGrid.RA4(Iref), RefGrid.Dec4(Iref)];
        UpixCenter = celestial.healpix.ang2pix(Args.NsideLow, RefGrid.RA(Iref)/RAD, RefGrid.Dec(Iref)/RAD);
        UpixNeighb = celestial.healpix.neighbors(Args.NsideLow, UpixCenter); % find all the neighbours 
        
        % 1. find the overlapping single-epoch proc images 
        
        S = "select * from raw_images where";
        W = sprintf("upix_low = %s or upix_low = UpixNeighb",UpixCenter);
        T = DB.query(strcat(S,W));
        
        % 2. qualify the overlapping proc images
        
        % 3. select exposures by specific obs. time, time span, etc. 
        
        % 4. for each epoch:
        Nexp = size();
        for Iexp = 1:Nexp
            % 4.1 merge the set of covering crops 
            
            % 4.2 rotate, align, and cut the merged crops to the ref. coordinates
        end
        
        % 5. proper coadd the the aligned and merged crops
        
        % 6. save the new reference on disk and fill the DB table line 
    end    
end

% function ipix8 = neighbors(Nside, Ipix)
% 
%     [x, y, f] = celestial.healpix.ipix2xyf(Ipix, Nside);
%     
%     ipix8(1) = celestial.healpix.xyf2ipix(x+1, y+1, f, Nside);
%     ipix8(2) = celestial.healpix.xyf2ipix(x+1, y-1, f, Nside);
%     ipix8(3) = celestial.healpix.xyf2ipix(x-1, y+1, f, Nside);
%     ipix8(4) = celestial.healpix.xyf2ipix(x-1, y-1, f, Nside);
%     ipix8(5) = celestial.healpix.xyf2ipix(x, y-1, f, Nside);
%     ipix8(6) = celestial.healpix.xyf2ipix(x, y+1, f, Nside);
%     ipix8(7) = celestial.healpix.xyf2ipix(x+1, y, f, Nside);
%     ipix8(8) = celestial.healpix.xyf2ipix(x-1, y, f, Nside);
% end
% % 
% % function [x, y, f] = pix2xyf(ipix, nside)
% % Convert nested HEALPix pixel index to (x, y, face number) for NESTED scheme
% % Based on the official HEALPix algorithm
% 
% % Constants
% npface = nside * nside;
% pix = ipix;
% f = floor(pix / npface);
% p = mod(pix, npface);
% 
% % Decode p into (ix, iy) using bit interleaving (Morton order)
% x = 0;
% y = 0;
% for i = 0:log2(nside)-1
%     x = bitor(x, bitshift(bitget(p, 2*i+1), i));
%     y = bitor(y, bitshift(bitget(p, 2*i+2), i));
% end
% end
% 
% function ipix = xyf2pix(x, y, f, nside)
% % Convert (x, y, face number) back to HEALPix pixel index in NESTED scheme
% % Reverses the pix2xyf logic
% 
% % Interleave bits of x and y to form the position within the face
% p = 0;
% for i = 0:log2(nside)-1
%     p = bitor(p, bitshift(bitget(x, i+1), 2*i));
%     p = bitor(p, bitshift(bitget(y, i+1), 2*i + 1));
% end
% 
% ipix = f * nside^2 + p;
% end
% 
% 
