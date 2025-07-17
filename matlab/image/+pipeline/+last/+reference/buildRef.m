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
        Args.NsideSearch = 2^8; % we could start the search at a larger region, e.g, 2^7 
        Args.NsideLow    = 2^8; 
        Args.SearchTable = 'visit_images'; % 'raw_images';
        % the list of table columns needed to check the overlaps + filtering + control 
        Args.Fields      = "id_visit, upix_low, jd_start, exptime, fieldid, mountnum, camnum, cropid," + ... 
                            "ra1, ra2, ra3, ra4, dec1, dec2, dec3, dec4"; 
        Args.RefTable    = 'ref_images_v4';     
    end
    % 
    RAD = 180/pi;  
    Nref = height(RefGrid);
    
    % convert the RA to [0, 360]:    % later change the grid itself
    RefGrid.RA  = RefGrid.RA + 180;
    RefGrid.RA1 = RefGrid.RA1 + 180;
    RefGrid.RA2 = RefGrid.RA2 + 180;
    RefGrid.RA3 = RefGrid.RA3 + 180;
    RefGrid.RA4 = RefGrid.RA4 + 180;
    
    % loop over the ref. image grid
    for Iref = 120000:Nref  % 1:Nref (no LAST obs in the South, so for the tests starting from around the equator)                      
        % 0. build the ref polygon to be covered and find the healpix coverage
        
        P0 = [RefGrid.RA1(Iref), RefGrid.Dec1(Iref); RefGrid.RA2(Iref), RefGrid.Dec2(Iref); ...
              RefGrid.RA3(Iref), RefGrid.Dec3(Iref); RefGrid.RA4(Iref), RefGrid.Dec4(Iref)];
        % find the center and neighbors at the search resolution Args.NsideSearch
        UpixCenter = celestial.healpix.ang2pix(Args.NsideSearch, RefGrid.RA(Iref)/RAD, RefGrid.Dec(Iref)/RAD);               
        UpixNeighb = celestial.healpix.neighbors(UpixCenter, Args.NsideSearch);  
        % translate the center and the neighbors to Args.NsideLow (as in the DB)                
        UpixCenterLow = celestial.healpix.increasePixelResolution(UpixCenter, Args.NsideSearch, Args.NsideLow); 
        UpixNeighbLow = celestial.healpix.increasePixelResolution(UpixNeighb, Args.NsideSearch, Args.NsideLow); 
        % convert to UNIQ:    
        UpixCenterLow = celestial.healpix.pix2uniqueId(Args.NsideLow, UpixCenterLow);
        UpixNeighbLow = celestial.healpix.pix2uniqueId(Args.NsideLow, UpixNeighbLow);
        
        % 1. find the overlapping single-epoch proc images 
        
        S = sprintf("select %s from %s",Args.Fields, Args.SearchTable);
        W = " where 1<0";
        for Icen=1:numel(UpixCenterLow)
            Wc = sprintf(" or toString(upix_low) = toString(%s)",string(UpixCenterLow(Icen)));
            W  = strcat(W,Wc);
        end
        for Inei=1:numel(UpixNeighbLow)
            Wn = sprintf(" or toString(upix_low) = toString(%s)",string(UpixNeighbLow(Inei)));
            W = strcat(W,Wn);
        end      
        T = DB.query(strcat(S,W)); % T = db.mex.query(strcat(S,W));
        
        % Assume T is your input table
%         [G, ~] = findgroups(T.mountnum, T.camnum, T.jd_start); % same camera or just same mount?
        [G, ~] = findgroups(T.mountnum, T.jd_start); % same camera or just same mount?
        
        % Preallocate cell array of tables
        subtables = cell(max(G), 1);
        
        % Loop over each group
        for i = 1:max(G)
            subtables{i} = T(G == i, :);
        end
        
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

% function ipix_list = upscale_nested_pixel(ipix0, Nside0, Nside1)
%     % Check that Nside1 is a multiple of Nside0
%     assert(mod(Nside1, Nside0) == 0, 'Nside1 must be a multiple of Nside0');
%     
%     ratio = Nside1 / Nside0;
%     npix_per_coarse = ratio^2;
% 
%     ipix_list = [];
%     % First fine pixel in the block
%     for i=1:numel(ipix0)
%         first = ipix0(i) * npix_per_coarse;
%         last = (ipix0(i) + 1) * npix_per_coarse - 1; 
%         ipix_list = [ipix_list; (first : last)']; 
%     end    
% end

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
