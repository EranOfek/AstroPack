function [Result] = buildRefImages(RefGrid, DB, Args)
    % given a grid of reference images, build them from proc images
    %     employs proc/coadd image DB 
    % Input  : - a grid of reference images (number, RA1-RA4, Dec1-Dec4) 
    %          - a DB object
    %          * ...,key,val,... 
    % Output : - reference image files written to disk and ref_images table filled in the DB
    % Author : A.M. Krassilchtchikov (2025 Jul) 
    % Example: load('~/LAST_RefIm_Grid_v2.mat'); D = db.Db; ...
    %          pipeline.last.reference.buildRefImages(LAST_RefIm_Grid,D);
    arguments
        RefGrid
        DB                
        Args.NsideSearch = 2^7; % we should start the search at a larger region 
        Args.NsideLow    = 2^8; 
        Args.SearchTable = 'visit_images'; % 'raw_images';
        % the list of table columns needed to check the overlaps + filtering + control 
        Args.Fields      = "id_visit, upix_low, jd_start, exptime, fieldid, mountnum, camnum, cropid," + ... 
                            "ra1, ra2, ra3, ra4, dec1, dec2, dec3, dec4, diryear, dirmon, dirday, subdir, filetime"; 
        Args.RefTable    = 'ref_images_v4';     
    end
    % 
    RAD = 180/pi;  
    Nref = height(RefGrid); 
    
    % convert the RA to [0, 360]:    % later change Yossi's grid itself to avoid this 
    RefGrid.RA  = RefGrid.RA + 180;
    RefGrid.RA1 = RefGrid.RA1 + 180;
    RefGrid.RA2 = RefGrid.RA2 + 180;
    RefGrid.RA3 = RefGrid.RA3 + 180;
    RefGrid.RA4 = RefGrid.RA4 + 180;
    
    % loop over the ref. image grid
    for Iref = 150000:Nref  % 1:Nref (no LAST obs in the South, so for the tests starting from around the equator)                      
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
        
        % 1. find the overlapping coadd proc or single-epoch proc images        
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

        for Im = 1:10
            for Ic = 1:4
                T1 = T(T.mountnum==Im & T.camnum==Ic,:);
                if height(T1) > 0
                    fprintf('M%dC%d:\n',Im,Ic);
                    [Grp, ~] = findgroups(T1.jd_start); 
                    Nepoch   = max(Grp);                 
                    S = repmat(AstroImage,Nepoch,1);
                    for i = 1:Nepoch
                        T2  = T1(Grp == i, :);
                        Nim = height(T2);
                        fprintf('epoch %d: %d images retrieved\n',i,Nim);
                        % 2. qualify the overlapping proc images
                        
                        % 3. select exposures by specific obs. time, time span, etc.
                        
                        % check the coverage
                        
                        % 4.1 retrieve the crop images and merge the set of covering crops
                        fprintf('epoch %d: %d images filtered\n',i,Nim);
                        Nim = height(T2);
                        AI  = repmat(AstroImage,Nim,1);
                        Mt  = compose('%02d',T2.mountnum(1)); Cam = compose('%02d',T2.camnum(1)); 
                        YY  = compose('%04d',T2.diryear(1)); MM = compose('%02d',T2.dirmon(1)); DD = compose('%02d',T2.dirday(1));
                        for Icrop = 1:Nim
                             FN = strcat('/mnt/euclid/last/data/LAST.01.',Mt,'.',Cam,'/',YY,'/',MM,'/',DD,...
                                 '/proc/',T2.subdir(Icrop),'/LAST.01.',Mt,'.',Cam,'_',YY,MM,DD,'.',T2.filetime(Icrop),...
                                 '_clear_',string(T2.fieldid(Icrop)),'_000_001_',compose('%03d',T2.cropid(Icrop)),...
                                 '_sci_coadd_Image_1.fits');                              
                             AI(Icrop)= AstroImage.readProducts(FN); % no data on Back or Var is saved @ euclid!  
                        end
                        
                        % merge
                        
                        % var1
                        [S(i), ~, RemappedXY]  = imProc.stack.stitch(AI,'WriteFile',false); % does not provide Back, Var, Mask
                        
                        % var2
%                         S = imProc.transIm.imwarp(AI(2), AI(1).WCS); %
%                         'BoundsStyle','SameAsInput' does not work
%
%                       % var3 
%                         MergedAI = imProc.transIm.merge(AI); % a new function to be written
%                           1. estimate the size of the merged image and
%                              enlarge the matrix, fill with 0s
%                           2. take the WCS0 form the 1st image
%                           3. use xy2sky with WCS1, then sky2xy with WCS0
%                           4. redistribute pixels (bilenear, like imProc.stack.addImageRedistributePixels)
%                           5. for each pixel of the merge take an exposure weighted mean of the merged pixel values
%                         
                        % 4.2 rotate, align, and cut the merged crops to the ref. coordinates
                        
                    end                                  
                    % 5. proper coadd the the aligned and merged crops
                    
                    % 6. save the new reference on disk and fill the DB table line
                end
            end % camera
        end % mount                 
    end % reference image grid      
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
