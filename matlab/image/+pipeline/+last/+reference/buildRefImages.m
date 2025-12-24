function [Result] = buildRefImages(RefGrid, DB, Args)
    % given a grid of reference images, build them from proc images
    %     employs proc/coadd image DB 
    % Input  : - a grid of reference images (number, RA1-RA4, Dec1-Dec4) 
    %          - a DB object
    %          * ...,key,val,... 
    % Output : - reference image files written to disk and ref_images table filled in the DB
    % Author : A.M. Krassilchtchikov (2025 Jul) 
    % Example: load('LAST_refGrid.mat'); D = db.Db.connectLASTdb('Pass','*')
    %          pipeline.last.reference.buildRefImages(LAST_RefIm_Grid,D);
    %          pipeline.last.reference.buildRefImages(LAST_RefIm_Grid,D,'RefNumbers',[150000 150001]);
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
        Args.Verbose     = 'false';
        Args.RefNumbers  = []; % [150000 150001]; % []  % input ref. image numbers 
        
        Args.UsePrebuiltRefWCS = false; % use pre-built WCS read with the Reference Grid
        Args.Naxis1       = 1726;  % the size of the reference image 
        Args.Naxis2       = 1726;
        
        Args.UseInterp2  = true; % method to warp the image: either imProc.transIm.interp2wcs or imProc.transIm.imwarp
        Args.interp2wcsArgs = {};  
        
        Args.RasterResolution   = 10;     % arcsec
        Args.MinAllowedCoverage = 0.995;  % allowed inaccuracy in the required reference field coverage  
    end
    % 
    RAD = 180/pi;  
    Nref = height(RefGrid); 
    
    % convert the RA to [0, 360]:    % later change Yossi's grid itself to avoid this 
    RefGrid.RA  = RefGrid.RA  + 180; 
    RefGrid.RA1 = RefGrid.RA1 + 180; RefGrid.RA2 = RefGrid.RA2 + 180;
    RefGrid.RA3 = RefGrid.RA3 + 180; RefGrid.RA4 = RefGrid.RA4 + 180;
    
    % loop over the ref. image grid
    if isempty(Args.RefNumbers)
        RefNumbers = 1:Nref;
    else
        RefNumbers = Args.RefNumbers;
    end
    
    for Iref = RefNumbers                    
        % if the WCS of the target Reference Image has not been read from the RefGrid object  
        % at the very beginning, build it here 
        if Args.UsePrebuiltRefWCS && exist('PrebuiltRefWCS','var')
            RefWCS = PrebuiltRefWCS(Iref,'Npix1',Args.Naxis1,'Npix2',Args.Naxis2); 
        else
            RefWCS = buildRefWCS(RefGrid.RA(Iref),RefGrid.Dec(Iref),'PA',RefGrid.PA(Iref));
        end         
        % create an empty reference AstroImage and attach the RefWCS to it
        AIref = AstroImage({zeros(Args.Naxis1,Args.Naxis2)});
        AIref.WCS = RefWCS;
        
        % 0. build the ref polygon to be covered and find the healpix coverage        
        P0 = [RefGrid.RA1(Iref), RefGrid.Dec1(Iref); RefGrid.RA2(Iref), RefGrid.Dec2(Iref); ...
              RefGrid.RA3(Iref), RefGrid.Dec3(Iref); RefGrid.RA4(Iref), RefGrid.Dec4(Iref)];
        Raster0 = celestial.healpix.rasterize_polygon(P0,'Resolution',Args.RasterResolution);
        
        % find the center and neighbors at the search resolution Args.NsideSearch
        UpixCenter = celestial.healpix.ang2pix(Args.NsideSearch, RefGrid.RA(Iref)/RAD, RefGrid.Dec(Iref)/RAD);               
        UpixNeighb = celestial.healpix.neighbors(UpixCenter, Args.NsideSearch);  
%         % TEMPORARY (celestial.healpix.neighbors does not work well near the poles!):
%         if abs(RefGrid.Dec(Iref)) > 99. % 70. % ???
%             UpixNeighb = UpixCenter;
%         end
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

        if isempty(T)          
            if Args.Verbose
                fprintf('No images to build reference #%d at %.2f, %.2f \n',Iref, RefGrid.RA(Iref), RefGrid.Dec(Iref));
            end
        else
        for Im = 1:10      % loop on mounts
            for Ic = 1:4   % loop on cameras
                T1 = T(T.mountnum==Im & T.camnum==Ic,:);
                if height(T1) > 0                    
                    [Grp, ~] = findgroups(T1.jd_start); 
                    Nepoch   = max(Grp);        
                    fprintf('M%dC%d: %d epochs\n',Im,Ic,Nepoch);
                    S        = AstroImage([Nepoch 1]);
                    Saligned = AstroImage([Nepoch 1]);
                    for Iepoch = 1:Nepoch
                        T2  = T1(Grp == Iepoch, :);
                        Nim = height(T2);
                        fprintf('M%dC%d epoch %d: %d images found\n',Im,Ic,Iepoch,Nim);
                        % 2. qualify the overlapping proc images
                        
                        % 3. select exposures by specific obs. time, time span, etc.
                        
                        % if the total coverage is incomplete, skip to the next epoch                         
                        Nim = height(T2);     
                        RasterC = [];
                        for Icrop = 1:Nim % merge the rasters of all the crops involved 
                            CropPoly = [T2.ra1(Icrop), T2.dec1(Icrop); T2.ra2(Icrop), T2.dec2(Icrop); ...
                                        T2.ra3(Icrop), T2.dec3(Icrop); T2.ra4(Icrop), T2.dec4(Icrop)];
                            Raster   = celestial.healpix.rasterize_polygon(CropPoly,'Resolution',Args.RasterResolution);
                            RasterC  = [RasterC; Raster(~ismember(Raster,RasterC))];
                        end
                        Coverage = sum(ismember(Raster0, RasterC))/numel(Raster0);
                        if Coverage < Args.MinAllowedCoverage   
                            fprintf('Incomplete coverage of %.2f, epoch %d is skipped\n', Coverage, Iepoch);
                            continue % to the next epoch
                        end
                        
                        % 4.1 retrieve the crop images and merge the set of covering crops
                        fprintf('M%dC%d epoch %d: %d images filtered\n',Im,Ic,Iepoch,Nim);
                        Nim = height(T2);                                               
                        AI = AstroImage([1 Nim]);
                        Mt  = compose('%02d',T2.mountnum(1)); Cam = compose('%02d',T2.camnum(1)); 
                        YY  = compose('%04d',T2.diryear(1)); MM = compose('%02d',T2.dirmon(1)); DD = compose('%02d',T2.dirday(1));
                        for Icrop = 1:Nim
                             FN = strcat('/mnt/euclid/last/data/LAST.01.',Mt,'.',Cam,'/',YY,'/',MM,'/',DD,...
                                 '/proc/',T2.subdir(Icrop),'/LAST.01.',Mt,'.',Cam,'_',YY,MM,DD,'.',T2.filetime(Icrop),...
                                 '_clear_',string(T2.fieldid(Icrop)),'_000_001_',compose('%03d',T2.cropid(Icrop)),...
                                 '_sci_coadd_Image_1.fits');                              
                             AI(Icrop)= AstroImage.readProducts(FN); % no data on Back or Var is saved @ euclid!  
                        end
                        
                        % check WCS
                        if any(isnan(arrayfun(@(x) x.WCS.PhiP, AI)))
                            fprintf('WCS not correct in one or several crops, skipping the epoch..\n');
                            continue
                        end
                        
                        % merge                         
                            % var1
                        [S(Iepoch), ~, ~]  = imProc.stack.stitch(AI,'OutputUnits','cts', 'WCSfromFirstIm',true,...
                            'WriteFile',false,'Verbosity',1); 
                        % issues: imProc.stack.stitch does not provide Back, Var, Mask                                                 
%                           % var2 
%                         MergedAI = imProc.transIm.merge(AI); % a new function to be written
%                           1. estimate the size of the merged image and
%                              enlarge the matrix, fill with 0s
%                           2. take the WCS0 form the 1st image
%                           3. use xy2sky with WCS1, then sky2xy with WCS0
%                           4. redistribute pixels (bilenear, like imProc.stack.addImageRedistributePixels)
%                           5. for each pixel of the merge take an exposure weighted mean of the merged pixel values
%                                                                                                 
                        % 4.2.1 rotate, align, and cut the merged crops to
                        % the ref. coordinates: imwarp with the Reference Grid WCS                                                  
                        if Args.UseInterp2
                            Saligned(Iepoch) = imProc.transIm.interp2wcs(S(Iepoch), AIref,...
                                'CreateNewObj',true,...
                                Args.interp2wcsArgs{:});
                        else
                            Saligned(Iepoch) = imProc.transIm.imwarp(S(Iepoch), AIref,...
                                'TransWCS',true,...
                                'FillValues',0,...
                                'ReplaceNaN',true,...
                                'CreateNewObj',true);
                        end
                        
                        % 4.2.2 refine the astrometry (or leave it for the next step?) 
                    end                                  
                    % 5. coadd the the aligned and merged crops
                    % employ pipeline.generic.procMergeCoadd or some of its fragments?
                    clear AI;
                    % 6. save the new reference on disk and fill the DB table line
                end
            end % camera
        end % mount                 
        end % if the image table is not empty
    end % reference image grid      
end

%%%%%%%%%%%%%%%

function WCS = buildRefWCS(RA0, Dec0, Args) 
        arguments
            RA0      
            Dec0
            Args.PA       = [];   % rad
            Args.PixScale = 1.25; % arcsec
            Args.Naxis1   = 1726; % pix
            Args.Naxis2   = 1726; % pix 
        end
        %
        PixScale = Args.PixScale / 3600;    % [deg] pixel scale
        %
        WCS = AstroWCS();
        WCS.ProjType  = 'TAN';
        WCS.ProjClass = 'ZENITHAL';
        WCS.CooName   = {'RA'  'DEC'};
        WCS.CTYPE     = {'RA---TAN','DEC---TAN'};
        WCS.CUNIT     = {'deg', 'deg'};
        WCS.CD(1,1)   = PixScale;
        WCS.CD(2,2)   = PixScale;
        WCS.CRVAL(1)  = RA0;
        WCS.CRVAL(2)  = Dec0;
        WCS.CRPIX(1)  = Args.Naxis1/2;
        WCS.CRPIX(2)  = Args.Naxis2/2;
        WCS.AlphaP    = RA0;
        WCS.DeltaP    = Dec0;
        WCS.PhiP      = 180;
        
        % rotate the WCS if PA is put in:  
        if ~isempty(Args.PA) 
            RotMatrix = [cos(Args.PA), -sin(Args.PA);
                sin(Args.PA),  cos(Args.PA)];
            WCS.CD = RotMatrix * WCS.CD;
        end
end


