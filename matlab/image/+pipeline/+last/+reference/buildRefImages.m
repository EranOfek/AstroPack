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
        
        Args.UsePrebuiltRefWCS = false; % use pre-built WCS read with the reference image grid
        Args.Naxis1       = 1726;       % the pixel size of a reference image 
        Args.Naxis2       = 1726;
        
        Args.UseInterp2WCS  = true; % method to warp the image: either imProc.transIm.interp2wcs or imProc.transIm.imwarp
        Args.interp2wcsArgs = {};  
        
        Args.CoaddFunction  = @imProc.stack.coaddW; % a handle to coadder of registered images 
        
        Args.RasterResolution   = 10;     % arcsec
        Args.MinAllowedCoverage = 0.95; % 0.995; % allowed inaccuracy in the required reference field coverage  
    end
    % 
    RAD = 180/pi;  
    Nref = height(RefGrid); 
    
    % convert the RA to [0, 360]:    % later change Yossi's grid itself to avoid this 
    RefGrid.RA  = RefGrid.RA  + 180; 
    RefGrid.RA1 = RefGrid.RA1 + 180; RefGrid.RA2 = RefGrid.RA2 + 180;
    RefGrid.RA3 = RefGrid.RA3 + 180; RefGrid.RA4 = RefGrid.RA4 + 180;
    
    % loop over the Reference Image grid that has been read above 
    if isempty(Args.RefNumbers)
        RefNumbers = 1:Nref;
    else
        RefNumbers = Args.RefNumbers;
    end
    
    % the main loop over the reference grid 
    for Iref = RefNumbers                    
        % if the WCS of the target Reference Image has not been read from the RefGrid object, build it here 
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
        
        % 1. find the overlapping coadd proc or single-epoch proc images (determined by Args.SearchTable)         
        StitchedImage = sprintf("select %s from %s",Args.Fields, Args.SearchTable);
        W = " where 1<0";
        for Icen=1:numel(UpixCenterLow)
            Wc = sprintf(" or toString(upix_low) = toString(%s)",string(UpixCenterLow(Icen)));
            W  = strcat(W,Wc);
        end
        for Inei=1:numel(UpixNeighbLow)
            Wn = sprintf(" or toString(upix_low) = toString(%s)",string(UpixNeighbLow(Inei)));
            W = strcat(W,Wn);
        end      
        T = DB.query(strcat(StitchedImage,W)); % T = db.mex.query(strcat(S,W));

        if isempty(T)                       
            if Args.Verbose
                fprintf('No images to build reference #%d at %.2f, %.2f \n',Iref, RefGrid.RA(Iref), RefGrid.Dec(Iref));
            end
        else
        for Imount = 1:10      % loop on mounts
            for Icam = 1:4     % loop on cameras
                TabMountCam = T(T.mountnum==Imount & T.camnum==Icam,:);
                if height(TabMountCam) > 0                    
                    [Grp, ~] = findgroups(TabMountCam.jd_start); 
                    Nepoch   = max(Grp);        
                    fprintf('M%dC%d: %d epochs\n',Imount,Icam,Nepoch);                    
                    RegisteredImage = AstroImage([Nepoch 1]);                    
                    %
                    for Iepoch = 1:Nepoch
                        TabEpoch  = TabMountCam(Grp == Iepoch, :);
                        Nim = height(TabEpoch);
                        fprintf('M%dC%d epoch %d: %d images found\n',Imount,Icam,Iepoch,Nim);                                               
                        
                        % 2. select exposures by specific obs. time, time span, etc.
                        %
                        % if T2.jd_start ... 
                        %   fprintf('Epoch %d is skipped due to..\n', Iepoch);
                        %   continue % to the next epoch
                        % end
                        
                        % 3. select the overlapping proc images by some quality
                        %
                        % T2 = T2(quality condition,:)
                        Nim = height(TabEpoch);   
                        fprintf('M%dC%d epoch %d: %d images selected\n',Imount,Icam,Iepoch,Nim);
                        % if the total coverage is incomplete, skip to the next epoch                                                 
                        RasterC = [];
                        for Icrop = 1:Nim % merge the rasters of all the crops involved 
                            CropPoly = [TabEpoch.ra1(Icrop), TabEpoch.dec1(Icrop); TabEpoch.ra2(Icrop), TabEpoch.dec2(Icrop); ...
                                        TabEpoch.ra3(Icrop), TabEpoch.dec3(Icrop); TabEpoch.ra4(Icrop), TabEpoch.dec4(Icrop)];
                            Raster   = celestial.healpix.rasterize_polygon(CropPoly,'Resolution',Args.RasterResolution);
                            RasterC  = [RasterC; Raster(~ismember(Raster,RasterC))];
                        end
                        Coverage = sum(ismember(Raster0, RasterC))/numel(Raster0);
                        if Coverage < Args.MinAllowedCoverage   
                            fprintf('Incomplete coverage of %.2f, epoch %d is skipped\n', Coverage, Iepoch);
                            continue % to the next epoch
                        end
                        
                        % 4.1 retrieve the crop images 
                        fprintf('M%dC%d epoch %d: %d images filtered\n',Imount,Icam,Iepoch,Nim);
                        Nim = height(TabEpoch);                                               
                        AI = AstroImage([1 Nim]);
                        Mt  = compose('%02d',TabEpoch.mountnum(1)); Cam = compose('%02d',TabEpoch.camnum(1)); 
                        YY  = compose('%04d',TabEpoch.diryear(1)); MM = compose('%02d',TabEpoch.dirmon(1)); DD = compose('%02d',TabEpoch.dirday(1));
                        for Icrop = 1:Nim
                             FN = strcat('/mnt/euclid/last/data/LAST.01.',Mt,'.',Cam,'/',YY,'/',MM,'/',DD,...
                                 '/proc/',TabEpoch.subdir(Icrop),'/LAST.01.',Mt,'.',Cam,'_',YY,MM,DD,'.',TabEpoch.filetime(Icrop),...
                                 '_clear_',string(TabEpoch.fieldid(Icrop)),'_000_001_',compose('%03d',TabEpoch.cropid(Icrop)),...
                                 '_sci_coadd_Image_1.fits');                              
                             AI(Icrop)= AstroImage.readProducts(FN); 
                             % NB: no data on background or variance is kept in the archive (Euclid), need to re-measure    
                        end
                        
                        % check WCS
                        if any(isnan(arrayfun(@(x) x.WCS.PhiP, AI)))
                            fprintf('WCS not correct in one or several crops, skipping the epoch..\n');
                            continue
                        end
                        
                        % 4.2 merge the set of covering crops                         
                            % var1
                        [StitchedImage, ~, ~]  = imProc.stack.stitch(AI,'OutputUnits','cts', 'WCSfromFirstIm',true,...
                            'WriteFile',false,'Verbosity',1); 
                        
                        % issues: imProc.stack.stitch does not yet operate on Back, Var, and Mask                                                 
%                           % var2 
%                         MergedAI = imProc.transIm.merge(AI); % a new function to be written?
%                           1. estimate the size of the merged image, enlarge the matrix, fill with 0s
%                           2. take the WCS0 from the 1st image
%                           3. use xy2sky with WCS1, then sky2xy with WCS0
%                           4. redistribute pixels (bilenear, like imProc.stack.addImageRedistributePixels)
%                           5. for each pixel of the merge take an inverse variance weighted mean of the merged pixel values
%                                                                                                 
                        % 4.3 rotate, align, and cut the merged crops to 
                        % the ref. coordinates: warp with the reference grid WCS                                                  
                        if Args.UseInterp2WCS
                            RegisteredImage(Iepoch) = imProc.transIm.interp2wcs(StitchedImage, AIref,...
                                'CreateNewObj',true,...
                                Args.interp2wcsArgs{:});
                        else
                            RegisteredImage(Iepoch) = imProc.transIm.imwarp(StitchedImage, AIref,...
                                'TransWCS',true,...
                                'FillValues',0,...
                                'ReplaceNaN',true,...
                                'CreateNewObj',true);
                        end  
                        
                        % 4.4 add the RegisteredImage to the stack
                        if exist('StackImages','var')
                            StackImages = [StackImages RegisteredImage(Iepoch)];
                        else
                            StackImages = RegisteredImage(Iepoch);
                        end
                        % clear the intermediate objects
                        clear AI;
                        clear StitchedImage;
                    end % epochs of the same mount and camera                                                                       
                end
            end % camera
        end % mount
        
        % 5. coadd the epochs from different telescopes and cameras                   
        % employ imProc.stack.coaddW or a simliar function        
        
        RefImage = Args.CoaddFunction(StackImages);
        
        % 6. save the new reference image and its catalog to the disk (euclid?)
        
        % RefImage.write1....
                                        
        % 7. write the image metadata to the reference image table of the DB 
        %    write the reference image catalog to the reference image catalog table of the DB
                    
        end % for the particular reference grid position we have some coadds to build on  
    end % reference image grid      
end

%%%%%%%%%%%%%%%

function WCS = buildRefWCS(RA0, Dec0, Args) 
        % builds a WCS from position, size, and rotation angle
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
        % rotate the WCS if a PA is given:  
        if ~isempty(Args.PA) 
            RotMatrix = [cos(Args.PA), -sin(Args.PA);
                sin(Args.PA),  cos(Args.PA)];
            WCS.CD = RotMatrix * WCS.CD;
        end
end


