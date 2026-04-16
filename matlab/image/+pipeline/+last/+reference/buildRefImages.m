function [Result] = buildRefImages(RefGrid, DB, Args)
    % given a grid of reference images, build them from proc/coadd images
    %     employs proc/coadd image DB 
    % NB: the current version is rather LAST-specific (mounts, cameras,..)!
    %
    % Input  : - a grid of reference images: coordinates of image centers and corners (RA0, Dec0, RA1-RA4, Dec1-Dec4) 
    %          - a DB object (to retrieve original reduced images to be further stitched, fit, and stacked) 
    %
    %          * ...,key,val,... 
    %         'NsideSearch' -
    %         'NsideLow'    -
    %         'SearchTable' - name of the DB table containing image data
    %
    % Output : - reference image files (Image, Mask, PSF, Cat) written to disk and ref_images table filled in the DB
    % Author : A.M. Krassilchtchikov (2025 Jul) 
    % Example: load('LAST_refGrid.mat'); D = db.Db.connectLASTdb('Pass','*');
    %          pipeline.last.reference.buildRefImages(LAST_RefIm_Grid,D); % a most general usage  
    %          pipeline.last.reference.buildRefImages(LAST_RefIm_Grid,D,'RefNumbers',[99945 99946]); % a short test
    arguments
        RefGrid
        DB            
        
        Args.NsideSearch = 2^7; % 2^7; % we should start the search at a somewhat larger region then the ref. image size  
        Args.NsideLow    = 2^8; 
        Args.SearchTable = 'visit_images'; 
        % the list of table columns needed to check the overlaps + filtering + control 
        Args.Fields      = "id_visit, upix_low, jd_start, midjd, exptime, fieldid, mountnum, camnum, cropid," + ... 
                           "ra1, ra2, ra3, ra4, dec1, dec2, dec3, dec4, diryear, dirmon, dirday, subdir, filetime"; 
                       
        Args.RefNumbers  = []; % e.g., [120000 120001] or [120000:120020]; input range of ref. image numbers  
        
        Args.PrebuiltRefWCS    = [];    % use an array of pre-built WCS (e.g., from the RefGrid object) 
        Args.Naxis1            = 1716;  % the pixel size of a reference image   
        Args.Naxis2            = 1716;  % NOTE that it was reduced to 1716 from 1726, while the grid was built for 1726 x 1726    
               
        Args.RasterResolution   = 3;    % arcsec
        Args.MinAllowedCoverage = 0.999;  % 0.995; % allowed inaccuracy in the required reference field coverage  
                       
        Args.CoaddFunction     = @pipeline.generic.procCoadd; 
        Args.SubBack           = true;
        Args.StackMethod       = 'wrobust';
        Args.StackMethodArgs   = {'coadd_WRobustArgs',{'backVarArgs',{'Method',@imUtil.background.modeVar_Hist}}};        
        
        Args.PixScale           = 1.25;        
        
        Args.OutputDir          = '~/NewRef/';
        Args.WriteProp          = ["Image","Cat","Mask","PSF"];
        
        Args.OutputRefTable    = 'ref_images_v5'; % the output DB table name   
        Args.Verbosity         = 2; % from 0 (mute) to 2 (maximal)
    end
    % 
    RAD = 180/pi;  
    Nref = height(RefGrid); 
    
    % convert the RA to [0, 360]:    % TEMPORARY: change Yossi's grid itself to avoid this 
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
        
            tstart = tic;
            if Args.Verbosity > 0
                cprintf('blue','Starting to build a reference image for field %d at RA %.2f Dec %.2f \n',Iref,RefGrid.RA(Iref),RefGrid.Dec(Iref));
            end
            
        % read or build the WCS of the target reference image
        if ~isempty(Args.PrebuiltRefWCS)
            RefWCS = Args.PrebuiltRefWCS(Iref);
        else
            RefWCS = AstroWCS.buildSimpleWCS(RefGrid.RA(Iref),RefGrid.Dec(Iref),'Naxis1',Args.Naxis1,'Naxis2',Args.Naxis2,...
                'PixScale',Args.PixScale); 
            % NOTE: when the right values of ref. image PA are written to the RefGrid, change for this:
            %  RefWCS = AstroWCS.buildSimpleWCS(RefGrid.RA(Iref),RefGrid.Dec(Iref),'Naxis1',Args.Naxis1,'Naxis2',Args.Naxis2,...
            %                      'PA',RefGrid.PA(Iref),'PixScale',Args.PixScale);
        end        
        % create an empty reference AstroImage and attach the RefWCS to it
        AIref = AstroImage({zeros(Args.Naxis1,Args.Naxis2)}); 
        AIref.WCS = RefWCS; 
        AIref.HeaderData = AIref.WCS.wcs2header; 
        
        % 0. build the ref polygon to be covered and find the healpix coverage
        P0 = [RefGrid.RA1(Iref), RefGrid.Dec1(Iref); RefGrid.RA2(Iref), RefGrid.Dec2(Iref); ...
              RefGrid.RA3(Iref), RefGrid.Dec3(Iref); RefGrid.RA4(Iref), RefGrid.Dec4(Iref)];
        [Raster0, NsideRaster] = celestial.healpix.mex.rasterize_polygon(P0, Args.RasterResolution); 
        
        % find the center and neighbors at the search resolution Args.NsideSearch
        UpixCenter = celestial.healpix.ang2pix(Args.NsideSearch, RefGrid.RA(Iref)/RAD, RefGrid.Dec(Iref)/RAD);
        UpixNeighb = celestial.healpix.mex.neighbors_nested(Args.NsideSearch,UpixCenter); 
        
        % translate the center and the neighbors to Args.NsideLow (as in the image table of the DB)
        UpixCenterLow = celestial.healpix.increasePixelResolution(UpixCenter, Args.NsideSearch, Args.NsideLow);
        UpixNeighbLow = celestial.healpix.increasePixelResolution(UpixNeighb, Args.NsideSearch, Args.NsideLow);
        % convert to UNIQ:
        UpixCenterLow = celestial.healpix.pix2uniqueId(Args.NsideLow, UpixCenterLow);
        UpixNeighbLow = celestial.healpix.pix2uniqueId(Args.NsideLow, UpixNeighbLow);
        
        % 1. find the overlapping coadd proc or single-epoch proc images (determined by Args.SearchTable)
        Q = sprintf("select %s from %s",Args.Fields, Args.SearchTable);
        W = " where 1<0";
        for Icen=1:numel(UpixCenterLow)
            Wc = sprintf(" or toString(upix_low) = toString(%s)",string(UpixCenterLow(Icen)));
            W  = strcat(W,Wc);
        end
        for Inei=1:numel(UpixNeighbLow)
            Wn = sprintf(" or toString(upix_low) = toString(%s)",string(UpixNeighbLow(Inei)));
            W = strcat(W,Wn);
        end
        T = DB.query(strcat(Q,W)); % T = db.mex.query(strcat(S,W));
        
        if isempty(T)
            if Args.Verbosity > 0
                fprintf('No images found in the DB to build reference #%d at %.2f, %.2f \n',Iref, RefGrid.RA(Iref), RefGrid.Dec(Iref));
            end
        else
            if Args.Verbosity > 0
                fprintf('%d crop images found in the DB to build reference #%d at %.2f, %.2f \n',height(T), Iref, RefGrid.RA(Iref), RefGrid.Dec(Iref));
            end
            
            % identify sets of subimages from the same epoch and telescope to be stitched
            T = sortrows(T, {'mountnum','camnum','jd_start'});
            [Grp, ~] = findgroups(T.mountnum, T.camnum, T.jd_start);
            
            Nepoch   = max(Grp);
                    if Args.Verbosity > 0              
                        fprintf('%d groups of same epoch + telescope images found\n',Nepoch);
                    end
            %
            StackImages = [];
            
            for Iepoch = 1:Nepoch % loop by sets of epoch + telescope
                
                TabEpoch  = T(Grp == Iepoch, :);
                Imount    = unique(TabEpoch.mountnum); % unique is used to prevent multiple mounts in one set
                Icam      = unique(TabEpoch.camnum);   % unique is used to prevent multiple cameras in one set
                Nim       = height(TabEpoch);
                    if Args.Verbosity > 1
                        fprintf('M%dC%d epoch %d: %d crop images found in the DB \n',Imount,Icam,Iepoch,Nim);
                    end
                
                % 2. select exposures by specific obs. time, mount, telescope, time span, etc.
                %
                % if T2.jd_start ...
                %   fprintf('Epoch %d is skipped due to..\n', Iepoch);
                %   continue % to the next epoch
                % end
                
                % 3. select the overlapping proc images by some quality
                %
                % T2 = T2(quality condition,:)
                
                Nim = height(TabEpoch);
                    if Args.Verbosity > 1
                        fprintf('M%dC%d epoch %d: %d images selected according to the time and quality criteria \n',Imount,Icam,Iepoch,Nim);
                    end
                
                % if the total coverage is incomplete, skip to the next epoch
                Coverage = []; RasterC = []; Icrop = 1;
                while Icrop < height(TabEpoch)+1 % merge the rasters of all the crops involved
                    CropPoly = double([TabEpoch.ra1(Icrop), TabEpoch.dec1(Icrop); TabEpoch.ra2(Icrop), TabEpoch.dec2(Icrop); ...
                        TabEpoch.ra3(Icrop), TabEpoch.dec3(Icrop); TabEpoch.ra4(Icrop), TabEpoch.dec4(Icrop)]);
                    Raster = celestial.healpix.mex.rasterize_polygon(CropPoly, Args.RasterResolution);                         
                    % if this crop does not overlap with the reference region, deselect it
                    Coverage(Icrop) = sum(ismember(Raster,Raster0));
                    if Coverage(Icrop) < 1
                        TabEpoch(Icrop,:) = [];
                    else
                        RasterC  = [RasterC; Raster(~ismember(Raster,RasterC))];
                    end
                    Icrop = Icrop + 1;
                end
                
                Nim = height(TabEpoch);
                
                CoverageAll = sum(ismember(Raster0, RasterC))/numel(Raster0);
                if CoverageAll < Args.MinAllowedCoverage
                    if Args.Verbosity > 1
                        fprintf('Incomplete coverage of %.4f, epoch %d is skipped\n', CoverageAll, Iepoch);
                    end
                    continue % to the next epoch
                end
                
                % 4.1 retrieve the crop images
                    if Args.Verbosity > 0
                        fprintf('M%dC%d epoch %d: %d images filtered, dowloading and stitching...',Imount,Icam,Iepoch,Nim);
                    end
                
                AF = AstroFileName;
                AF.ProjName = {'LAST', 1, TabEpoch.mountnum, TabEpoch.camnum};
                AF.JD = double(TabEpoch.jd_start);
                AF.julday2time;
                AF.FieldID = TabEpoch.fieldid;
                AF.CropID  = TabEpoch.cropid;
                AF.Counter = 0;
                AF.Level   = "coadd";
                AF.CCDID   = 1;
                AF.SubDir  = TabEpoch.subdir;
                AF.BasePath                = '/mnt/euclid/last/data';
                AF.BasePathIncludeProjName = true;
                AF.AddSubDir               = true;
                
                AI = AstroImage.readProducts(AF.genFull);
                
                % check if WCS is present in all the selected crops
                if any(isnan(arrayfun(@(x) x.WCS.PhiP, AI)))
                    if Args.Verbosity > 0
                        cprintf('red','\nWCS is not correct in one or several crops, skipping the epoch %d\n',Iepoch);
                    end
                    continue
                end
                
                % 4.2 stitch the set of covering crops
                %                         telescope.obs.plotFOVfromQueryTable(TabEpoch,'Lines',L)
                StitchedImage = imProc.stack.stitchCrops(AI,'UpdateWCS',true,'UpdateZP',true);
                
                if isnan(julday(StitchedImage))
                    StitchedImage.HeaderData = replaceVal(StitchedImage.HeaderData, 'JD', TabEpoch.jd_start(Iepoch));
                end
                
                if Args.Verbosity > 0
                    fprintf(' done \n');
                end
                
                % add the images to the stack
                if exist('StackImages','var')
                    StackImages = [StackImages StitchedImage];
                else
                    StackImages = StitchedImage;
                end  
                
                clear AI
            end % epochs
            
            % 5. coadd the epochs from different telescopes and cameras
            %    rotate, align, and cut the merged crops to the ref. coordinates
            %    measure background, find sources, populate PSF
            if isempty(StackImages)
                if Args.Verbosity > 0
                    cprintf('err','No images have been qualified for the field %d, skipping to the next field..\n',Iref);
                end
                continue
            else
                if Args.Verbosity > 0
                    cprintf('blue','Coadding %d epochs \n',numel(StackImages));
                end
            end
            
            RefImage = Args.CoaddFunction(StackImages','WCS',AIref,'SubBack',Args.SubBack,...
                'StackMethod',Args.StackMethod, Args.StackMethodArgs{:});
                                       
            % 6. save the new reference image and its catalog, mask, and PSF to the disk
            for Iprop=1:numel(Args.WriteProp)
                FN = sprintf('%s/LAST_clear_%d_sci_ref_%s_1.fits',Args.OutputDir,Iref,Args.WriteProp(Iprop));
                RefImage.write1(FN, Args.WriteProp(Iprop), 'OverWrite', true, 'MkDir', true);
            end
            
            % 7. write the image metadata to the reference image table of the DB (use Args.OutputRefTable)
            %    write the reference image catalog to the reference image catalog table of the DB
            
        end % for the particular reference grid position we have some coadds to build on
        if Args.Verbosity > 0
            fprintf('Finished building a reference image for field %d: %d epochs stacked in %.1f s\n',...
                Iref, RefImage.HeaderData.Key.NCOADD,toc(tstart));
        end
    end % reference image grid
end 


