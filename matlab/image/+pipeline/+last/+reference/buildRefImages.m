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
    % Output : - reference image files written to disk and ref_images table filled in the DB
    % Author : A.M. Krassilchtchikov (2025 Jul) 
    % Example: load('LAST_refGrid.mat'); D = db.Db.connectLASTdb('Pass','*');
    %          pipeline.last.reference.buildRefImages(LAST_RefIm_Grid,D); % a most general usage  
    %          pipeline.last.reference.buildRefImages(LAST_RefIm_Grid,D,'RefNumbers',[150000 150001]); % a short test
    arguments
        RefGrid
        DB            
        
        Args.NsideSearch = 2^7; % we should start the search at a somewhat larger region then the ref. image size  
        Args.NsideLow    = 2^8; 
        Args.SearchTable = 'visit_images'; % 'proc_images'
        % the list of table columns needed to check the overlaps + filtering + control 
        Args.Fields      = "id_visit, upix_low, jd_start, exptime, fieldid, mountnum, camnum, cropid," + ... 
                           "ra1, ra2, ra3, ra4, dec1, dec2, dec3, dec4, diryear, dirmon, dirday, subdir, filetime"; 
                       
        Args.RefNumbers  = []; % [150000 150001]; % []  % input ref. image numbers 
        
        Args.UsePrebuiltRefWCS = false; % use pre-built WCS read with the reference image grid
        Args.Naxis1            = 1726;  % the pixel size of a reference image:  
        Args.Naxis2            = 1726;  % note: will like be reduced ro 1716 for the new LAST pipeline   
        
        Args.UseInterp2WCS     = true; % the method to warp the image: either imProc.transIm.interp2wcs or imProc.transIm.imwarp
        Args.interp2wcsArgs    = {'Sampling',5,'CreateNewObj',true};  
        
        Args.RasterResolution   = 10;     % arcsec
        Args.MinAllowedCoverage = 0.95; % 0.995; % allowed inaccuracy in the required reference field coverage  
        
        Args.StitchPars         = {'Crop',[10 10 10 10],'SizeMargin',[100 100],'Verbosity',1}; % parameters passed to the stitch function
        
        Args.BackSubSizeXY      = [128 128];
        Args.Threshold          = 5;
        Args.MomRadius          = 6;
        Args.PsfFunPar cell     = {[0.1;1.0;1.5]};  % search for sources  
        Args.ZP                 = 25;
        Args.ColCell cell       = {'XPEAK','YPEAK',...
                                    'X1', 'Y1',...
                                    'X2','Y2','XY',...
                                    'SN','BACK_IM','VAR_IM',...
                                    'BACK_ANNULUS', 'STD_ANNULUS', ...
                                    'FLUX_APER', 'FLUXERR_APER',...
                                    'MAG_APER', 'MAGERR_APER'};
        
        Args.CoaddFunction  = @imProc.stack.coaddW; % a handle to coadder of registered images 
        
        Args.PixScale           = 1.25;
        Args.Tran               = Tran2D('poly3');
        Args.CatName            = 'GAIAEDR3';
        
        Args.OutputDir          = '~/NewRef/';
        Args.WriteProp          = ["Image","Cat","Mask","PSF"];
        
        Args.OutputRefTable    = 'ref_images_v5'; % the output DB table name   
        Args.Verbosity         = 1; % from 0 to 2 
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
                if Args.Verbosity > 0
                    fprintf('Starting to build a reference image for field %d at RA %.2f Dec %.2f \n',Iref,RefGrid.RA(Iref),RefGrid.Dec(Iref));
                end
        % if the WCS of the target Reference Image has not been read from the RefGrid object, build it here 
        if Args.UsePrebuiltRefWCS && exist('PrebuiltRefWCS','var')
            RefWCS = PrebuiltRefWCS(Iref); 
        else
            % NOTE: when the right values of ref. image PA are written to the RefGrid, use this line: 
%             RefWCS = AstroWCS.buildSimpleWCS(RefGrid.RA(Iref),RefGrid.Dec(Iref),'Naxis1',Args.Naxis1,'Naxis2',Args.Naxis2,...
%                      'PA',RefGrid.PA(Iref),'PixScale',Args.PixScale);
            RefWCS = AstroWCS.buildSimpleWCS(RefGrid.RA(Iref),RefGrid.Dec(Iref),'Naxis1',Args.Naxis1,'Naxis2',Args.Naxis2,...
                     'PixScale',Args.PixScale); % temporary! 
        end         
        % create an empty reference AstroImage and attach the RefWCS to it
        AIref = AstroImage({zeros(Args.Naxis1,Args.Naxis2)});
        AIref.WCS = RefWCS;
        AIref.HeaderData = AIref.WCS.wcs2header;
        
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
        % translate the center and the neighbors to Args.NsideLow (as in the image table of the DB)                 
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
            if Args.Verbosity > 0
                fprintf('No images ar found in the DB to build reference #%d at %.2f, %.2f \n',Iref, RefGrid.RA(Iref), RefGrid.Dec(Iref));
            end
        else
        for Imount = 1:10      % loop on LAST mounts
            for Icam = 1:4     % loop on LAST cameras
                TabMountCam = T(T.mountnum==Imount & T.camnum==Icam,:);
                if height(TabMountCam) > 0                    
                    [Grp, ~] = findgroups(TabMountCam.jd_start); 
                    Nepoch   = max(Grp);      
                    if Args.Verbosity > 0
                        fprintf('M%dC%d: %d epochs\n',Imount,Icam,Nepoch);
                    end
                    %
                    for Iepoch = 1:Nepoch
                        TabEpoch  = TabMountCam(Grp == Iepoch, :);
                        Nim = height(TabEpoch);
                        if Args.Verbosity > 1
                            fprintf('M%dC%d epoch %d: %d images found\n',Imount,Icam,Iepoch,Nim);
                        end
                        
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
                        if Args.Verbosity > 1
                            fprintf('M%dC%d epoch %d: %d images selected\n',Imount,Icam,Iepoch,Nim);
                        end
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
                            if Args.Verbosity > 1
                                fprintf('Incomplete coverage of %.2f, epoch %d is skipped\n', Coverage, Iepoch);
                            end
                            continue % to the next epoch
                        end
                        
                        %%% DEBUG: Nim = 6 causes errors in imProc.stack.stitchCrops  
                        if Nim > 4
                            continue % to the next epoch
                        end
                        
                        % 4.1 retrieve the crop images 
                        if Args.Verbosity > 1
                            fprintf('M%dC%d epoch %d: %d images filtered\n',Imount,Icam,Iepoch,Nim);
                        end
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
                             AI(Icrop).CatData.JD = AI(Icrop).julday;
                             % NB: no data on background or variance is kept in the archive (Euclid), need to re-measure    
                        end
                        
                        % check WCS
                        if any(isnan(arrayfun(@(x) x.WCS.PhiP, AI)))
                            if Args.Verbosity > 1
                                fprintf('WCS not correct in one or several crops, skipping the epoch %d\n',Iepoch);
                            end
                            continue
                        end
                        
                        % 4.2 merge the set of covering crops                                                     
                            %                         telescope.obs.plotFOVfromQueryTable(TabEpoch,'Lines',L)
                            try % 
                                StitchedImage = imProc.stack.stitchCrops(AI,'UpdateWCS',true,'UpdateZP',true);
                            catch ME
                                fprintf('%s\n',ME.message);
                                 if Args.Verbosity > 1
                                     cprintf('err','However stitching of epoch %d failed, we are going on with other epochs\n',Iepoch);
                                 end
                                continue
                            end
                            
                            if isnan(julday(StitchedImage))
                                StitchedImage.HeaderData = replaceVal(StitchedImage.HeaderData, 'JD', TabEpoch.jd_start(Iepoch));
                            end
                                                                                                 
                        % 4.3 rotate, align, and cut the merged crops to the ref. coordinates: 
                        % warp with the reference grid WCS                                                  
                        if Args.UseInterp2WCS
                            RegisteredImage = imProc.transIm.interp2wcs(StitchedImage, AIref, Args.interp2wcsArgs{:});
                        else
                            RegisteredImage = imProc.transIm.imwarp(StitchedImage, AIref,...
                                'TransWCS',true,...
                                'FillValues',0,...
                                'ReplaceNaN',true,...
                                'Sampling',1,...
                                'InterpMethod','linear',...
                                'CreateNewObj',true);
                        end  
                        
                        % 4.4 measure background, find sources, populate PSF
                        RegisteredImage = imProc.background.background(RegisteredImage, 'SubSizeXY',Args.BackSubSizeXY);
                        
                        RegisteredImage = imProc.sources.findMeasureSources(RegisteredImage, ...
                                                       'Threshold', Args.Threshold,...
                                                       'ReCalcBack',false,...
                                                       'MomPar',{'MomRadius',Args.MomRadius},...
                                                       'ColCell',Args.ColCell,...
                                                       'FlagCR',true,...
                                                       'ZP',Args.ZP,...
                                                       'CreateNewObj',false);                                            
                                                                           
                        % re-measure PSF [do PSF photometry -- do we really need it here?]
                        RegisteredImage = imProc.psf.populatePSF(RegisteredImage, 'RePopulatePSF', true, 'DataType',@single);
%                         RegisteredImage = imProc.sources.psfFitPhot(RegisteredImage, 'CreateNewObj',false, 'ZP',Args.ZP);    

                        % 4.5 add the RegisteredImage to the stack
                        if exist('StackImages','var')
                            StackImages = [StackImages RegisteredImage];
                        else
                            StackImages = RegisteredImage;
                        end
                        % clear the intermediate objects
                        clear AI;
                        clear StitchedImage;
                        clear RegisteredImage;
                    end % epochs of the same mount and camera                                                                       
                end
            end % camera
        end % mount
        
        % 5. coadd the epochs from different telescopes and cameras                   
        RefImage = Args.CoaddFunction(StackImages,'SubBack',false,'FluxMatch','PH_ZP'); 
        
        % measure the background, find and measure sources, measure the PSF
        RefImage = imProc.background.background(RefImage, 'SubSizeXY',Args.BackSubSizeXY);
                        
        RefImage = imProc.sources.findMeasureSources(RefImage, ...
                                       'Threshold', Args.Threshold,...
                                       'ReCalcBack',false,...
                                       'MomPar',{'MomRadius',Args.MomRadius},...
                                       'ColCell',Args.ColCell,...
                                       'FlagCR',true,...
                                       'ZP',Args.ZP,...
                                       'CreateNewObj',false);                                                                                                      
                        
        % re-measure PSF, do PSF photometry 
        RefImage = imProc.psf.populatePSF(RefImage, 'RePopulatePSF', true, 'DataType',@single);
        RefImage = imProc.sources.psfFitPhot(RefImage, 'CreateNewObj',false, 'ZP', Args.ZP); 
                
        MeanJD = mean(julday(StackImages));        
        RefImage.HeaderData = replaceVal(RefImage.HeaderData, 'MIDJD', MeanJD);
%                            
%         [~, RefImage, ~] = imProc.astrometry.astrometryRefine(RefImage,...                                            
%                                             'EpochOut',MeanJD,...
%                                             'Scale',Args.PixScale,...
%                                             'CatName',Args.CatName,...
%                                             'Tran',Args.Tran,...
%                                             'CreateNewObj',false);                    
%                                         
        [~, RefImage, ~] = imProc.astrometry.astrometryRefine(RefImage);
%         RefImage = imProc.calib.photometricZP(RefImage);
        
%         RefImage.Back = imProc.stat.median(RefImage).*ones(size(RefImage.Image));
%         RefImage.Var = imProc.stat.rstd(RefImage).^2 .*ones(size(RefImage.Image));
%         RefImage = imProc.sources.findMeasureSources(RefImage,'AddFlags',true);
%         RefImage = imProc.psf.populatePSF(RefImage);
%         imProc.psf.fwhm(RefImage);
%         RefImage = imProc.sources.psfFitPhot(RefImage);
%         RefImage = imProc.astrometry.addCoordinates2catalog(RefImage,'OutUnits','deg');
%         RefImage = imProc.calib.photometricZP(RefImage);
%         RefImage = imProc.match.match_catsHTMmerged(RefImage, 'SameField',false, 'CreateNewObj',false);
        
        RefImage.UserData.Nimages = numel(StackImages);        

        % 6. save the new reference image and its catalog, mask, and PSF to the disk                 
        for Iprop=1:numel(Args.WriteProp)
            FN = sprintf('%s/LAST_clear_%d_sci_ref_%s_1.fits',Args.OutputDir,Iref,Args.WriteProp(Iprop));
            RefImage.write1(FN, Args.WriteProp(Iprop), 'OverWrite', true, 'MkDir', true);
        end
                                        
        % 7. write the image metadata to the reference image table of the DB (use Args.OutputRefTable)  
        %    write the reference image catalog to the reference image catalog table of the DB
                    
        end % for the particular reference grid position we have some coadds to build on  
        if Args.Verbosity > 0
            fprintf('Finished building a reference image for field %d: %d epochs stacked\n',...
                Iref, RefImage.UserData.Nimages);
        end
    end % reference image grid      
end 


