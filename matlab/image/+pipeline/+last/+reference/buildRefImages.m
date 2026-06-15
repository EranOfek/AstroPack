function [Result] = buildRefImages(RefGrid, Args)
    % given a grid of reference images, build them from proc/coadd images
    %     employs proc/coadd image DB     
    %
    % Input  : - a grid of reference images: coordinates of image centers and corners (RA0, Dec0, RA1-RA4, Dec1-Dec4)
    %
    %          * ...,key,val,...
    %
    %         'RefID'                - a vector of reference image IDs to be built (def. empty = all)
    %         'PrebuiltRefWCS'       - if not empty, use an array of pre-built WCS, e.g., from the RefGrid object (def. empty)
    %         'Naxis1'               - the pixel size of a reference image, X axis (def. 1716)
    %         'Naxis2'               - the pixel size of a reference image, Y axis (def. 1716)
    %         'NsideSearch'          - the healpix Nside at which overlapping regions are searched; coarser than the DB table by one step (def. 2^7)
    %         'NsideLow'             - the healpix Nside of the image DB table (def. 2^8)
    %         'DB'                   - a DB object; auto-generated from the connection args below if not supplied (def. empty)
    %         'SearchTable'          - name of the DB table containing image data (def. 'last.visit_images')
    %         'Fields'               - comma-separated list of DB table columns to be retrieved for overlap checks, filtering, and control
    %         'GroupByFields'        - table fields used to group images that will be stitched separately, e.g., same epoch + telescope (def. {'mountnum','camnum','jd_start'})
    %         'BasePath'             - base path for retrieving the input crop images (def. '/mnt/euclid/last/data')
    %         'ImageQualityFilter'   - a user-supplied quality filter injected directly into the SQL query (def. "fwhm < 4")
    %         'RasterResolution'     - polygon rasterization step, in arcsec (def. 3)
    %         'MinAllowedCoverage'   - minimum fractional coverage of the reference field required to accept a group (def. 0.999)
    %         'CoaddFunction'        - function handle used to coadd the per-group stitched images (def. @pipeline.generic.procCoadd)
    %         'SubBack'              - subtract the background in the coaddition step (def. true)
    %         'StackMethod'          - stacking method passed to the coadd function (def. 'wrobust')
    %         'StackMethodArgs'      - extra arguments controlling the stacking method
    %         'CoaddFunctionArgs'    - extra arguments passed to the coadd function (def. {})
    %         'PixScale'             - pixel scale of the reference image, in arcsec/pix (def. 1.25)
    %         'Write2Disk'           - whether to write the products to disk (def. true)
    %         'OutputDir'            - directory where the reference image products are written (def. '~/NewRef/')
    %         'WriteProp'            - list of AstroImage properties to write to disk (def. ["Image","Cat","Mask","PSF"])
    %         'OutputRefTable'       - name of the DB table to be populated with reference-image metadata (def. 'ref_images_v5')
    %         'DbHost'               - DB server host address (def. '10.150.28.18')
    %         'DbPort'               - DB server port (def. 9000)
    %         'DbUser'               - DB user name (def. 'last_user')
    %         'DbName'               - DB name used to look up the password in the AstroPack passwords file (def. 'last_ro')
    %         'AstroDBPassFile'      - path to the AstroPack YAML passwords file (def. '~/.astropack/Passwords.yml')
    %         'Verbose'              - verbosity level: 0 (mute), 1, 2 (maximal) (def. 2)
    %
    % Output : - an AstroImage object for the last reference ID from the input list 
    %          - reference image files (Image, Mask, PSF, Cat) written to disk and ref_images table filled in the DB
    % Author : A.M. Krassilchtchikov (2026 Apr) 
    % Example: load('LAST_refGrid_new.mat'); 
    %          D = db.Db.connectLASTdb('Pass','*');
    %          pipeline.last.reference.buildRefImages(LAST_RefIm_Grid,'DB',D); % a most general usage  
    %          R=pipeline.last.reference.buildRefImages(LAST_RefIm_Grid,'DB',D,'RefID',[99945 99946]); % a short test
    arguments
        RefGrid
                                                        
        Args.RefID             = []; % e.g., [120000 120001] or [120000:120020]; input range of ref. image numbers  
        
        Args.PrebuiltRefWCS    = [];    % use an array of pre-built WCS (e.g., from the RefGrid object)
        Args.Naxis1            = 1716;  % the pixel size of a reference image   
        Args.Naxis2            = 1716;  % NOTE that it was reduced to 1716 from 1726, while the grid was built for 1726 x 1726    

        Args.NsideSearch       = 2^7; % 2^7; % we should start the search at a somewhat larger region then the ref. image size  
        Args.NsideLow          = 2^8; 
        
        Args.DB                = []; % a DB object (auto-generated, if not supplied)
        Args.SearchTable       = 'last.visit_images'; 
        % the list of table columns needed to check the overlaps + filtering + control 
        Args.Fields            = "id_visit, upix_low, jd_start, midjd, exptime, fieldid, nodenumb, mountnum, camnum, cropid," + ... 
                                 "ra1, ra2, ra3, ra4, dec1, dec2, dec3, dec4, diryear, dirmon, dirday, subdir, filetime"; 
        Args.GroupByFields     = {'mountnum','camnum','jd_start'} % fields employed for grouping images to be stitched separately
        
        Args.BasePath          = {'/mnt/euclid/last/data','/euclid/last/data'}; % base path for image retrieval  
        
        Args.ImageQualityFilter = "fwhm < 4"; % a user-supplied filter (to be included directly into the SQL query) 
                       
        Args.RasterResolution   = 3;    % arcsec
        Args.MinAllowedCoverage = 0.999;  % 0.995; % allowed inaccuracy in the required reference field coverage  
                       
        %Args.CoaddFunction      = @pipeline.generic.procCoadd; 
        %Args.backVarArgs        = {'Method',@imUtil.background.modeVar_Hist, 'Block',[128 128], 'MethodArgs',{{'Range',[-50 50]}}}
        %Args.backVarArgs        = {'Method',{@imUtil.background.modeVar_Hist, @imUtil.background.rvar} 'Block',[256 256], 'MethodArgs',{{'Range',[-20 20], 'ApplyCeil',false, 'NinBin',100}, {}} };
        Args.backVarArgs        = {'Method',{@imUtil.background.modeVar_Hist, @imUtil.background.rvar} 'Block',[128 128], 'MethodArgs',{{'Range',[-20 20], 'ApplyCeil',false, 'NinBin',50}, {}} };
        %Args.backVarArgs        = {'Method',@imUtil.background.modeVar_Hist, 'Block',[256 256], 'MethodArgs',{{'Range',[-20 20], 'ApplyCeil',false, 'NinBin',100}, {}} };
        Args.backVarIndivArgs   =  {'Method',@imUtil.background.modeVar_LogHist, 'Block',[], 'MethodArgs',{{},{}}};

        Args.Threshold          = [500 50 4];

        Args.SubBack            = true;  % don't change unless you understand what you are doing
        Args.StackMethod        = 'wrobust';
        Args.StackMethodArgs    = {}; %{'coadd_WRobustArgs',{'backVarArgs',{'Method',@imUtil.background.modeVar_Hist}}};     
        Args.CoaddFunctionArgs  = {}; % additional arguments to be passed to the coadd function 
        
        Args.PixScale           = 1.25;        
        
        Args.Write2Disk         = true;
        Args.OutputDir          = '~/NewRef/';        
        Args.WriteProp          = ["Image","Cat","Mask","PSF"];
        
        Args.OutputRefTable     = 'ref_images_v5'; % the output DB table name  
        
        Args.DbHost             = '10.150.28.18' 
        Args.DbPort             = 9000;
        Args.DbUser             = 'last_user'
        Args.DbName             = 'last_ro'
        Args.AstroDBPassFile    = '~/.astropack/Passwords.yml';
        
        Args.Verbose            = 0; % from 0 (mute) to 2 (maximal)
    end
    % 
    RAD = 180/pi;  

    % make a connection to the image DB
    if isempty(Args.DB)
        Configuration.getSingleton().loadFile(Args.AstroDBPassFile);
        PM = PasswordsManager;
        Db.Password = PM.search(Args.DbName).Pass;
        Args.DB = db.mex.ClickHouseClient(Args.DbHost, Args.DbPort, Args.DbUser, Db.Password);
    end

    Nref = height(RefGrid); 
           
    Ibp = find(isfolder(Args.BasePath), 1, 'first');
    Args.BasePath = Args.BasePath(Ibp);

    % loop over the Reference Image grid that has been read above 
    if isempty(Args.RefID)
        RefID = 1:Nref;
    else
        RefID = Args.RefID;
    end
    
    % the main loop over the reference grid 
    K = 0;
    for Iref = RefID
        K = K + 1;

        if Args.Verbose > 0
            tstart = tic;
            cprintf('blue','Starting to build a reference image for field %d of %d at RA %.2f Dec %.2f \n',Iref,Nref,RefGrid.RA(Iref),RefGrid.Dec(Iref));
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
        AIref = AstroImage({zeros(Args.Naxis1,Args.Naxis2,'single')}); 
        AIref.WCS = RefWCS; 
        AIref.WCS.Success = true;  % must have Success=true
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
        Q = sprintf("select %s from %s where",Args.Fields, Args.SearchTable);
        W = " 1<0";
        for Icen=1:numel(UpixCenterLow)
            Wc = sprintf(" or toString(upix_low) = toString(%s)",string(UpixCenterLow(Icen)));
            W  = strcat(W,Wc);
        end
        for Inei=1:numel(UpixNeighbLow)
            Wn = sprintf(" or toString(upix_low) = toString(%s)",string(UpixNeighbLow(Inei)));
            W = strcat(W,Wn);
        end
        
        % add image quality filter
        if ~isempty(Args.ImageQualityFilter)
            W = strcat("(",W,") and ",Args.ImageQualityFilter); 
        end
        
        % send the query and retrieve a table of image characteristics        
        T = Args.DB.query(strcat(Q,W)); 
        
        if isempty(T)
            if Args.Verbose > 0
                fprintf('No images found in the DB to build reference #%d at %.2f, %.2f \n',Iref, RefGrid.RA(Iref), RefGrid.Dec(Iref));
            end
        else
            if Args.Verbose > 0
                fprintf('%d images found in the DB to build reference #%d at %.2f, %.2f \n',height(T), Iref, RefGrid.RA(Iref), RefGrid.Dec(Iref));
            end
            
            % identify sets of subimages from the same epoch and telescope to be stitched
            T = sortrows(T, Args.GroupByFields);            
            GroupFields = T(:, Args.GroupByFields);
            [Grp, ~]    = findgroups(GroupFields);                       
            
            Ngroup   = max(Grp);
            if Args.Verbose > 0              
                fprintf('%d groups of images found\n',Ngroup);
            end
            %
            StackImages = [];
            
            for Igroup = 1:Ngroup % loop by sets of epoch + telescope
                
                TabGrp  = T(Grp == Igroup, :);               
                Nim     = height(TabGrp);

                if Args.Verbose > 1
                    fprintf('Group %d: %d images found in the DB \n',Igroup,Nim);
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
                
                Nim = height(TabGrp);
                if Args.Verbose > 1
                    fprintf('Group %d: %d images selected according to the time and quality criteria \n',Igroup,Nim);
                end
            
                % if the total coverage is incomplete, skip to the next epoch
                Coverage = []; RasterC = []; Icrop = 1;
                while Icrop < height(TabGrp)+1 % merge the rasters of all the crops involved
                    CropPoly = double([TabGrp.ra1(Icrop), TabGrp.dec1(Icrop); TabGrp.ra2(Icrop), TabGrp.dec2(Icrop); ...
                        TabGrp.ra3(Icrop), TabGrp.dec3(Icrop); TabGrp.ra4(Icrop), TabGrp.dec4(Icrop)]);
                    Raster = celestial.healpix.mex.rasterize_polygon(CropPoly, Args.RasterResolution);                         
                    % if this crop does not overlap with the reference region, deselect it
                    Coverage(Icrop) = sum(ismember(Raster,Raster0));
                    if Coverage(Icrop) < 1
                        TabGrp(Icrop,:) = [];
                    else
                        RasterC  = [RasterC; Raster(~ismember(Raster,RasterC))];
                    end
                    Icrop = Icrop + 1;
                end
                
                Nim = height(TabGrp);
                
                CoverageAll = sum(ismember(Raster0, RasterC))/numel(Raster0);
                if CoverageAll < Args.MinAllowedCoverage
                    if Args.Verbose > 1
                        fprintf('Incomplete coverage of %.4f, epoch %d is skipped\n', CoverageAll, Igroup);
                    end
                    continue % to the next epoch
                end
                
                % 4.1 retrieve the crop images
                if Args.Verbose > 0
                    fprintf('Group %d: %d images filtered, dowloading and stitching...',Igroup,Nim);
                end
            
                % Replace this block after verification 
                AF = AstroFileName;
                AF.ProjName = {'LAST', 1, TabGrp.mountnum, TabGrp.camnum};
                AF.JD = double(TabGrp.jd_start);
                AF.julday2time;
                AF.FieldID = TabGrp.fieldid;
                AF.CropID  = TabGrp.cropid;
                AF.Counter = 0;
                AF.Level   = "coadd";
                AF.CCDID   = 1;
                AF.SubDir  = TabGrp.subdir;
                AF.BasePath                = Args.BasePath;
                AF.BasePathIncludeProjName = true;
                AF.AddSubDir               = true;
                
                AI = AstroImage.readProducts(AF.genFull);
                
                % for:
%                 AI=pipeline.last.queryDB.loadProducts(TabGrp); % does not load anything ?                

                % check if WCS is present in all the selected crops
                if any(isnan(arrayfun(@(x) x.WCS.PhiP, AI)))
%                 if all(arrayfun(@(x) x.WCS.Success, AI))
                    if Args.Verbose > 0
                        cprintf('red','\nWCS is not correct in one or several crops, skipping the epoch %d\n',Igroup);
                    end
                    continue
                end
                
                % 4.2 stitch the set of covering crops
                %                         telescope.obs.plotFOVfromQueryTable(TabEpoch,'Lines',L)
                StitchedImage = imProc.stack.stitchCrops(AI,'UpdateWCS',true,'UpdateZP',true);
                
                if isnan(julday(StitchedImage))
                    StitchedImage.HeaderData = replaceVal(StitchedImage.HeaderData, 'JD', TabGrp.jd_start(Igroup));
                end
                
                if Args.Verbose > 0
                    fprintf(' done \n');
                end
                
                % add the images to the stack
                if exist('StackImages','var')
                    StackImages = [StackImages StitchedImage];
                else
                    StackImages = StitchedImage;
                end                                  
            end % groups (epochs + telescopes)
            
            % do the stacking 
            if isempty(StackImages)
                if Args.Verbose > 0
                    cprintf('err','No images have been qualified for the field %d, skipping to the next field..\n',Iref);
                end                
            else
                if Args.Verbose > 0
                    cprintf('blue','Coadding %d groups \n',numel(StackImages));
                end
                
                % 5. coadd the epochs from different groups of images (e.g., telescopes and cameras)
                %    rotate, align, and cut the merged crops to the ref. coordinates
                %    measure background, find sources, populate PSF
                RefImage = pipeline.generic.procCoadd(StackImages','WCS',AIref,...
                                    'SubBack',Args.SubBack,...
                                    'SetBackTo0',false,...
                                    'ReMeasureBack',true, 'ReMeasureVar',true,...
                                    'Threshold',Args.Threshold,...
                                    'AddBackNoise',true,...
                                    'BS_BackMaxR',1501,...
                                    'AddExtraBack',true,...
                                    'AddExtraVar',true,...
                                    'ZP','PH_ZP',...
                                    'NcoaddFactor',20,...
                                    'CleanSN',4,...
                                    'StackMethod',Args.StackMethod, Args.StackMethodArgs{:}, Args.CoaddFunctionArgs{:},...
                                    'backVarArgs',Args.backVarArgs,...
                                    'backVarIndivArgs',Args.backVarIndivArgs,...
                                    'AddMaskSrcNoise',false);
                
                % 5a. add the ID_REF keyword
                RefImage.HeaderData = replaceVal(RefImage.HeaderData, 'MOUNTNUM', 0);
                RefImage.HeaderData = replaceVal(RefImage.HeaderData, 'CAMNUM', 0);
                JD = RefImage.getStructKey('MIDJD').MIDJD;
                [RefImage,~] = imProc.db.generateImageID(RefImage,'KeyID','ID_REF','JD',JD);
                
                % 6. save the new reference image and its catalog, mask, and PSF to the disk
                if Args.Write2Disk
                    for Iprop=1:numel(Args.WriteProp)
                        FN = sprintf('%s/LAST_clear_%d_sci_ref_%s_1.fits',Args.OutputDir,Iref,Args.WriteProp(Iprop));
                        RefImage.write1(FN, Args.WriteProp(Iprop), 'OverWrite', true, 'MkDir', true);
                    end
                end
                
                % 7. write the image metadata to the reference image table of the DB (use Args.OutputRefTable)
                %    write the reference image catalog to the reference image catalog table of the DB
                
            
                if Args.Verbose > 0
                    fprintf('Finished building a reference image for field %d: %d epochs stacked in %.1f s\n',...
                        Iref, RefImage.HeaderData.Key.NCOADD,toc(tstart));
                end
            end % if isempty(StackImages)
        end % for the particular reference grid position we have some coadds to build on        
        Result(K) = RefImage;
    end % for Iref = RefID / reference image grid
    
end 


