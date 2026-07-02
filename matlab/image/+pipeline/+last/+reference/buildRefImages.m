function [Result,Info] = buildRefImages(RefID, Args)
    % given a grid of reference images, build them from proc/coadd images
    %     employs proc/coadd image DB     
    %
    % Input  : - Reference ID. Default is 146446 (M51 field).
    %            If empty, then create ID of all images.
    %          * ...,key,val,...
    %            'RefTable' - A table with gid of reference images: coordinates of image
    %                   centers and corners (RA0, Dec0, RA1-RA4, Dec1-Dec4).
    %                   If empty, then load file in 'RefTableName' arg.
    %            'RefTableName'    - File containing the Reference IDs.
    %                   Default is 'LAST_RefIm_Grid.mat'.
    %         'RefWCS'               - if not empty, use an array of pre-built WCS, e.g., from the RefGrid object (def. empty)
    %         'RA'                   - [deg] optional array of sky point RA for building ad hoc, North-oriented
    %                   reference image(s), instead of using the RefID/RefTable grid (def. empty)
    %         'Dec'                  - [deg] array of sky point Dec, matching 'RA' element-by-element (def. empty)
    %         'RefName'              - optional string/cellstr array, matching 'RA'/'Dec' element-by-element,
    %                   used as the output filename tag for ad hoc sky points instead of the grid index.
    %                   Default (if 'RA' is given but 'RefName' is not) is a sequential "1","2",... (def. empty)
    %         'Naxis1'               - the pixel size of a reference image, X axis (def. 1716)
    %         'Naxis2'               - the pixel size of a reference image, Y axis (def. 1716)
    %         'NsideSearch'          - the healpix Nside at which overlapping regions are searched; coarser than the DB table by one step (def. 2^7)
    %         'NsideLow'             - the healpix Nside of the image DB table (def. 2^8)
    %         'DB'                   - a DB object; auto-generated from the connection args below if not supplied (def. empty)
    %         'SearchTable'          - name of the DB table containing image data (def. 'last.visit_images')
    %         'Fields'               - comma-separated list of DB table columns to be retrieved for overlap checks, filtering, and control
    %         'GroupByFields'        - table fields used to group images that will be stitched separately, e.g., same epoch + telescope (def. {'mountnum','camnum','jd_start'})
    %         'BasePath'             - base path for retrieving the input crop images (def. '/mnt/euclid/last/data')
    %         'QueryFilter'          - a user-supplied quality filter injected directly into the SQL query (def. "fwhm < 4")
    %         'RasterResolution'     - polygon rasterization step, in arcsec (def. 3)
    %         'MinCoverage'          - minimum fractional coverage of the reference field required to accept a group (def. 0.999)
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
    %         'AstrometricCatRad'    - cone radius [deg] for pre-fetching astrometric/photometric
    %                    reference catalogs once per field (def. 1)
    %         'AstrometricCatMagRange' - [min max] magnitude range for the astrometric catalog (def. [12 19.5])
    %         'AstrometricCatPlxRange' - [min max] parallax range [mas] for the astrometric catalog (def. [-Inf 50])
    %         'PhotCatMagRange'       - [min max] magnitude range for the photometric catalog (def. [13 21.5])
    %         'PhotCatPlxRange'       - [min max] parallax range [mas] for the photometric catalog (def. [0.1 100])
    %
    % Output : - an AstroImage object for the last reference ID from the input list
    %          - reference image files (Image, Mask, PSF, Cat) written to disk and ref_images table filled in the DB
    % Author : A.M. Krassilchtchikov (2026 Apr) 
    % Example: load('LAST_refGrid_new.mat'); 
    %          D = db.Db.connectLASTdb('Pass','*');
    %          pipeline.last.reference.buildRefImages(LAST_RefIm_Grid,'DB',D); % a most general usage  
    %          R=pipeline.last.reference.buildRefImages(LAST_RefIm_Grid,'DB',D,'RefID',[99945 99946]); % a short test
    %          R=pipeline.last.reference.buildRefImages([],'DB',D,'RA',210.8,'Dec',54.3,'RefName',"myField"); % ad hoc sky point
    arguments
        RefID                  = 146446;
        Args.RefTable          = [];
        Args.RefTableName      = 'LAST_RefIm_Grid.mat';                                               
        %Args.RefID             = []; % e.g., [120000 120001] or [120000:120020]; input range of ref. image numbers  
        
        Args.RefWCS            = [];    % use an array of pre-built WCS (e.g., from the RefGrid object)
        Args.RA                = [];    % [deg] optional ad hoc sky point(s), instead of the RefID/RefTable grid
        Args.Dec               = [];    % [deg] matching Dec for each Args.RA
        Args.RefName           = [];    % output filename tag(s) for the ad hoc sky point(s); default is sequential numbering
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
        
        Args.QueryFilter       = "fwhm < 4"; % a user-supplied filter (to be included directly into the SQL query) 
                       
        Args.RasterResolution   = 3;     % arcsec
        Args.MinCoverage        = 0.999; % 0.995; % allowed inaccuracy in the required reference field coverage  
                               
        %Args.backVarArgs        = {'Method',@imUtil.background.modeVar_Hist, 'Block',[128 128], 'MethodArgs',{{'Range',[-50 50]}}}
        %Args.backVarArgs        = {'Method',{@imUtil.background.modeVar_Hist, @imUtil.background.rvar} 'Block',[256 256], 'MethodArgs',{{'Range',[-20 20], 'ApplyCeil',false, 'NinBin',100}, {}} };
        %Args.backVarArgs        = {'Method',{@imUtil.background.modeVar_Hist, @imUtil.background.rvar} 'Block',[512 512], 'MethodArgs',{{'Range',[-20 20], 'ApplyCeil',false, 'NinBin',50}, {}} };
        Args.backVarArgs        = {'Method','backBertinLowerRMS', 'MethodArgs',{} };
        %Args.backVarArgs        = {'Method',{@imUtil.background.modeVar_Hist, @imUtil.background.rvar} 'Block',[], 'MethodArgs',{{'Range',[-20 20], 'ApplyCeil',false, 'NinBin',50}, {}} };
        %Args.backVarArgs        = {'Method',@imUtil.background.modeVar_Hist, 'Block',[256 256], 'MethodArgs',{{'Range',[-20 20], 'ApplyCeil',false, 'NinBin',100}, {}} };
        Args.backVarIndivArgs   =  {'Method',@imUtil.background.modeVar_LogHist, 'Block',[512 512], 'MethodArgs',{{},{}}};

        Args.Threshold          = [500 100 50 20 4];

        Args.SubBack            = true;  % don't change unless you understand what you are doing
        Args.StackMethod        = 'wrobust';
        Args.StackMethodArgs    = {}; %{'coadd_WRobustArgs',{'backVarArgs',{'Method',@imUtil.background.modeVar_Hist}}};     
        Args.CoaddFunctionArgs  = {}; % additional arguments to be passed to the coadd function 
        
        Args.PixScale           = 1.25;
        Args.EdgeDist           = 10;  % [pix] distance from the frame edge to flag with the NearEdge mask bit
        
        Args.Write2Disk         = true;
        Args.OutputDir          = '~/NewRef/';        
        Args.WriteProp          = ["Image","Cat","Mask","PSF"];
        
        Args.OutputRefTable     = 'ref_images_v5'; % the output DB table name  
        
        Args.DbHost             = '10.150.28.18' 
        Args.DbPort             = 9000;
        Args.DbUser             = 'last_user'
        Args.DbName             = 'last_ro'
        Args.AstroDBPassFile    = '~/matlab/AstroPack/config/local/Passwords.yml'; % '~/.astropack/Passwords.yml';
                
        Args.AstrometricCatRad     = 1;           % [deg] cone radius for pre-fetching reference catalogs
        Args.AstrometricCatMagRange = [12 19.5];  % magnitude range for the astrometric catalog
        Args.AstrometricCatPlxRange = [-Inf 50];  % parallax range [mas] for the astrometric catalog
        Args.PhotCatMagRange        = [13 21.5];  % magnitude range for the photometric catalog
        Args.PhotCatPlxRange        = [0.1 100];  % parallax range [mas] for the photometric catalog
        
        Args.Verbose                = 0; % from 0 (mute) to 2 (chatty)
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

    SkyPointMode = ~isempty(Args.RA);
    if SkyPointMode
        % build an ad hoc, one-row-per-point RefGrid instead of loading the RefID/RefTable grid
        [RefGrid, Args.RefWCS, Args.RefName] = buildSkyPointGrid(Args.RA, Args.Dec, Args.RefName, ...
            Args.Naxis1, Args.Naxis2, Args.PixScale);
        RefID = 1:height(RefGrid);
    else
        if isempty(Args.RefTable)
            RefGrid = io.files.load2(Args.RefTableName);
        else
            RefGrid = Args.RefGrid;
        end
        % loop over the Reference Image grid that has been read above
        if isempty(RefID)
            RefID = 1:height(RefGrid);
        end
    end
    Nref = height(RefGrid);

    Ibp = find(isfolder(Args.BasePath), 1, 'first');
    Args.BasePath = Args.BasePath(Ibp);
    
    Info.CounterBadWCS  = 0;
    Info.CounterGoodWCS = 0;

    % build the mask bit dictionary once and reuse it in every stitchCrops call
    BitDict = BitDictionary('BitMask.Image.Default');

    % the main loop over the reference grid
    K = 0;
    for Iref = RefID
        K = K + 1;
        if SkyPointMode
            Tag = Args.RefName(K);
        else
            Tag = string(Iref);
        end

        if Args.Verbose > 0
            tstart = tic;
            cprintf('blue','Starting to build a reference image for field %d of %d at RA %.2f Dec %.2f \n',Iref,Nref,RefGrid.RA(Iref),RefGrid.Dec(Iref));
        end
            
        % read or build the WCS of the target reference image
        if ~isempty(Args.RefWCS)
            RefWCS = Args.RefWCS(Iref);
        else
            RefWCS = AstroWCS.buildSimpleWCS(RefGrid.RA(Iref),RefGrid.Dec(Iref),'Naxis1',Args.Naxis1,'Naxis2',Args.Naxis2,...
                'PixScale',Args.PixScale); 
            % NOTE: when the right values of ref. image PA are written to the RefGrid, change for this:
            %  RefWCS = AstroWCS.buildSimpleWCS(RefGrid.RA(Iref),RefGrid.Dec(Iref),'Naxis1',Args.Naxis1,'Naxis2',Args.Naxis2,...
            %                      'PA',RefGrid.PA(Iref),'PixScale',Args.PixScale);
        end        
        % create an empty reference AstroImage and attach the RefWCS to it
        AIref = AstroImage({zeros(Args.Naxis2,Args.Naxis1,'single')}); % rows=Naxis2=Y, cols=Naxis1=X
        AIref.WCS = RefWCS; 
        AIref.WCS.Success = true;  % must have Success=true
        AIref.HeaderData = AIref.WCS.wcs2header; 
        
        % 0. build the ref polygon to be covered and find the healpix coverage
        P0 = [RefGrid.RA1(Iref), RefGrid.Dec1(Iref); RefGrid.RA2(Iref), RefGrid.Dec2(Iref); ...
              RefGrid.RA3(Iref), RefGrid.Dec3(Iref); RefGrid.RA4(Iref), RefGrid.Dec4(Iref)]; 
        [UpixLow, Raster0] = celestial.healpix.pixCoversPolygon(P0, 'RA0',RefGrid.RA(Iref), 'Dec0',RefGrid.Dec(Iref), ...
            'RasterResolution',Args.RasterResolution, 'NsideSearch',Args.NsideSearch, 'NsideLow',Args.NsideLow);
        
        % 1. find the overlapping coadd proc or single-epoch proc images (determined by Args.SearchTable)
        Q = sprintf("select %s from %s where",Args.Fields, Args.SearchTable);
        PixList = strjoin("toString(" + string(UpixLow(:)).' + ")", ", ");
        W = sprintf(" toString(upix_low) IN (%s)", PixList);
        
        % add image quality filter
        if ~isempty(Args.QueryFilter)
            W = strcat("(",W,") and ",Args.QueryFilter); 
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

            % pre-fetch reference catalogs once for this field (shared across all groups)
            if Args.Verbose > 1
                fprintf('Pre-fetching astrometric and photometric catalogs (r=%.1f deg)...\n', Args.AstrometricCatRad);
            end
            RawMagRange = [min(Args.AstrometricCatMagRange(1), Args.PhotCatMagRange(1)), max(Args.AstrometricCatMagRange(2), Args.PhotCatMagRange(2))];
            RawPlxRange = [min(Args.AstrometricCatPlxRange(1), Args.PhotCatPlxRange(1)), max(Args.AstrometricCatPlxRange(2), Args.PhotCatPlxRange(2))];
            FullCat = imProc.cat.getAstrometricCatalog(RefGrid.RA(Iref), RefGrid.Dec(Iref), ...
                'Radius',Args.AstrometricCatRad,'RadiusUnits','deg','OutUnits','rad', 'RangeMag',RawMagRange,'RangePlx',RawPlxRange);
            AstrometricCat = queryRange(FullCat, {'phot_bp_mean_mag','phot_g_mean_mag'}, Args.AstrometricCatMagRange, ...
                'Plx', Args.AstrometricCatPlxRange); 
            PhotCat        = queryRange(FullCat, {'phot_bp_mean_mag','phot_g_mean_mag'}, Args.PhotCatMagRange, ...
                'Plx', Args.PhotCatPlxRange); 

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
            Info(K).NimagesFootprint = size(T,1);

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
                    Raster = celestial.healpix.mex.rasterize_polygon(CropPoly, Args.RasterResolution,'arcsec');                         
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
                if CoverageAll < Args.MinCoverage
                    % incomplete coverage: skip this epoch
                    if Args.Verbose > 1
                        fprintf('Incomplete coverage of %.4f, epoch %d is skipped\n', CoverageAll, Igroup);
                    end
                else
                    % 4.1 retrieve the crop images
                    if Args.Verbose > 0
                        fprintf('Group %d: %d images filtered, dowloading and stitching...',Igroup,Nim);
                    end

                    % Replace this block after verification
                    AF = AstroFileName;
                    AF.ProjName = {'LAST', 1, TabGrp.mountnum, TabGrp.camnum};
                    AF.JD = double(TabGrp.jd_start);
                    AF.julday2time;
                    AF.Time = extractBefore(AF.Time, ".") + "." + TabGrp.filetime; % repair the last digits from 'filetime'
                    AF.FieldID = TabGrp.fieldid;
                    AF.CropID  = TabGrp.cropid;
                    AF.Counter = 0;
                    AF.Level   = "coadd";
                    AF.CCDID   = 1;
                    AF.SubDir  = TabGrp.subdir;
                    AF.BasePath                = Args.BasePath;
                    AF.BasePathIncludeProjName = true;
                    AF.AddSubDir               = true;

                    AI = AstroImage.readProducts(AF.genFull, 'UseMex', true);

                    % for:
%                     AI=pipeline.last.queryDB.loadProducts(TabGrp); % does not load anything ?

                    % check if WCS is present in all the selected crops
                    if any(isnan(arrayfun(@(x) x.WCS.PhiP, AI)))
                        % bad WCS in one or several crops: skip this epoch
                        Info(K).CounterBadWCS = Info(K).CounterBadWCS + 1;
                        if Args.Verbose > 0
                            cprintf('red','\nWCS is not correct in one or several crops, skipping the epoch %d\n',Igroup);
                        end
                    else
                        Info(K).CounterGoodWCS = Info(K).CounterGoodWCS + 1;

                        % 4.2 stitch the set of covering crops
                        %                         telescope.obs.plotFOVfromQueryTable(TabEpoch,'Lines',L)
                        StitchedImage = imProc.stack.stitchCrops(AI, ...
                            'UpdateWCS',true,'UpdateZP',true, ...
                            'AstrometricCat',AstrometricCat,'PhotCat',PhotCat, ...
                            'BitDict',BitDict);

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
                    end
                end
            end % groups (epochs + telescopes)
            
            % do the stacking 
            if isempty(StackImages) || numel(StackImages)<2
                if Args.Verbose > 0
                    cprintf('err','No images have been qualified for the field %d, skipping to the next field..\n',Iref);
                end  
                RefImage = AstroImage; % empty
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
                                    'ReMeasureBack',true,...                                    
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
                                    'AddMaskSrcNoise',false,...
                                    'AddLimMag',true,...
                                    'LimMagArgs',{},...
                                    'AddBackMag',true,...
                                    'KeyZP','PH_ZP',...
                                    'BackMagArgs',{});

                % 5a. flag the frame edges of the coadded/warped reference image
                EdgeFlag = imUtil.ccdsec.selectNearEdges(size(RefImage.Image), Args.EdgeDist);
                RefImage.MaskData = RefImage.MaskData.maskSet(EdgeFlag, 'NearEdge', 1);

                % 5b. add the ID_REF keyword
                RefImage.HeaderData = replaceVal(RefImage.HeaderData, 'MOUNTNUM', 0);
                RefImage.HeaderData = replaceVal(RefImage.HeaderData, 'CAMNUM', 0);
                JD = RefImage.getStructKey('MIDJD').MIDJD;
                [RefImage,~] = imProc.db.generateImageID(RefImage,'KeyID','ID_REF','JD',JD);
                
                % 6. save the new reference image and its catalog, mask, and PSF to the disk
                if Args.Write2Disk
                    for Iprop=1:numel(Args.WriteProp)
                        FN = sprintf('%s/LAST_clear_%s_sci_ref_%s_1.fits',Args.OutputDir,Tag,Args.WriteProp(Iprop));
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

function [RefGrid, RefWCS, RefName] = buildSkyPointGrid(RA, Dec, RefName, Naxis1, Naxis2, PixScale)
    % build a one-row-per-point RefGrid (and matching WCS array) for ad hoc sky-point reference images
    % Input  : - [deg] array of sky point RA.
    %          - [deg] array of sky point Dec, matching RA element-by-element.
    %          - output filename tag(s) matching RA/Dec. If empty, defaults to sequential "1","2",...
    %          - Naxis1, Naxis2, PixScale, as in buildRefImages.
    % Output : - RefGrid table with columns RA, Dec, RA1..RA4, Dec1..Dec4 (image center and corners).
    %          - array of North-oriented AstroWCS objects, one per point.
    %          - resolved RefName string array.
    % Author : A.M. Krassilchtchikov (2026 Jul)
    if numel(RA) ~= numel(Dec)
        error('RA and Dec must have the same number of elements');
    end
    Npt = numel(RA);
    if isempty(RefName)
        RefName = string(1:Npt);
    else
        RefName = string(RefName);
        if numel(RefName) ~= Npt
            error('RefName must have the same number of elements as RA/Dec');
        end
    end

    RefGrid = table();
    RefGrid.RA  = RA(:);
    RefGrid.Dec = Dec(:);

    RefWCS(Npt,1) = AstroWCS;
    for Ipt = 1:Npt
        % North-oriented WCS (no PA given)
        RefWCS(Ipt) = AstroWCS.buildSimpleWCS(RA(Ipt), Dec(Ipt), 'Naxis1',Naxis1, 'Naxis2',Naxis2, 'PixScale',PixScale);
        Corners = RefWCS(Ipt).cooImage([1 Naxis1 1 Naxis2]).Corners;
        RefGrid.RA1(Ipt)  = Corners(1,1); RefGrid.Dec1(Ipt) = Corners(1,2);
        RefGrid.RA2(Ipt)  = Corners(2,1); RefGrid.Dec2(Ipt) = Corners(2,2);
        RefGrid.RA3(Ipt)  = Corners(3,1); RefGrid.Dec3(Ipt) = Corners(3,2);
        RefGrid.RA4(Ipt)  = Corners(4,1); RefGrid.Dec4(Ipt) = Corners(4,2);
    end
end
