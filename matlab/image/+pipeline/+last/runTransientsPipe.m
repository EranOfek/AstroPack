function [AD, ADc, Status] = runTransientsPipe(VisitPath, Args)
    %{
    Performs the subtraction and transient search algorithms using 
    AstroDiff on images within a visit directory. 
    Input   : - Path to visit directory holding sub-image coadds.
              * ...,key,val,...
                'SaveProducts' - Bool on whether to save subtraction and 
                       transients products. Default is false.
                'SavePath' - Path to directory in which to save products in
                       case SaveProducts is true. Default is the VisitPath.
                'RefPath' - Path to directory with images. If empty, 
                       constructs assuming reference directory is 
                       "/'machine_name'/data/references'. Default is ''.
                'Product' - Products to be saved in case SaveProducts is
                       true. Default is ''.
                'WriteHeader' - Array of bools indicating on whether to 
                       write a head for the products. Required by 
                       imProc.io.writeProduct and has to be the same length 
                       as Product. Default is ''.
                'SaveMergedCat' - Bool on whether to save all produced
                       transients catalogs as a single merged catalog.
                       Default is true.
                'AddMeta' - Bool on whether to add some meta data to the
                       transients catalog, for e.g. mount, camera, croID data. 
                       Default is true.
                'SameTelOnly' - Bool on whether to force to use the same
                       exact same telescope (same mount) for reference
                       images. Default is true.
    Output  : - AstroDiff objects holding all products and results derived 
                by the algorithm.
              - AstroDiff cutouts around each single transients candidate 
                which passes the flagging criteria.
              - Result message
    Author  : Ruslan Konno (Jun 2024)
    Example : VisitPath = '/path/to/visit/dir'
              [AD, ADc] = runTransientsPipe(VisitPath)
    %}

    arguments
        VisitPath

        Args.SaveProducts logical = false;
        Args.SavePath = VisitPath;
        Args.RefPath = '';
        Args.Product = '';
        Args.WriteHeader = '';
        Args.SaveMergedCat logical = true;
        Args.AddMeta logical = true;
        Args.SameTelOnly logical = true;
    end

    % Set default status.
    Status = 'Uncontrolled exit.';

    % Find New image coadds and load
    
    if isa(VisitPath, 'char') || isa(VisitPath, 'string')
        Coadds = strcat(VisitPath,'/LAST*coadd_Image_1.fits');
        New = AstroImage.readFileNamesObj(Coadds, 'Path', VisitPath);
    elseif isa(VisitPath, 'AstroImage')
        New = VisitPath;
    end

    % Get path of reference images
    if isempty(Args.RefPath)
        Computer = tools.os.get_computer;
        RefPath = strcat('/',Computer,'/data/references');
    else
        RefPath = Args.RefPath;
    end
    
    % Find reference image for each new image
    Nobj = numel(New);

    % Track number of found reference images
    NRefsFound = 0;

    % Initialize empty output arguments
    AD = AstroZOGY();
    ADc = AstroZOGY();
    
    for Iobj=Nobj:-1:1
        % Get name of new image and search for ref image via wildcards
        FN = FileNames.generateFromFileName(New(Iobj).ImageData.FileName);
        FNref = FN.copy();
        if ~Args.SameTelOnly
            FNref.ProjName={replaceBetween(FNref.ProjName{1},"LAST.01.",".0","*")};
        end
        FNref.Time = {'*.*.*'};
        FieldID = FNref.FieldID{1};

        FieldRefPath = strcat(RefPath, '/', FieldID);
        RefFile = fullfile(FieldRefPath,FNref.genFile);

        % Continue if no ref image found
        if isempty(dir(RefFile{1}))
            warning('Reference image not found for image %s', FN.genFile{1});
            continue
        else
            NRefsFound = NRefsFound + 1;
        end
    
        % Load ref image and ref image name
        Ref = AstroImage.readFileNamesObj(RefFile{1}, 'Path', FieldRefPath);
        FNrref = FileNames.generateFromFileName(Ref.ImageData.FileName);

        NewName = FN.genFile;
        RefName = FNrref.genFile;

        % Compare new and ref image names, continue if both are the same
        % image
        if convertCharsToStrings(NewName{1}) == convertCharsToStrings(RefName{1})
            warning('New image is reference image.');
            NRefsFound = NRefsFound - 1;
            continue
        end

        % Create AstroDiff
        AD(Iobj) = AstroZOGY(New(Iobj), Ref);
    end

    % If no reference images found, return
    if NRefsFound < 1
        Status = 'No reference images found.';
        return;
    end

    % Remove empty AstroDiff objects and remember number of AstroDiffs
    NonEmptyCell = any(~cellfun('isempty',{AD(:).New}), 1);
    if ~any(NonEmptyCell)
        Status = 'All AstroDiffs are empty.';
        return;
    end
    
    AD = AD(:, NonEmptyCell);
    Nobj = numel(AD);

    % Register New and Ref
    AD.register;
    % Estimate backround and variance of New and Ref
    AD.estimateBackVar;
    % Estimate zero points
    AD.estimateFnFr;
    % Create proper subtraction image D
    AD.subtractionD;
    % Create gabor magnitude filter
    AD.matchfilterGabor;
    % Derive S stat image
    AD.subtractionS;
    % Derive Scorr stat image
    AD.subtractionScorr;
    % Derive Z2 stat image
    AD.translient;
    % Find transients
    AD.findTransients;
    % Catalog match

    % Merged cat
    %imProc.match.match_catsHTMmerged(AD);
    %imProc.match.match_catsHTM(AD,'MergedCat',...
    %    'ColDistName','MergedDist','ColNmatchName','MergedMatches');

    % Galaxy match
    imProc.match.match2Galaxies(AD);

    % Star match
   
    % Cut down catalog to full visit image
    for Iobj=Nobj:-1:1
        C_RA(Iobj) = convert.angular('deg','rad',AD(Iobj).HeaderData.getVal('RA'));
        C_Dec(Iobj) = convert.angular('deg','rad',AD(Iobj).HeaderData.getVal('Dec'));        
    end

    C_RA_med = median(C_RA);
    C_Dec_med = median(C_Dec);

    MaxDistRad = max(celestial.coo.sphere_dist(...
        C_RA, C_Dec, C_RA_med, C_Dec_med, 'rad'), [], 'all')*1.5;

    StarCat = catsHTM.cone_search('GAIADR3', C_RA_med, C_Dec_med, ...
        MaxDistRad, 'Con', {{'phot_bp_mean_mag', @(x) 21>(x)}},...
    'RadiusUnits','rad', 'OutType','AstroCatalog');
    StarCat.sortrows('Dec');

    % Search for star matches on cutdown catalog
    imProc.match.match2Stars(AD,'StarCat',StarCat);
    % Clear for memory
    clear StarCat;

    % MP match

    % Get asteroid catalogs for new and ref
    INPOP = celestial.INPOP;
    INPOP.populateAll;
    OrbEl= celestial.OrbitalEl.loadSolarSystem('merge');

    % Propogate catalog to new image epoch
    NewJulDay = median(arrayfun(@(x) x.New.julday,AD));

    [AstCat_new] = searchMinorPlanetsNearPosition(...
        OrbEl, NewJulDay, C_RA_med, C_Dec_med, MaxDistRad,...
        'INPOP', INPOP, 'CooUnits','rad', 'SearchRadiusUnits','rad',...
        'QuickSearchBuffer', 500,'MagLimit', 21,...
        'RefEllipsoid','WGS84',...
        'OutUnitsDeg',true,'Integration', true);

    % Match MP in new
    [~,~,AD] = imProc.match.match2solarSystem(AD, 'InCooUnits', 'deg', ...
                    'SourcesColDistName', 'N_DistMP', 'AstCat', AstCat_new,...
                    'JD', NewJulDay, 'AddMag2Obj', true, ...
                    'ColMag', 'Mag', 'ObjColMag', 'N_MagMP',...
                    'SearchRadius',20);

    % Clear for memory
    clear AstCat_new;

    % Propogate catalog to ref image epoch
    RefJulDay = median(arrayfun(@(x) x.Ref.julday,AD));

    [AstCat_ref] = searchMinorPlanetsNearPosition(...
        OrbEl, RefJulDay, C_RA_med, C_Dec_med, MaxDistRad,...
        'INPOP', INPOP, 'CooUnits','rad', 'SearchRadiusUnits','rad',...
        'QuickSearchBuffer', 500,'MagLimit', 21,...
        'RefEllipsoid','WGS84',...
        'OutUnitsDeg',true,'Integration', true);
    
    % Match MP in ref
    [~,~,AD] = imProc.match.match2solarSystem(AD, 'InCooUnits', 'deg', ...
                    'SourcesColDistName', 'R_DistMP', 'AstCat', AstCat_ref,...
                    'JD', RefJulDay, 'AddMag2Obj', true, ...
                    'ColMag', 'Mag', 'ObjColMag', 'R_MagMP',...
                    'SearchRadius',20);

    %Clear for memory
    clear AstCat_ref;
    clear INPOP;
    clear OrbEl;

    % Measure transients
    AD.measureTransients;
    % Flag non transients
    AD.flagNonTransients;
    % Remove clear bad transients
    %AD = AD.removeNonTransients('removeCol','BadPixel_Hard');
    
    % If AddMeta true, add meta information to catalog
    % TODO: clean this up, maybe move elsewhere
    if Args.AddMeta
        for Iobj=1:1:Nobj
            
            Header = AD(Iobj).HeaderData;
            NumTran = size(AD(Iobj).CatData.Catalog,1);
            Mount = Header.getVal('MOUNTNUM')*ones(NumTran,1);
            Cam = Header.getVal('CAMNUM')*ones(NumTran,1);
            CropID = Header.getVal('CROPID')*ones(NumTran,1);
            FWHM_new = AD(Iobj).New.PSFData.fwhm*ones(NumTran,1);
            FWHM_ref = AD(Iobj).Ref.PSFData.fwhm*ones(NumTran,1);
            LIMMAG_new = AD(Iobj).New.HeaderData.getVal('LIMMAG')*ones(NumTran,1);
            LIMMAG_ref = AD(Iobj).Ref.HeaderData.getVal('LIMMAG')*ones(NumTran,1);
            PH_COL1_new = AD(Iobj).New.HeaderData.getVal('PH_COL1')*ones(NumTran,1);
            PH_COL1_ref = AD(Iobj).Ref.HeaderData.getVal('PH_COL1')*ones(NumTran,1);            
            Exposure_new = AD(Iobj).New.HeaderData.getVal('EXPTIME')*ones(NumTran,1);
            Exposure_ref = AD(Iobj).Ref.HeaderData.getVal('EXPTIME')*ones(NumTran,1);
            ZP_new = AD(Iobj).Fn*ones(NumTran,1);
            ZP_ref = AD(Iobj).Fr*ones(NumTran,1);
            ZP_D = AD(Iobj).Fd*ones(NumTran,1);
    
            AD(Iobj).CatData.insertCol(...
                cell2mat({cast(Mount,'double'), cast(Cam,'double'), cast(CropID,'double'), ...
                cast(FWHM_new,'double'), cast(FWHM_ref,'double'), cast(LIMMAG_new,'double'),...
                cast(LIMMAG_ref,'double'),cast(ZP_D,'double'),cast(ZP_new,'double'),...
                cast(ZP_ref,'double'),cast(PH_COL1_new,'double'),cast(PH_COL1_ref,'double'), ...
                cast(Exposure_new,'double'),cast(Exposure_ref,'double')}), ...
                'SCORE',...
                {'MOUNT','CAM','CROPID','N_FWHM','R_FWHM','N_LIMMAG',...
                'R_LIMMAG','ZP','N_ZP','R_ZP', 'N_PH_COL1', 'R_PH_COL1', ...
                'N_EXPTIME','R_EXPTIME'}, ...
                {'','','','','','mag','mag','','','','','','s','s'});
        end
    end

    % If SaveProducts true, save desired products in desired path
    if Args.SaveProducts
        for Iobj=Nobj:-1:1

            TranCat(Iobj) = AD(Iobj).CatData;

            if isempty(Args.Product)
                continue
            end

            FN = FileNames.generateFromFileName(AD(Iobj).New.ImageData.FileName);
            % Set AD name
            FNad = FN.copy();
            FNad.Level = {'coadd.zogyD'};
            FNad.FullPath = Args.SavePath;
            AD(Iobj).ImageData.FileName = FNad.genFull{1};

            [~,~,~]=imProc.io.writeProduct(AD(Iobj), FNad, ...
                'Level', 'coadd.zogyD', 'Product', Args.Product,...
                'WriteHeader',Args.WriteHeader,'Overwrite', true);

        end

        if Args.SaveMergedCat
            FN = FileNames.generateFromFileName(AD(1).New.ImageData.FileName);
            FN_merged = FN.copy();
            FN_merged.Level = {'coadd.zogyD'};
            FN_merged.CropID = 0;
            FN_merged.Product = {'Cat'};
            FN_merged.FullPath = Args.SavePath;

            MergedTranCat = merge(TranCat);

            [~,~,~]=imProc.io.writeProduct(MergedTranCat, FN_merged, ...
                'Level', 'coadd.zogyD', 'Product', {'Cat'},...
                'WriteHeader',false,'Overwrite', true, 'GetHeaderJD', false, ...
                'CropID_FromIndex',false);
        end
    end

    % Get transients only for cutouts
    ADn = removeNonTransients(AD);
    % Make cutouts
    ADc = ADn.cutoutTransients;

    % Save cutouts
    % TODO: Structure file holding all of the cutouts can sometimes be 
    % several GB large. Investigate this issue.
    
    %{
    if Args.SaveProducts
        FN = FileNames.generateFromFileName(AD(1).New.ImageData.FileName);
        % Set AD name
        FNtran = FN.copy();
        FNtran.Level = {'coadd.zogyD'};
        FNtran.Product = {'TransientsCat'};
        FNtran.CropID = 0;
        FNtran.FileType = {'mat'};
        FNtran.FullPath = Args.SavePath;
        TranCatFileName = FNtran.genFull{1};
        
        save(TranCatFileName,"ADc","-v7.3");
    end  
    %}

    NADc = numel(ADc);
    if NADc == 1 && isempty(ADc(1).Table)
        NADc = 0;
    end
    StatusCell = strcat('Succesful exit,',{' '}, ...
        num2str(NADc),{' '},'transient(s) found.');
    Status = StatusCell{1};
    
end