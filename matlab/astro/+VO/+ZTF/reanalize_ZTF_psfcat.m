function [Result, ObjCat, OnlyMP, AstrometricCat, Status, Path] = reanalize_ZTF_psfcat(File, Args)
    % Given ZTF psfcat apply  photometric ZP, astrometry and match to known asteroids
    % Input  : - FITS table name, or a cell array of FITS table names
    %          * ...,key,val,...
    %            see code for options
    % Output : - Array of calibrated AstroCatalog objects, one per input.
    %          - Calibrated catalog for the last input (legacy output).
    %          - Minor-planet catalog for the last input (empty if disabled).
    %          - Astrometric reference catalog for the last input.
    %          - Per-file status structure array.
    %          - Absolute input working directory.
    % WriteProd saves <input basename>_recalibrated.mat in OutputDir,
    % including ObjCat, OnlyMP, StatusEntry, and calibration metadata H.
    % Original FITS files are never overwritten. Calibrated magnitudes use
    % the current fitPhotCalibTrans MAG_AB_* naming; FLUX_PSF remains native.
    % Example:
    % [Cats,~,~,Ref,Status] = VO.ZTF.reanalize_ZTF_psfcat(...
    %     {'ztf_20180426501748_000596_zg_c10_o_q3_psfcat.fits'}, ...
    %     'RunSolarSystem',false,'WriteProd',false);

    arguments
        File
        Args.Path              = [];
        Args.AstrometricCat    = 'GAIADR3'
        Args.OrbEl             = [];
        Args.INPOP             = [];
        Args.GeoPos            = 'Palom';       % [Lon (rad), Lat (rad), Height (m)]. 

        Args.WriteProd logical = true;
        Args.FieldID           = [];
        Args.CCDID             = [];

        Args.RunPhotCalib logical = true;
        Args.RunSolarSystem logical = true;
        Args.OutputDir = 'recalibrated';
        Args.OverWrite logical = false;

    end
    RAD = 180./pi;

    FilterList = {'zg','zr','zi'};

    if ischar(Args.GeoPos) || isstring(Args.GeoPos)
        [Args.GeoPos] = telescope.obs.observatory_coo(Args.GeoPos);
    end
    if numel(Args.GeoPos) == 2
        Args.GeoPos(3) = 0;  % observatory_coo returns [lon,lat] in radians
    end

    if Args.RunSolarSystem && isempty(Args.OrbEl)
        Args.OrbEl = celestial.OrbitalEl.loadSolarSystem('merge');
    end
    if Args.RunSolarSystem && isempty(Args.INPOP)
        Args.INPOP = celestial.INPOP.init;
    end


    if ischar(File)
        FileList = {File};
    elseif isstring(File)
        FileList = cellstr(File(:));
    elseif iscell(File)
        FileList = File(:);
    else
        error('VO:ZTF:InvalidInput','Expected FITS filenames, not catalog objects.');
    end
    if isempty(FileList) || ~all(cellfun(@(f) ischar(f) || (isstring(f) && isscalar(f)), FileList))
        error('VO:ZTF:InvalidInput','Supply a nonempty list of FITS filenames.');
    end
    FileList = cellfun(@char, FileList, 'UniformOutput',false);
    Nfile = numel(FileList);

    PWD = pwd;
    RestoreDir = onCleanup(@() cd(PWD));
    if ~isempty(Args.Path)
        cd(Args.Path);
    end
    Path = pwd;

    Result = AstroCatalog([Nfile,1]);
    Status = repmat(ztfReductionStatus(),Nfile,1);
    ObjCat = AstroCatalog;
    OnlyMP = AstroCatalog;
    AstrometricCat = [];
    if Args.WriteProd && ~isfolder(Args.OutputDir)
        mkdir(Args.OutputDir);
    end

    for Ifile=1:1:Nfile
        
        StartTime = tic;
        File = FileList{Ifile};


    
        OnlyMP = AstroCatalog;
        Status(Ifile).File = File;
        Status(Ifile).Path = Path;
        [~,BaseName] = fileparts(File);
        OutputFile = fullfile(Args.OutputDir,[BaseName '_recalibrated.mat']);
        if Args.WriteProd && isfile(OutputFile) && ~Args.OverWrite
            error('VO:ZTF:ProductExists','Product already exists: %s',OutputFile);
        end
        ObjCat = FITS.readTable1(File, 'OutTable','AstroCatalog');
        FileSplit = split(BaseName,'_');
        if numel(FileSplit)~=8 || ~strcmp(FileSplit{1},'ztf') || ~strcmp(FileSplit{8},'psfcat')
            error('VO:ZTF:InvalidFilename','Expected a ZTF psfcat filename: %s',File);
        end
        FieldID = FileSplit{3};
        CCDID = FileSplit{5}(2:end);
        CropID = FileSplit{7}(2:end);
        Filter = FileSplit{4};
        FilterInd = find(strcmp(FilterList,Filter),1);
        if isempty(FilterInd)
            error('VO:ZTF:InvalidFilter','Unknown ZTF filter: %s',Filter);
        end
        if (~isempty(Args.FieldID) && str2double(string(Args.FieldID))~=str2double(FieldID)) || ...
           (~isempty(Args.CCDID) && str2double(string(Args.CCDID))~=str2double(CCDID))
            error('VO:ZTF:MetadataMismatch','Filename does not match requested field/CCD.');
        end
        Head = AstroHeader(File);
        JD = Head.Key.OBSMJD + 2400000.5;
        ObjCat.JD = JD;
        % Keep the archive observation time; do not silently shift to mid-exposure.
        ExpTime = 30;  % fallback when absent from the PSF-catalog header
        if isfield(Head.Key,'EXPTIME') && isfinite(Head.Key.EXPTIME)
            ExpTime = Head.Key.EXPTIME;
        end
            % calculate airmass
            MeanRA  = mean(ObjCat.Table.ra,1,'omitmissing');
            MeanDec = median(ObjCat.Table.dec,1,'omitmissing');


            [~,~,AM]=celestial.coo.radec2azalt(JD, MeanRA, MeanDec, ...
                'GeoCoo',[Args.GeoPos(1:2).*RAD, Args.GeoPos(3)./1000]);
           

            H = AstroHeader;
            H.Data = { ...
                'EXPTIME',  ExpTime,               'Exposure time [s]'; ...
                'AIRMASS',  AM,            'Airmass of the exposure'; ...
                'MNTTEMP',  10,               'Ambient temperature [C]'; ...
                'PRESSURE', 825,              'Pressure [mbar] - Palomar, 1712 m'; ...
                'NCOADD',   1,                'Single exposure'; ...
                'CCDSEC',   '[1 3072 1 3080]','One ZTF readout quadrant'; ...
                'JD',       JD,    'Observation JD'};
    
        % Re do astrometry
        SortCat = 'dec';
    
        OrigKey = {'xpos','ypos','ra','dec','flux','sigflux'};
        NewKey  = {'X','Y',      'RA','Dec','FLUX_PSF','FLUXERR_PSF'};
       
        ObjCat.replaceColNames(OrigKey, NewKey);
    
        ObjCat.sortrows('Dec');
        
        OrigCooData = getCol(ObjCat, {'RA','Dec'});

        ObjCat = insertCol(ObjCat, OrigCooData, Inf, {'ALPHAWIN_J2000','DELTAWIN_J2000'}, {'deg','deg'});

        [ResAstrometry, ObjCat, AstrometricCat] = imProc.astrometry.astrometryRefine(ObjCat, 'WCS',[],...
                                'CatName',Args.AstrometricCat, 'RA',[], 'Dec',[], 'SortCat',SortCat,...
                                'EpochOut',JD);

        Status(Ifile).AstNgood = ResAstrometry.ResFit.Ngood;
        Status(Ifile).AstAssymRMS = ResAstrometry.ResFit.AssymRMS;
        ObjCat.UserData.Astrom = ResAstrometry.ResFit;
        % RefMag/ResidX/ResidY are retained for Gaia RMS-versus-magnitude
        % uncertainty calibration; AST_ARMS is a field summary, not a source error.

        if Args.RunPhotCalib

            % Add MergedCat
            ObjCat = imProc.match.match_catsHTMmerged(ObjCat);
            
            
            [ObjCat, PC] = imProc.calib.fitPhotCalibTrans(ObjCat, 'MagType', 'mag', 'Verbose',false, 'AddMagErr', true, 'FluxColName','FLUX_PSF', 'ExtraCalibArgs', {'Mission','ZTF', 'Band',Filter(2), 'Metadata',H,'Aperture',pi*0.605^2}); % 8.7s for all in loop
        
            
        
         

            ObjCat.UserData.PC = PC;
            Status(Ifile).PhotCalibrated = PC.CalFound;
            if ~PC.CalFound
                Status(Ifile).Error = 'Photometric calibration failed';
            else
                Status(Ifile).PT_ARMS = PC.ARMS;
            end
        end
        if Args.RunSolarSystem
            [OnlyMP,~,~] = imProc.match.match2solarSystem(ObjCat, 'JD',JD, ...
                'GeoPos',Args.GeoPos,'OrbEl',Args.OrbEl,'SearchRadius',10,'INPOP',Args.INPOP);
            Nast = OnlyMP.sizeCatalog;
            if Nast>0
                insertCol(OnlyMP,repmat(JD,Nast,1),Inf,'JD','day');
                insertCol(OnlyMP,repmat(FilterInd,Nast,1),Inf,'Filter');
                insertCol(OnlyMP,repmat(str2double(FieldID),Nast,1),Inf,'FieldID');
                insertCol(OnlyMP,repmat(str2double(CCDID),Nast,1),Inf,'CCDID');
                insertCol(OnlyMP,repmat(str2double(CropID),Nast,1),Inf,'CropID');
            end
        end
                Nsrc = ObjCat.sizeCatalog;
                ObjCat.insertCol(Status(Ifile).PT_ARMS.*ones(Nsrc,1), Inf, 'PT_ARMS','mag');
                ObjCat.insertCol(ResAstrometry.ResFit.AssymRMS.*ones(Nsrc,1).*3600, Inf, 'AST_ARMS', 'arcsec');
                ObjCat.insertCol(JD.*ones(Nsrc,1), Inf, 'JD','day');
                ObjCat.insertCol(str2double(FieldID).*ones(Nsrc,1), Inf, 'FieldID');
                ObjCat.insertCol(str2double(CCDID).*ones(Nsrc,1), Inf, 'CCDID');
                ObjCat.insertCol(str2double(CropID).*ones(Nsrc,1), Inf, 'CropID');
                ObjCat.insertCol(FilterInd.*ones(Nsrc,1), Inf, 'Filter');

        Result(Ifile) = ObjCat.copy;
        Status(Ifile).Success = ~Args.RunPhotCalib || Status(Ifile).PhotCalibrated;
        Status(Ifile).RunTime = toc(StartTime);
        if Args.WriteProd
            Status(Ifile).OutputFile = tools.os.relPath2absPath(OutputFile);
            StatusEntry = Status(Ifile);
            % Atomic replacement prevents a partial MAT file from appearing complete.
            TempFile = [tempname(Args.OutputDir) '.mat'];
            RemoveTemp = onCleanup(@() localRemoveTemp(TempFile));
            save(TempFile,'ObjCat','OnlyMP','StatusEntry','H','-v7.3');
            movefile(TempFile,OutputFile,'f');
            clear RemoveTemp;
        end
    end
end

function localRemoveTemp(File)
    if isfile(File)
        delete(File);
    end
end
