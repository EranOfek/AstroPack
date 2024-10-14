function [ObjCat, OnlyMP, AstrometricCat, Status, Path] = reanalize_ZTF_psfcat(File, Args)
    % Given ZTF psfcat apply  photometric ZP, astrometry and match to known asteroids
    % Input  : - FITS table name.
    %          * ...,key,val,...
    %            see code for options
    % Output : - AstroCatalog with updated information.
    %          - AstroCatalog of only minor planets.
    %          - A structure array with Status of reduction.
    %          - Path.
    % Example: OrbEl = celestial.OrbitalEl.loadSolarSystem('merge');
    %          INPOP = celestial.INPOP; INPOP.populateAll;
    %          GeoPosSt = celestial.earth.observatoryCoo('Name','Palomar48');
    %          GeoPos = [GeoPosSt.Lon./RAD, GeoPosSt.Lat./RAD, GeoPosSt.Height];
    %          [Result, AstrometricCat] = reanalize_ZTF_psfcat(File,'Path','/raid/eran/projects/telescopes/ZTF/sourceCatalogs/DAO/649/10','INPOP',INPOP','OrbEl',OrbEl,'GeoPos',GeoPos);
    %          [Result, AstrometricCat] = reanalize_ZTF_psfcat(File,'Path','/raid/eran/projects/telescopes/ZTF/sourceCatalogs/DAO/601/10','INPOP',INPOP','OrbEl',OrbEl,'GeoPos',GeoPos);

    arguments
        File
        Args.Path              = [];
        Args.AstrometricCat    = 'GAIADR3'
        Args.OrbEl             = [];
        Args.INPOP             = [];
        Args.GeoPos            = [];       % [Lon (rad), Lat (rad), Height (m)]. 

        Args.WriteProd logical = true;
        Args.FieldID           = [];
        Args.CCDID             = [];

    end
    RAD = 180./pi;


    PWD = pwd;
    if ~isempty(Args.Path)
        cd(Args.Path);
    end
    Path = pwd;

    if ischar(File)
        ObjCat = FITS.readTable1(File, 'OutTable','AstroCatalog');

        FileSplit = split(File,'_');
        Year  = str2double(FileSplit{2}(1:4));
        Month = str2double(FileSplit{2}(5:6));
        Day   = str2double(FileSplit{2}(7:8));
        Frac  = str2double(FileSplit{2}(9:end));
        Frac  = Frac./(10.^ceil(log10(Frac)));

        JD    = celestial.time.julday([Day Month Year Frac]);
        ObjCat.JD = JD;
    else
        ObjCat = File;
    end

    % Re do astrometry
    SortCat = 'dec';


    TT = ObjCat.copy;
    [ResAstrometry, ObjCat, AstrometricCat] = imProc.astrometry.astrometryRefine(ObjCat, 'WCS',[],...
                            'CatName',Args.AstrometricCat, 'RA',[], 'Dec',[], 'SortCat',SortCat,...
                            'EpochOut',JD);

     if 1==0
        % Comparison between the astromery of ZTF and astrometryRefine
        % astrometryRefine is much better
        %TT = ObjCat.copy;
        R1=imProc.astrometry.astrometryQualityData(TT);  
        R2=imProc.astrometry.astrometryQualityData(ObjCat);
        %
        semilogy(R1.Mag, R1.DeltaDist.*RAD.*3600,'.')
        hold on
        semilogy(R2.Mag, R2.DeltaDist.*RAD.*3600,'.')
    end

    % Add MergedCat
    ObjCat = imProc.match.match_catsHTMmerged(ObjCat);
    

    % Re do photometric calibration - what about g/r/i?
    Att     = split(File,'_');
    Filter  = Att{4};
    switch lower(Filter)
        case 'zg'
            RefColNameMag    = 'phot_bp_mean_mag';
            RefColNameMagErr = 'phot_bp_mean_flux_over_error';
        otherwise
            % zr zi
            RefColNameMag    = 'phot_rp_mean_mag';
            RefColNameMagErr = 'phot_rp_mean_flux_over_error';
    end
    [ObjCat, ZP, PhotCat] = imProc.calib.photometricZP(ObjCat,'UseOnlyMainSeq',false,'Plot',false, 'CreateNewObj',false,...
                                                              'CatColNameMag',{'mag'},'CatColNameMagErr',{'sigmag'},...
                                                              'CatColNameSN','snr',...
                                                              'UpdateMagCols',true,...
                                                              'RefColNameMag',RefColNameMag,...
                                                              'RefColNameMagErr' ,RefColNameMagErr,...
                                                              'MagZP',0,...
                                                              'SignZP',-1,...
                                                              'MagColName2update','mag');
    
    Status.File = File;
    Status.Path = Args.Path;
    Status.AstNgood    = ResAstrometry.ResFit.Ngood;
    Status.AstAssymRMS = ResAstrometry.ResFit.AssymRMS;

    if ZP.Nsrc<5
        % skip file
        
        Status.ZP   = NaN;
        OnlyMP    = [];
        AstCat    = [];

    else
        Status.ZP = ZP;
        % Asteroids search
        [OnlyMP, AstCat, ~] = imProc.match.match2solarSystem(ObjCat, 'JD',JD, 'GeoPos',Args.GeoPos, 'OrbEl',Args.OrbEl, 'SearchRadius',3, 'INPOP',Args.INPOP);
        % add JD to OnlyMP
        Nast = OnlyMP.sizeCatalog;
        if Nast>0
            OnlyMP.insertCol(repmat(OnlyMP.JD, Nast,1), Inf, 'JD', 'day');
        end
    

        ObjCat.UserData.ZP     = ZP;
        ObjCat.UserData.Astrom = ResAstrometry.ResFit;
    
    
        if Args.WriteProd
            FilterList = {'zg','zr','zi'};
    
            % write data products
            FN = FileNames;
            FN.ProjName = 'ZTF';
            FN.Level    = 'proc';
            FN.Product  = 'Cat';
            FN.FieldID  = {Args.FieldID};
            FN.CCDID    = Args.CCDID;
            Att     = split(File,'_');
            FN.Filter  = Att{4};
            FN.CropID  = Att{7}(2);
            FN.Counter = 0;
            FN.Time    = ObjCat.JD;
            FN.FullPath = Args.Path;
    
    
            % add band information to OnlyMP
            FilterInd = find(strcmp(FilterList, FN.Filter{1}));
            Nast = OnlyMP.sizeCatalog;
    
            FieldNumber = str2double(Args.FieldID);
            CCDN        = str2double(Args.CCDID);
    
            insertCol(OnlyMP, FilterInd.*ones(Nast,1), Inf, 'Filter');
            insertCol(OnlyMP, FieldNumber.*ones(Nast,1), Inf, 'FieldID');
            insertCol(OnlyMP, CCDN.*ones(Nast,1), Inf, 'CCDID');
            
    
            Nsrc = ObjCat.sizeCatalog;
            ObjCat.insertCol(ObjCat.UserData.ZP.ZP.*ones(Nsrc,1), Inf, 'ZP','mag');
            ObjCat.insertCol(ObjCat.UserData.ZP.MedC.*ones(Nsrc,1), Inf, 'MedC','mag');
            ObjCat.insertCol(ObjCat.UserData.ZP.Par(2).*ones(Nsrc,1), Inf, 'ColorT','mag/mag');
            ObjCat.insertCol(ObjCat.UserData.ZP.RMS.*ones(Nsrc,1), Inf, 'PhotRMS','mag');
            ObjCat.insertCol(ObjCat.UserData.Astrom.AssymRMS_mag.*ones(Nsrc,1).*3600, Inf, 'AstromRMS','arcsec');
            ObjCat.insertCol(ObjCat.JD.*ones(Nsrc,1), Inf, 'JD','day');

    
            imProc.io.writeProduct(ObjCat, FN, 'GetHeaderJD',false, 'AI_CropID_FromHeader',false, 'AI_Counter_FromHeader',false, 'FullPath',pwd);
    
        end
    end


    if ~isempty(Args.Path)
        cd(PWD);
    end

end
