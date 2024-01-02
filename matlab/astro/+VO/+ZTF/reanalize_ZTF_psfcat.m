function [ObjCat, OnlyMP, AstrometricCat, Skip, Path] = reanalize_ZTF_psfcat(File, Args)
    % Given ZTF psfcatapply  photometric ZP, astrometry and match to known asteroids
    % Input  : - FITS table name.
    %          * ...,key,val,...
    %            see code for options
    % Output : - AstroCatalog with updated information.
    %          - AstroCatalog of only minor planets.
    %          - A structure array with skipped files.
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
        semilogy(R2.Mag, R2.DeltaDist.*RAD.*3600,'o')
    end

    % Add MergedCat
    ObjCat = imProc.match.match_catsHTMmerged(ObjCat);
    

    % Re do photometric calibration - what about g/r/i?
    [ObjCat, ZP, PhotCat] = imProc.calib.photometricZP(ObjCat,'UseOnlyMainSeq',false,'Plot',false, 'CreateNewObj',false,...
                                                              'CatColNameMag',{'mag'},'CatColNameMagErr',{'sigmag'},...
                                                              'CatColNameSN','snr',...
                                                              'UpdateMagCols',true,...
                                                              'MagZP',0,...
                                                              'SignZP',-1,...
                                                              'MagColName2update','mag');
    
    if ZP.Nsrc<5
        % skip file
        Skip.File = File;
        Skip.Path = Args.Path;
        Skip.ZP   = ZP;
        OnlyMP    = [];
        AstCat    = [];

    else
        Skip = [];
        % Asteroids search
        [OnlyMP, AstCat, ~] = imProc.match.match2solarSystem(ObjCat, 'JD',JD, 'GeoPos',Args.GeoPos, 'OrbEl',Args.OrbEl, 'SearchRadius',1, 'INPOP',Args.INPOP);

    end

    if ~isempty(Args.Path)
        cd(PWD);
    end

end
