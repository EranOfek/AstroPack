function [Out, Info] = read_ZTF_cat(File, Args)
    % Read a ZTF per-epoch catalog into a LAST-like AstroCatalog/AstroImage.
    % Package: +VO/+ZTF
    % Description: ZTF ships two catalogs per exposure quadrant, measured on
    %              the same image but with different source lists:
    %                psfcat - PSF-fitting photometry, 13 columns, the basis
    %                         of ZTF's own light curves;
    %                sexcat - SExtractor, 91 columns, aperture photometry in
    %                         six fixed apertures plus morphology.
    %              This reads either, or both cross-matched into one row per
    %              source the way a LAST catalog keeps PSF and aperture
    %              fluxes together, and renames/rescales everything into
    %              AstroPack conventions so the catalog can go straight into
    %              imProc.calib.fitPhotCalibTrans (with 'Mission','ZTF').
    %
    %              What it converts, and why each one matters:
    %              - FLUXERR_* becomes RELATIVE (dF/F). ZTF stores an
    %                absolute error in counts; AstroPack stores dF/F, and
    %                reads SN = 1/FLUXERR and MagErr = 1.086*FLUXERR off it.
    %              - the aperture vectors are expanded into FLUX_APER_<k> /
    %                FLUXERR_APER_<k>, the names calcAperCorr finds by prefix.
    %              - FLUX_APER has only 5 elements while MAG_APER has 6; the
    %                missing flux is recovered from the magnitudes (the
    %                catalog's zero point is 0), exact to ~5e-7.
    %              - SExtractor's +/-99 magnitudes (flux not measurable) are
    %                screened out before that power is taken.
    %              - the flags are translated into a LAST bit mask where a
    %                LAST equivalent exists (see VO.ZTF.flags2LastMask); the
    %                untranslated originals stay as SEX_FLAGS / ZTF_FLAGS.
    %              - the header gets EXPTIME/AIRMASS/JD/CCDSEC plus the
    %                aperture radii, since the files carry no airmass and no
    %                exposure time at all (only the airmass matters: exposure
    %                time and collecting area are degenerate with the fitted
    %                normalization).
    % Input  : - File - path to a psfcat or sexcat FITS file.
    %          * ...,key,val,...
    %            'CatType' - 'auto' (from the file name), 'psfcat', 'sexcat'
    %                   or 'both'. 'both' reads this file and its sibling and
    %                   keeps the cross-matched sources only. Default 'auto'.
    %            'OtherFile' - Explicit path to the sibling catalog for
    %                   'both'. Default '' (same directory, name built with
    %                   VO.ZTF.ztf_prop2filename).
    %            'OutType' - 'AstroImage' (CatData + HeaderData populated, so
    %                   fitPhotCalibTrans(AI) works with no Metadata
    %                   argument) or 'AstroCatalog' (the catalog alone - the
    %                   calibration then needs 'Metadata', Info.Header,
    %                   since the airmass lives in the header).
    %                   Default 'AstroImage'. The object carries no pixel
    %                   data either way: ZTF ships these as catalogs, and
    %                   the AstroImage is only the carrier that lets the
    %                   catalog and its header travel together.
    %            'AirMass' - Airmass of the exposure. Default NaN. The files
    %                   do not carry it; pass it, or give 'Inventory'.
    %            'ExpTime' - Exposure time [s]. Default 30 (ZTF standard).
    %            'Inventory' - IRSA inventory, as a table from
    %                   VO.ZTF.read_ZTF_inventory or a path to the CSV. When
    %                   given, the airmass/exposure time/seeing of this file
    %                   are taken from it. Default [].
    %            'Temp' - Ambient temperature [C] for the header. Default 10.
    %            'Pressure' - Pressure [mbar] for the header. Default 825
    %                   (Palomar, 1712 m).
    %            'KeepFlagged' - Keep rows with a non-zero ZTF/SExtractor
    %                   flag. Default false.
    %            'SNAper' - Which aperture fills the SN column and ZTF's own
    %                   calibrated aperture magnitude. Default 5 (10 pixel
    %                   diameter).
    %            'SNSource' - 'aper' | 'psf', which measurement fills SN when
    %                   both are present. Default 'aper'.
    %            'AddLastFlags' - Add the translated FLAGS column.
    %                   Default true.
    %            'MatchRadius' - Match radius for 'both' [arcsec]. Default 1.
    %            'Naxis' - [NAXIS1 NAXIS2] for CCDSEC. Default [] = the
    %                   quadrant size from VO.ZTF.pixScale.
    % Output : - Out - AstroImage or AstroCatalog, per 'OutType'.
    %          - Info - struct with Header (the AstroHeader built here, for
    %            an 'AstroCatalog' caller to pass as 'Metadata'), MAGZP,
    %            MAGZPRMS, CLRCOEFF, OBSMJD, ObsJD,
    %            FixApers (diameters [pix]), ApRadiusPix, ApRadiusArcsec,
    %            APCOR/APCORUN (sexcat), NsrcIn, Nsrc, AirMass, AirMassSource,
    %            MaxFluxRelDiff (the flux-from-magnitude check) and, for
    %            'both', Nmatched and MatchFrac.
    % Author : D. Kovaleva (Oct 2026)
    % Example: AI = VO.ZTF.read_ZTF_cat(SexFile, 'CatType','both', 'AirMass',1.32);
    %          R  = imProc.calib.fitPhotCalibTrans(AI, 'FluxColName','FLUX_APER_5', ...
    %                   'AperCorrRefFluxCol','FLUX_APER_5', 'RefSpecSlope',0, ...
    %                   'ExtraCalibArgs',{'Mission','ZTF','Band','r','MagRange',[14 19]});

    arguments
        File
        Args.CatType (1,:) char {mustBeMember(Args.CatType, {'auto','psfcat','sexcat','both'})} = 'auto'
        Args.OtherFile (1,:) char = ''
        Args.OutType (1,:) char {mustBeMember(Args.OutType, {'AstroImage','AstroCatalog'})} = 'AstroImage'
        Args.AirMass        = NaN
        Args.ExpTime        = 30
        Args.Inventory      = []
        Args.Temp           = 10
        Args.Pressure       = 825
        Args.KeepFlagged logical = false
        Args.SNAper (1,1) double = 5
        Args.SNSource (1,:) char {mustBeMember(Args.SNSource, {'aper','psf'})} = 'aper'
        Args.AddLastFlags logical = true
        Args.MatchRadius    = 1
        Args.Naxis          = []
    end

    Prop = VO.ZTF.ztf_filename2prop(File);
    [PixScale, NaxisDef] = VO.ZTF.pixScale();
    if isempty(Args.Naxis)
        Args.Naxis = NaxisDef;
    end

    CatType = Args.CatType;
    if strcmp(CatType, 'auto')
        CatType = Prop.FileType;
        if ~ismember(CatType, {'psfcat','sexcat'})
            error('VO:ZTF:read_ZTF_cat:UnknownCatType', ...
                  '%s is a ''%s'' product; pass CatType explicitly', File, Prop.FileType);
        end
    end

    % --- the exposure metadata the files do not carry ------------------
    [AirMass, ExpTime, Seeing, AirMassSource] = localExposureMeta(File, Prop, Args);

    % --- read the requested catalog(s) ---------------------------------
    Info = struct('Band', Prop.Filter(end), 'Field', Prop.Field, 'CCDID', Prop.CCDID, ...
                  'QID', Prop.QuadID, 'RCID', Prop.RCID, 'CatType', CatType);
    switch CatType
        case 'psfcat'
            [Mat, Names, Units, Info] = localReadPsf(File, Args, Info);
        case 'sexcat'
            [Mat, Names, Units, Info] = localReadSex(File, Args, Info);
        case 'both'
            [Mat, Names, Units, Info] = localReadBoth(File, Prop, Args, Info);
        otherwise
            error('VO:ZTF:read_ZTF_cat:UnknownCatType', 'Unknown CatType ''%s''', CatType);
    end

    Info.Nsrc     = size(Mat, 1);
    Info.AirMass  = AirMass;
    Info.AirMassSource = AirMassSource;
    Info.ExpTime  = ExpTime;
    Info.Seeing   = Seeing;
    Info.PixScale = PixScale;
    Info.ApRadiusArcsec = Info.ApRadiusPix .* PixScale;

    Cat = AstroCatalog({Mat}, 'ColNames', Names, 'ColUnits', Units);

    % --- header --------------------------------------------------------
    HeadCell = { ...
        'EXPTIME',  ExpTime,        'Exposure time [s]'; ...
        'AIRMASS',  AirMass,        sprintf('Airmass (%s)', AirMassSource); ...
        'MNTTEMP',  Args.Temp,      'Ambient temperature [C]'; ...
        'PRESSURE', Args.Pressure,  'Pressure [mbar]'; ...
        'NCOADD',   1,              'Number of coadded images'; ...
        'JD',       Info.ObsJD,     'Observation JD (from OBSMJD)'; ...
        'CCDSEC',   sprintf('[1 %d 1 %d]', Args.Naxis(1), Args.Naxis(2)), 'Image section'; ...
        'FILTER',   Info.Band,      'ZTF band (g|r|i)'; ...
        'PIXSCALE', PixScale,       'Plate scale [arcsec/pix]'; ...
        'FIELDID',  Prop.Field,     'ZTF field'; ...
        'CCDID',    Prop.CCDID,     'ZTF CCD'; ...
        'QID',      Prop.QuadID,    'Readout quadrant within the CCD'; ...
        'RCID',     Prop.RCID,      'Readout channel (0-63)'; ...
        'MAGZP',    Info.MAGZP,     'ZTF PSF-fit zero point [mag]'; ...
        'MAGZPRMS', Info.MAGZPRMS,  'RMS of the ZTF zero point [mag]'; ...
        'CLRCOEFF', Info.CLRCOEFF,  'ZTF colour coefficient [mag/mag]'};
    for Iap = 1:numel(Info.ApRadiusPix)
        HeadCell = [HeadCell; { ...
            sprintf('APERRAD%d', Iap), Info.ApRadiusPix(Iap), ...
            sprintf('Aperture %d radius [pix] = %.2f arcsec', Iap, Info.ApRadiusArcsec(Iap))}]; %#ok<AGROW>
    end

    Head = AstroHeader;
    Head.Data = HeadCell;
    % Returned in both modes: an 'AstroCatalog' caller has no HeaderData to
    % carry the airmass, so it passes this straight back in as the
    % 'Metadata' argument of the calibration.
    Info.Header = Head;

    switch Args.OutType
        case 'AstroCatalog'
            Out = Cat;
        otherwise
            Out = AstroImage;
            Out.CatData    = Cat;
            Out.HeaderData = Head;
    end
end

% ----------------------------------------------------------------------
function [AirMass, ExpTime, Seeing, Source] = localExposureMeta(File, Prop, Args)
    % Airmass/exposure time: explicit argument wins, then the inventory.
    AirMass = Args.AirMass;
    ExpTime = Args.ExpTime;
    Seeing  = NaN;
    Source  = 'argument';

    if ~isempty(Args.Inventory)
        Inv = Args.Inventory;
        if ~istable(Inv)
            Inv = VO.ZTF.read_ZTF_inventory(Inv);
        end
        [~, Base, Ext] = fileparts(File);
        Name = [Base, Ext];
        Row  = [];
        for Col = {'PsfcatFile','SexcatFile'}
            if ismember(Col{1}, Inv.Properties.VariableNames)
                Hit = strcmp(string(Inv.(Col{1})), Name);
                if any(Hit)
                    Row = Inv(find(Hit, 1), :);
                end
            end
        end
        if isempty(Row)
            % fall back on the exposure identity rather than the file name
            Hit = Inv.field==Prop.Field & Inv.ccdid==Prop.CCDID & Inv.qid==Prop.QuadID & ...
                  strcmp(string(Inv.filtercode), Prop.Filter) & ...
                  abs(double(Inv.filefracday) - str2double(Prop.FileFracDay)) < 0.5;
            if any(Hit)
                Row = Inv(find(Hit, 1), :);
            end
        end
        if ~isempty(Row)
            if ~isfinite(AirMass) && ismember('airmass', Row.Properties.VariableNames)
                AirMass = Row.airmass;
                Source  = 'inventory';
            end
            if ismember('exptime', Row.Properties.VariableNames)
                ExpTime = Row.exptime;
            end
            if ismember('seeing', Row.Properties.VariableNames)
                Seeing = Row.seeing;
            end
        end
    end
end

% ----------------------------------------------------------------------
function [Mat, Names, Units, Info] = localReadPsf(File, Args, Info)
    % psfcat: PSF-fitting photometry, one flux per source.
    [C, Head] = ztfReadBinTable(File);

    Info.MAGZP    = ztfHeadVal(Head, 'MAGZP');
    Info.MAGZPRMS = ztfHeadVal(Head, 'MAGZPRMS');
    Info.CLRCOEFF = ztfHeadVal(Head, 'CLRCOEFF');
    Info.OBSMJD   = ztfHeadVal(Head, 'OBSMJD');
    Info.ObsJD    = Info.OBSMJD + 2400000.5;
    Info.FixApers    = [];
    Info.ApRadiusPix = [];
    Info.APCOR       = [];
    Info.MaxFluxRelDiff = NaN;
    Info.NsrcIn   = numel(C.ra);

    Ok = isfinite(C.ra) & isfinite(C.dec) & C.flux > 0 & isfinite(C.snr);
    if ~Args.KeepFlagged
        % ZTF's psfcat flags are its own scheme (observed values 1, 16, 256,
        % 4096 and -1) and are NOT documented in the file, so they are used
        % as a yes/no gate and never translated into LAST bits.
        Ok = Ok & C.flags == 0;
    end

    Names = {'RA','Dec','X','Y','FLUX_PSF','FLUXERR_PSF','SN','ZTF_MAG_PSF','ZTF_MAGERR_PSF', ...
             'CHI','SHARP','ZTF_FLAGS'};
    Units = {'deg','deg','pix','pix','count','','','mag','mag','','',''};
    Mat   = [C.ra, C.dec, C.xpos, C.ypos, C.flux, C.sigflux./C.flux, C.snr, ...
             C.mag + Info.MAGZP, C.sigmag, C.chi, C.sharp, C.flags];
    Mat   = Mat(Ok, :);
end

% ----------------------------------------------------------------------
function [Mat, Names, Units, Info] = localReadSex(File, Args, Info)
    % sexcat: SExtractor, aperture photometry in vector columns.
    [C, Head] = ztfReadBinTable(File);

    Info.MAGZP    = ztfHeadVal(Head, 'MAGZP');
    Info.MAGZPRMS = ztfHeadVal(Head, 'MAGZPRMS');
    Info.CLRCOEFF = ztfHeadVal(Head, 'CLRCOEFF');
    Info.OBSMJD   = ztfHeadVal(Head, 'OBSMJD');
    Info.ObsJD    = Info.OBSMJD + 2400000.5;

    % FIXAPERS lists aperture DIAMETERS in pixels.
    Info.FixApers    = sscanf(strtrim(char(string(ztfHeadVal(Head, 'FIXAPERS', '')))), '%g,').';
    Naper            = size(C.MAG_APER, 2);
    if numel(Info.FixApers) ~= Naper
        Info.FixApers = NaN(1, Naper);
    end
    Info.ApRadiusPix = Info.FixApers ./ 2;
    Info.APCOR       = NaN(1, Naper);
    Info.APCORUN     = NaN(1, Naper);
    for Iap = 1:Naper
        Info.APCOR(Iap)   = ztfHeadVal(Head, sprintf('APCOR%d', Iap));
        Info.APCORUN(Iap) = ztfHeadVal(Head, sprintf('APCORUN%d', Iap));
    end

    SNAper = min(Args.SNAper, Naper);

    % Fluxes from the magnitudes: the catalog's zero point is 0, and this is
    % the only route to the last aperture, whose FLUX_APER element ZTF omits.
    Flux  = 10.^(-0.4.*C.MAG_APER);
    Info.NsrcIn = size(C.MAG_APER, 1);

    Ok = isfinite(C.ALPHAWIN_J2000) & isfinite(C.DELTAWIN_J2000) & ...
         all(isfinite(C.MAG_APER), 2) & all(abs(C.MAG_APER) < 50, 2) & ...
         all(Flux > 0, 2) & all(isfinite(C.FLUXERR_APER), 2);
    if ~Args.KeepFlagged
        Ok = Ok & C.FLAGS == 0;
    end

    % consistency of the two representations where both exist
    if isfield(C, 'FLUX_APER')
        Ncol = min(size(C.FLUX_APER,2), Naper);
        Info.MaxFluxRelDiff = max(abs(C.FLUX_APER(Ok,1:Ncol) - Flux(Ok,1:Ncol)) ./ ...
                                  C.FLUX_APER(Ok,1:Ncol), [], 'all');
    else
        Info.MaxFluxRelDiff = NaN;
    end

    RelErr = C.FLUXERR_APER ./ Flux;

    Names = {'RA','Dec','X','Y'};
    Units = {'deg','deg','pix','pix'};
    Mat   = [C.ALPHAWIN_J2000, C.DELTAWIN_J2000, C.XWIN_IMAGE, C.YWIN_IMAGE];
    for Iap = 1:Naper
        Names = [Names, {sprintf('FLUX_APER_%d', Iap)}, {sprintf('FLUXERR_APER_%d', Iap)}]; %#ok<AGROW>
        Units = [Units, {'count'}, {''}];                                                   %#ok<AGROW>
        Mat   = [Mat, Flux(:,Iap), RelErr(:,Iap)];                                          %#ok<AGROW>
    end

    Names = [Names, {'SN','ZTF_MAG_APER','ZTF_MAGERR_APER','SEX_FLAGS','CLASS_STAR', ...
                     'FWHM','BACK_IM','X2','Y2','XY','ELONGATION'}];
    Units = [Units, {'','mag','mag','','','pix','count','pix^2','pix^2','pix^2',''}];
    Mat   = [Mat, 1./RelErr(:,SNAper), ...
                  C.MAG_APER(:,SNAper) + Info.MAGZP + Info.APCOR(SNAper), ...
                  C.MAGERR_APER(:,SNAper), C.FLAGS, C.CLASS_STAR, C.FWHM_IMAGE, ...
                  C.BACKGROUND, C.X2WIN_IMAGE, C.Y2WIN_IMAGE, C.XYWIN_IMAGE, C.ELONGATION];

    if Args.AddLastFlags
        Names = [Names, {'FLAGS'}];
        Units = [Units, {''}];
        Mat   = [Mat, double(VO.ZTF.flags2LastMask(C.FLAGS))];
    end

    Mat = Mat(Ok, :);
    Info.SNAper = SNAper;
end

% ----------------------------------------------------------------------
function [Mat, Names, Units, Info] = localReadBoth(File, Prop, Args, Info)
    % Both catalogs of the same exposure, cross-matched into one row per
    % source - the way a LAST catalog keeps PSF and aperture fluxes together.
    [Dir, ~, ~] = fileparts(File);
    if isempty(Args.OtherFile)
        switch Prop.FileType
            case 'sexcat'
                SexFile = File;
                PsfFile = VO.ZTF.ztf_prop2filename(Prop, 'psfcat', 'Dir', Dir);
            otherwise
                PsfFile = File;
                SexFile = VO.ZTF.ztf_prop2filename(Prop, 'sexcat', 'Dir', Dir);
        end
    else
        if strcmp(Prop.FileType, 'sexcat')
            SexFile = File;  PsfFile = Args.OtherFile;
        else
            PsfFile = File;  SexFile = Args.OtherFile;
        end
    end
    if ~isfile(PsfFile) || ~isfile(SexFile)
        error('VO:ZTF:read_ZTF_cat:SiblingMissing', ...
              'CatType=''both'' needs both products; missing %s', ...
              strjoin({PsfFile, SexFile}, ' or '));
    end

    InfoS = Info;  InfoP = Info;
    [MatS, NamesS, UnitsS, InfoS] = localReadSex(SexFile, Args, InfoS);
    [MatP, NamesP, UnitsP, InfoP] = localReadPsf(PsfFile, Args, InfoP);

    CatS = AstroCatalog({MatS}, 'ColNames', NamesS, 'ColUnits', UnitsS);
    CatP = AstroCatalog({MatP}, 'ColNames', NamesP, 'ColUnits', UnitsP);

    % matchReturnIndices sorts its FIRST argument in place, so read both
    % catalogs back from the objects afterwards rather than from MatS/MatP.
    ResM = imProc.match.matchReturnIndices(CatP, CatS, 'Radius', Args.MatchRadius, ...
                                           'RadiusUnits', 'arcsec', 'CooType', 'sphere');
    IndP = ResM.Obj2_IndInObj1;
    Good = isfinite(IndP);

    SexAll = CatS.Catalog;
    PsfAll = CatP.Catalog;
    MatS   = SexAll(Good, :);
    MatP   = PsfAll(IndP(Good), :);
    Dist   = ResM.Obj2_Dist(Good) .* (180./pi) .* 3600;   % rad -> arcsec

    % sexcat gives the position and the apertures; psfcat adds its own flux.
    TakeP  = {'FLUX_PSF','FLUXERR_PSF','ZTF_MAG_PSF','ZTF_MAGERR_PSF','CHI','SHARP','ZTF_FLAGS'};
    [~, IdxP] = ismember(TakeP, NamesP);
    UnitsPsel = UnitsP(IdxP);

    Names = [NamesS, TakeP, {'DIST_PSF_APER'}];
    Units = [UnitsS, UnitsPsel, {'arcsec'}];
    Mat   = [MatS, MatP(:, IdxP), Dist(:)];

    if strcmp(Args.SNSource, 'psf')
        IsSN   = strcmp(Names, 'SN');
        IsSNps = strcmp(NamesP, 'FLUXERR_PSF');
        Mat(:, IsSN) = 1 ./ MatP(:, IsSNps);
    end

    Info = InfoS;
    Info.MAGZP_PSF = InfoP.MAGZP;
    Info.NsrcIn    = InfoS.NsrcIn;
    Info.NsrcPsf   = size(PsfAll, 1);
    Info.NsrcSex   = size(SexAll, 1);
    Info.Nmatched  = sum(Good);
    Info.MatchFrac = sum(Good) ./ max(size(SexAll,1), 1);
    Info.CatType   = 'both';
end
