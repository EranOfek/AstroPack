function [T, Info] = harvestNight(Args)
    % Collect per-crop quality metrics from all coadd headers of one observing night.
    %   Walks every camera directory under a LAST data root, reads the visit-coadd
    %   image headers of the requested night, and returns one table row per crop.
    %   The table is the input of pipeline.last.quality.nightReport; harvesting is
    %   separated from rendering so that a report can be re-rendered without
    %   re-reading thousands of headers.
    %
    %   Metrics with no keyword in a given product are returned as NaN (the
    %   blank<->NaN convention of issue #1252), so a missing keyword never
    %   aborts the walk.
    %
    % Input  : * ...,key,val,...
    %            'BasePath' - Data root holding the per-camera trees.
    %                   Default is '/bigdata3/projects/last/data'.
    %            'Night' - Night to harvest, as 'YYYY-MM-DD' or 'YYYYMMDD'.
    %                   The directory layout is <BasePath>/<Camera>/YYYY/MM/DD/proc.
    %                   Default is '' (the most recent night found under BasePath).
    %            'CameraTemplate' - Camera directory template.
    %                   Default is 'LAST.*'.
    %            'Keys' - Header keywords to extract. Each becomes a numeric
    %                   table column of the same name (non-numeric values -> NaN).
    %                   Default is the standard night-report set.
    %            'StrKeys' - Header keywords to extract as strings.
    %                   Default is {'FIELDID'}.
    %            'ProductTemplate' - File template of the product to read.
    %                   Default is '*coadd_Image_1.fits'.
    %            'HDU' - HDU holding the header. Default is 1.
    %            'CropsPerVisit' - Expected crops per visit, used to report
    %                   missing products. Default is 24.
    %            'CacheFile' - If not empty, save T and Info to this .mat file.
    %                   Default is ''.
    %            'Verbose' - Print progress. Default is true.
    % Output : - A table with one row per crop: Camera, Visit, CropID, plus one
    %            column per requested keyword.
    %          - A structure with night-level bookkeeping: Night, BasePath,
    %            Ncam, Nvisit, Ncrop, NcropExpected, TimeSpanUT, CadenceSec,
    %            and the harvest duration.
    % Author : D. Kovaleva (Oct 2026)
    % Example: T = pipeline.last.quality.harvestNight('Night','2026-10-04');
    %          [T,Info] = pipeline.last.quality.harvestNight('Night','2026-10-04',...
    %                        'CacheFile','/tmp/night_20261004.mat');

    arguments
        Args.BasePath char        = '/bigdata3/projects/last/data'
        Args.Night                = ''
        Args.CameraTemplate char  = 'LAST.*'
        Args.Keys cell            = {'AIRMASS','FWHM','LIMMAG','BACKMAG', ...
                                     'AST_ARMS','AST_ERRM','AST_NSRC','N_STARS', ...
                                     'PT_ZP','PT_ARMS','PT_RMS','PT_NCALI','PT_DOF', ...
                                     'PT_CHI2','PT_CTA','APC0_PS','RP_MRMS','PH_RMS', ...
                                     'MED_X2','MED_Y2','MED_XY','FOCUS','MNTTEMP', ...
                                     'CAMTEMP','MIDJD','EXPTIME','NCOADD','CROPID'}
        Args.StrKeys cell         = {'FIELDID'}
        Args.ProductTemplate char = '*coadd_Image_1.fits'
        Args.HDU (1,1) double     = 1
        Args.CropsPerVisit (1,1) double = 24
        Args.CacheFile char       = ''
        Args.Verbose logical      = true
    end

    Tstart = tic;

    % --- resolve the night into YYYY/MM/DD path components
    CamDir = dir(fullfile(Args.BasePath, Args.CameraTemplate));
    CamDir = CamDir([CamDir.isdir]);
    if isempty(CamDir)
        error('pipeline:last:quality:harvestNight:NoCameras', ...
              'No camera directories matching %s under %s.', Args.CameraTemplate, Args.BasePath);
    end
    CamNames = {CamDir.name};

    if isempty(Args.Night)
        NightStr = latestNight(Args.BasePath, CamNames);
        if isempty(NightStr)
            error('pipeline:last:quality:harvestNight:NoNights', ...
                  'Could not find any night directory under %s.', Args.BasePath);
        end
    else
        NightStr = char(Args.Night);
    end
    Digits = NightStr(isstrprop(NightStr, 'digit'));
    if numel(Digits) ~= 8
        error('pipeline:last:quality:harvestNight:BadNight', ...
              'Night must carry 8 digits (YYYYMMDD or YYYY-MM-DD), got "%s".', NightStr);
    end
    Ystr = Digits(1:4);  Mstr = Digits(5:6);  Dstr = Digits(7:8);
    NightStr = [Ystr '-' Mstr '-' Dstr];

    % --- walk the cameras
    Nkey    = numel(Args.Keys);
    Nstr    = numel(Args.StrKeys);
    AllCam  = {};
    AllVis  = {};
    AllFile = {};
    for Icam = 1:numel(CamNames)
        VisDir = fullfile(Args.BasePath, CamNames{Icam}, Ystr, Mstr, Dstr, 'proc');
        if isfolder(VisDir)
            Files = dir(fullfile(VisDir, '*v1', Args.ProductTemplate));
            if ~isempty(Files)
                Nf = numel(Files);
                AllCam(end+1:end+Nf,1)  = CamNames(Icam);                       %#ok<AGROW>
                AllFile(end+1:end+Nf,1) = fullfile({Files.folder}, {Files.name}); %#ok<AGROW>
                % visit = the timestamp field of the file name
                Tok = regexp({Files.name}, '_(\d{8}\.\d{6}\.\d{3})_', 'tokens', 'once');
                Vis = cellfun(@(c) char(string(c)), Tok, 'UniformOutput', false);
                AllVis(end+1:end+Nf,1) = Vis;                                   %#ok<AGROW>
            end
        end
    end

    Ncrop = numel(AllFile);
    if Ncrop == 0
        error('pipeline:last:quality:harvestNight:NoProducts', ...
              'No %s found for night %s under %s.', Args.ProductTemplate, NightStr, Args.BasePath);
    end
    if Args.Verbose
        fprintf('harvestNight: %s - %d crops from %d cameras\n', ...
                NightStr, Ncrop, numel(unique(AllCam)));
    end

    % --- read the headers
    Val    = nan(Ncrop, Nkey);
    StrVal = repmat({''}, Ncrop, Nstr);
    Step   = max(1, floor(Ncrop/10));
    for Icrop = 1:Ncrop
        try
            H = AstroHeader(AllFile{Icrop}, Args.HDU);
            for Ikey = 1:Nkey
                V = H.getVal(Args.Keys{Ikey});
                if isnumeric(V) && isscalar(V)
                    Val(Icrop, Ikey) = V;
                elseif islogical(V) && isscalar(V)
                    Val(Icrop, Ikey) = double(V);
                end
            end
            for Istr = 1:Nstr
                V = H.getVal(Args.StrKeys{Istr});
                % FIELDID and friends are numeric whenever the field name is a
                % bare number (e.g. 977), so coerce rather than drop the value.
                if ischar(V) || isstring(V)
                    StrVal{Icrop, Istr} = char(V);
                elseif isnumeric(V) && isscalar(V) && isfinite(V)
                    StrVal{Icrop, Istr} = num2str(V);
                end
            end
        catch ME
            if Args.Verbose && mod(Icrop, Step) == 0
                fprintf('  (unreadable header: %s)\n', ME.identifier);
            end
        end
        if Args.Verbose && mod(Icrop, Step) == 0
            fprintf('  %5d / %d\n', Icrop, Ncrop);
        end
    end

    % --- assemble the table
    T = table(string(AllCam), string(AllVis), 'VariableNames', {'Camera','Visit'});
    for Ikey = 1:Nkey
        T.(matlab.lang.makeValidName(Args.Keys{Ikey})) = Val(:, Ikey);
    end
    for Istr = 1:Nstr
        T.(matlab.lang.makeValidName(Args.StrKeys{Istr})) = string(StrVal(:, Istr));
    end

    % --- night-level bookkeeping
    VisitKey = strcat(T.Camera, '|', T.Visit);
    Nvisit   = numel(unique(VisitKey));
    Info             = struct;
    Info.Night       = NightStr;
    Info.BasePath    = Args.BasePath;
    Info.Ncam        = numel(unique(T.Camera));
    Info.Nvisit      = Nvisit;
    Info.Ncrop       = Ncrop;
    Info.NcropExpect = Nvisit * Args.CropsPerVisit;
    Info.HarvestSec  = toc(Tstart);
    Info.Keys        = Args.Keys;

    % time span and cadence from the visit mid-times
    if ismember('MIDJD', T.Properties.VariableNames)
        [~, Ifirst] = unique(VisitKey, 'stable');
        VisJD = sort(T.MIDJD(Ifirst));
        VisJD = VisJD(isfinite(VisJD));
        if numel(VisJD) > 1
            UT = mod(VisJD - 0.5, 1) * 24;
            Info.TimeSpanUT = [min(UT), max(UT)];
            Info.CadenceSec = median(diff(VisJD)) * 86400;
        else
            Info.TimeSpanUT = [NaN NaN];
            Info.CadenceSec = NaN;
        end
    else
        Info.TimeSpanUT = [NaN NaN];
        Info.CadenceSec = NaN;
    end

    if ~isempty(Args.CacheFile)
        save(Args.CacheFile, 'T', 'Info', '-v7.3');
        if Args.Verbose
            fprintf('harvestNight: cached to %s\n', Args.CacheFile);
        end
    end
    if Args.Verbose
        fprintf('harvestNight: done in %.0f s (%d visits, %d/%d crops)\n', ...
                Info.HarvestSec, Info.Nvisit, Info.Ncrop, Info.NcropExpect);
    end
end

function NightStr = latestNight(BasePath, CamNames)
    % Most recent YYYY/MM/DD directory holding a proc/ subdirectory.
    NightStr = '';
    Best     = '';
    for Icam = 1:numel(CamNames)
        Yr = dir(fullfile(BasePath, CamNames{Icam}, '[12][0-9][0-9][0-9]'));
        Yr = Yr([Yr.isdir]);
        for Iy = 1:numel(Yr)
            Mo = dir(fullfile(Yr(Iy).folder, Yr(Iy).name, '[0-1][0-9]'));
            Mo = Mo([Mo.isdir]);
            for Im = 1:numel(Mo)
                Dy = dir(fullfile(Mo(Im).folder, Mo(Im).name, '[0-3][0-9]'));
                Dy = Dy([Dy.isdir]);
                for Id = 1:numel(Dy)
                    Cand = [Yr(Iy).name Mo(Im).name Dy(Id).name];
                    if isfolder(fullfile(Dy(Id).folder, Dy(Id).name, 'proc')) && ...
                            (isempty(Best) || str2double(Cand) > str2double(Best))
                        Best = Cand;
                    end
                end
            end
        end
    end
    if ~isempty(Best)
        NightStr = Best;
    end
end
