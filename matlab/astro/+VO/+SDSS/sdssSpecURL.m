function UrlFits = sdssSpecURL(Res, Args)
    % Generate SDSS spectrum FITS URLs (DR10–DR19, legacy or BOSS/eBOSS)
    % Input  : - Table which is the output of the SDSS SQL query.
    %            The table must contain (at least) the columns:
    %            plate, mjd, fiberid, run2d.
    %            Optionally survey and/or programname are used to decide
    %            between legacy and BOSS/eBOSS locations.
    %          * ...,key,val,...
    %            'Type' - 'lite'|'full'. Default is 'lite'.
    % Output : - String array of URLs to the FITS spectra.
    % Author : ChatGPT + Eran Ofek (Dec 2025)
    % Example: Url = VO.SDSS.sdssSpecURL(T)
    %
    % Q = ['SELECT TOP 1 ra, dec, plate, mjd, fiberid, survey, run2d, ' ...
    % 'sqrt((ra-150.11475)*(ra-150.11475) + (dec-2.205833)*(dec-2.205833)) AS dist_deg ' ...
    % 'FROM sdss_dr16.specobjall ' ...
    % 'WHERE ra BETWEEN 150.11475-0.1 AND 150.11475+0.1 ' ...
    % 'AND dec BETWEEN 2.205833-0.1 AND 2.205833+0.1 ' ...
    % 'ORDER BY dist_deg'];
    % T   = Tap.query(Q,'TapUrl','https://datalab.noirlab.edu/tap','Ofmt','csv','TimeoutSec',120);
    % Url = VO.SDSS.sdssSpecURL(T)

    arguments
        Res  table
        Args.Type (1,1) string = "lite"
    end

    TypeStr = lower(strtrim(Args.Type));
    if ~(TypeStr=="lite" || TypeStr=="full")
        error('Type must be "lite" or "full".');
    end

    % --- Required columns ------------------------------------------------
    NeedCols = {'plate','mjd','fiberid','run2d'};
    if ~all(ismember(NeedCols, Res.Properties.VariableNames))
        error('Input table must contain the columns: plate, mjd, fiberid, run2d.');
    end

    Plate = Res.plate(:);
    Mjd   = Res.mjd(:);
    Fiber = Res.fiberid(:);
    Run2d = string(Res.run2d(:));

    N = numel(Plate);
    if ~(numel(Mjd)==N && numel(Fiber)==N && numel(Run2d)==N)
        error('Columns plate, mjd, fiberid, run2d must have the same length.');
    end

    % --- Optional survey / programname info ------------------------------
    HasSurvey      = ismember('survey',      Res.Properties.VariableNames);
    HasProgramName = ismember('programname', Res.Properties.VariableNames);

    if HasSurvey
        SurveyCol = lower(string(Res.survey(:)));
    else
        SurveyCol = repmat("", N, 1);
    end

    if HasProgramName
        ProgramCol = lower(string(Res.programname(:)));
    else
        ProgramCol = repmat("", N, 1);
    end

    % --- Host & DR choice ------------------------------------------------
    % DR18/19 serve old spectra from DR18 "prior-surveys" trees.
    Host  = "https://dr18.sdss.org";
    SasDr = 18;

    UrlFits = strings(N,1);

    for I = 1:N
        PlateVal   = Plate(I);
        MjdVal     = Mjd(I);
        FiberVal   = Fiber(I);
        Run2dStr   = strtrim(Run2d(I));
        SurveyStr  = SurveyCol(I);
        ProgramStr = ProgramCol(I);

        % --- Decide if this is BOSS/eBOSS vs legacy ---------------------
        IsBoss = false;
        if SurveyStr ~= "" && (contains(SurveyStr,"boss") || contains(SurveyStr,"eboss"))
            IsBoss = true;
        elseif ProgramStr ~= "" && (contains(ProgramStr,"boss") || contains(ProgramStr,"eboss"))
            IsBoss = true;
        end

        % --- Normalize run2d --------------------------------------------
        if Run2dStr=="" || ismissing(Run2dStr)
            if IsBoss
                Run2dStr = "v5_13_2";   % DR18 eBOSS/BOSS default redux
            else
                Run2dStr = "26";        % typical SDSS-I/II redux
            end
        end

        % Map v5_13_0 → v5_13_2 in DR18 prior-surveys for BOSS/eBOSS
        if IsBoss && SasDr==18
            if Run2dStr=="v5_13_0" || Run2dStr=="v5_13"
                Run2dStr = "v5_13_2";
            end
        end

        % --- Plate & fiber strings --------------------------------------
        % Legacy (sdss2-dr8-sdss):
        %   if plate < 1000 → pad (e.g., 0350/spec-0350-...)
        %   else            → no pad (e.g., 3586/spec-3586-...)
        % BOSS/eBOSS (sdss4-dr17-eboss):
        %   always pad to 4 digits.
        if IsBoss
            PlateDirStr  = sprintf('%04d', PlateVal);
            PlateFileStr = sprintf('%04d', PlateVal);
        else
            if PlateVal < 1000
                PlateDirStr  = sprintf('%04d', PlateVal);
                PlateFileStr = sprintf('%04d', PlateVal);
            else
                PlateDirStr  = string(PlateVal);
                PlateFileStr = string(PlateVal);
            end
        end

        FiberStr = sprintf('%04d', FiberVal);   % always 4-digit fiber

        % --- Build base URL ---------------------------------------------
        if IsBoss
            % BOSS/eBOSS in DR18 prior-surveys:
            % .../sdss4-dr17-eboss/spectro/redux/RUN2D/spectra/<Type>/PLATE/spec-PLATE-MJD-FIBER.fits
            Base = Host + "/sas/dr" + string(SasDr) + ...
                   "/prior-surveys/sdss4-dr17-eboss/spectro/redux/" + Run2dStr + ...
                   "/spectra/" + TypeStr + "/";
        else
            % Legacy in DR18 prior-surveys:
            % .../sdss2-dr8-sdss/spectro/redux/RUN2D/spectra/PLATE/spec-PLATE-MJD-FIBER.fits 
            Base = Host + "/sas/dr" + string(SasDr) + ...
                   "/prior-surveys/sdss2-dr8-sdss/spectro/redux/" + Run2dStr + ...
                   "/spectra/";
        end

        % --- Final URL --------------------------------------------------
        UrlFits(I) = Base + PlateDirStr + "/" + ...
                     "spec-" + PlateFileStr + "-" + string(MjdVal) + "-" + FiberStr + ".fits";
    end
end
