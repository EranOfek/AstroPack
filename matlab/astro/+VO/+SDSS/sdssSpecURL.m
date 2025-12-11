function UrlFits = sdssSpecURL(Plate, Mjd, Fiber, Args)
    % Generate SDSS spectrum FITS URLs (DR10–DR19, legacy or BOSS/eBOSS)
    % Input  : - Plate number
    %          - Mjd
    %          - Fiber ID (0-padded to 4 digits)
    %          * ...,key,val,...
    %            'Survey' - 'legacy'|'boss'. Default is 'legacy'.
    %            'Dr' - 10-19. Default is 19.
    %            'Redux' - Default is "".
    % Output : - String array of URLs to the FITS spectra.
    % Author : ChatGPT + Eran Ofek (Dec 2025)
    % Example: Url = VO.SDSS.sdssSpecURL(Plate, MJD, Fiber)
    %
    % Q = ['SELECT TOP 1 ra, dec, plate, mjd, fiberid, ' ...
    % 'sqrt((ra-150.11475)*(ra-150.11475) + (dec-2.205833)*(dec-2.205833)) AS dist_deg ' ...
    % 'FROM sdss_dr16.specobjall ' ...
    % 'WHERE ra BETWEEN 150.11475-0.1 AND 150.11475+0.1 ' ...
    % 'AND dec BETWEEN 2.205833-0.1 AND 2.205833+0.1 ' ...
    % 'ORDER BY dist_deg'];
    % T = Tap.query(Q,'TapUrl','https://datalab.noirlab.edu/tap','Ofmt','csv','TimeoutSec',120);
    % Url = VO.SDSS.sdssSpecURL(T.plate, T.mjd, T.fiberid)

    arguments
        Plate (:,1) double
        Mjd   (:,1) double
        Fiber (:,1) double
        Args.Survey (1,1) string = "legacy"
        Args.Dr     (1,1) double = 19
        Args.Redux  (1,1) string = ""
    end

    % Sanity check: all vectors must match in length
    if ~(numel(Plate) == numel(Mjd) && numel(Mjd) == numel(Fiber))
        error("Plate, Mjd, and Fiber must have the same length.");
    end

    % Normalize internal strings
    Args.Survey = lower(Args.Survey);

    % ---- Determine host -------------------------------------------------
    if Args.Dr >= 18
        Host = "https://dr" + string(Args.Dr) + ".sdss.org";
    else
        Host = "https://data.sdss.org";
    end

    % ---- Determine base path --------------------------------------------
    switch Args.Survey

        % === SDSS-I/II Legacy Spectra ====================================
        case {"legacy", "sdss"}
            if Args.Dr >= 18
                % DR18/DR19 prior-surveys location
                Base = Host + "/sas/dr" + string(Args.Dr) + ...
                       "/prior-surveys/sdss2-dr8-sdss/spectro/redux/26/spectra/";
            else
                % DR10–DR16 style
                Base = Host + "/sas/dr" + string(Args.Dr) + ...
                       "/sdss/spectro/redux/26/spectra/";
            end

        % === BOSS/eBOSS Spectra =========================================
        case {"boss", "eboss"}
            % Default Redux version if not provided
            if Args.Redux == ""
                if Dr <= 16
                    Args.Redux = "v5_13_0";   % DR16 default (official)
                else
                    Args.Redux = "v5_13_2";   % DR18/DR19 prior-surveys default
                end
            end

            if Dr >= 18
                Base = Host + "/sas/dr" + string(Args.Dr) + ...
                       "/prior-surveys/sdss4-dr17-eboss/spectro/redux/" + Args.Redux + "/spectra/";
            else
                Base = Host + "/sas/dr" + string(Args.Dr) + ...
                       "/eboss/spectro/redux/" + Args.Redux + "/spectra/";
            end

        otherwise
            error("Survey must be ""legacy"", ""sdss"", ""boss"", or ""eboss"".");
    end

    % ---- Build URLs -----------------------------------------------------
    N = numel(Plate);
    UrlFits = strings(N,1);

    for I = 1:N
        FiberStr = sprintf('%04d', Fiber(I));
        PlateStr = sprintf('%04d', Plate(I));

        UrlFits(I) = Base + ...
                     string(PlateStr) + "/" + ...
                     "spec-" + string(PlateStr) + "-" + string(Mjd(I)) + "-" + FiberStr + ".fits";
    end

end
