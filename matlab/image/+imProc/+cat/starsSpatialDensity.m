function [Result, Obj] = starsSpatialDensity(Obj, Args)
    % Spatial-density quantiles of the detected sources over a 2D grid.
    %       Bins the catalog's X/Y positions into an NbinX x NbinY grid over
    %       the image area and reports quantiles of the per-bin source
    %       counts. A smooth field gives quantiles close to each other; a
    %       localised overdensity (an artifact clump, e.g. the ghost haloes
    %       of issue #1274, or a genuinely crowded region) raises the upper
    %       quantiles relative to the median.
    %       Raw counts are reported - not ratios - so that any derived
    %       measure can be formed later from the header values.
    %       The quantiles are written to the header as SPATQ<pp> (e.g.
    %       SPATQ50, SPATQ75, SPATQ90, SPATQ95), together with SPATNBIN
    %       (number of grid cells), without which the per-bin counts cannot
    %       be compared between images binned on different grids. The
    %       number of sources is not written: the total is already in the
    %       header as N_STARS, and it is returned in Result.Nsrc (it
    %       differs from N_STARS only when MinSN is used).
    % Input  : - An AstroImage or AstroCatalog object (array supported).
    %          * ...,key,val,...
    %            'Nbin' - Grid size; scalar for a square grid, or
    %                   [NbinX NbinY]. Default is 12 (i.e. 12x12; for a
    %                   1716 pix LAST crop one bin is ~3 arcmin).
    %            'CCDSEC' - [Xmin Xmax Ymin Ymax] area to bin over.
    %                   If empty, taken from the image size, else from the
    %                   NAXIS1/NAXIS2 header keywords, else from the
    %                   maximal X/Y in the catalog.
    %                   Default is [].
    %            'ColX' - X column name. Default is 'X'.
    %            'ColY' - Y column name. Default is 'Y'.
    %            'Quantiles' - Quantiles of the per-bin counts to report.
    %                   Default is [0.50 0.75 0.90 0.95].
    %            'KeyPrefix' - Header keyword prefix; the keyword is
    %                   [KeyPrefix sprintf('%02d', 100*Quantile)].
    %                   Default is 'SPATQ'.
    %            'MinSN' - If not empty, bin only sources with S/N above
    %                   this value. Default is [] (bin all detections -
    %                   note that artifact clumps consist mostly of
    %                   marginal detections, so an S/N cut suppresses
    %                   exactly the signal this statistic is meant to show).
    %            'ColSN' - S/N column name, used only when MinSN is set.
    %                   Default is 'SN'.
    %            'UpdateHeader' - Write the keywords to the header
    %                   (AstroImage input only). Default is true.
    %            'ReturnCounts' - Also return the per-bin count matrix in
    %                   Result.Counts. Default is false.
    % Output : - A structure array (element per input object) with fields:
    %            .Quantiles - The quantile levels used.
    %            .Value     - The per-bin counts at those quantiles.
    %            .KeyNames  - The corresponding header keyword names.
    %            .Nsrc      - Number of sources binned.
    %            .Nbin      - [NbinX NbinY].
    %            .Counts    - Per-bin counts (only if ReturnCounts).
    %          - The input object; for AstroImage input with UpdateHeader
    %            the headers are populated.
    % Author : Dana Kovaleva (Sep 2026), issue #1274
    % Example: [Res, AI] = imProc.cat.starsSpatialDensity(AI);
    %          Res = imProc.cat.starsSpatialDensity(AC, 'Nbin',[12 12]);

    arguments
        Obj
        Args.Nbin                 = 12;
        Args.CCDSEC               = [];
        Args.ColX  (1,:) char     = 'X';
        Args.ColY  (1,:) char     = 'Y';
        Args.Quantiles            = [0.50 0.75 0.90 0.95];
        Args.KeyPrefix (1,:) char = 'SPATQ';
        Args.MinSN                = [];
        Args.ColSN (1,:) char     = 'SN';
        Args.UpdateHeader logical = true;
        Args.ReturnCounts logical = false;
    end

    IsAstroImage = isa(Obj, 'AstroImage');

    Nbin = Args.Nbin;
    if isscalar(Nbin)
        Nbin = [Nbin Nbin];
    end
    Nq       = numel(Args.Quantiles);
    KeyNames = cell(1, Nq);
    for Iq = 1:Nq
        KeyNames{Iq} = sprintf('%s%02d', Args.KeyPrefix, round(100.*Args.Quantiles(Iq)));
    end

    Nobj   = numel(Obj);
    Result = struct('Quantiles',cell(size(Obj)), 'Value',[], 'KeyNames',[], ...
                    'Nsrc',[], 'Nbin',[], 'Counts',[]);

    for Iobj = 1:Nobj
        if IsAstroImage
            Cat = Obj(Iobj).CatData;
        else
            Cat = Obj(Iobj);
        end

        Val  = nan(1, Nq);
        Nsrc = 0;
        Cnt  = [];

        if ~isemptyCatalog(Cat)
            X = getColOrEmpty(Cat, Args.ColX);
            Y = getColOrEmpty(Cat, Args.ColY);
            if ~isempty(X) && ~isempty(Y)
                Flag = isfinite(X) & isfinite(Y);
                if ~isempty(Args.MinSN)
                    SN = getColOrEmpty(Cat, Args.ColSN);
                    if ~isempty(SN)
                        Flag = Flag & SN > Args.MinSN;
                    end
                end
                X = X(Flag);
                Y = Y(Flag);
                Nsrc = numel(X);

                if Nsrc > 0
                    CCDSEC = resolveCCDSEC(Args.CCDSEC, Obj, Iobj, IsAstroImage, X, Y);
                    EdgeX  = linspace(CCDSEC(1), CCDSEC(2)+1, Nbin(1)+1);
                    EdgeY  = linspace(CCDSEC(3), CCDSEC(4)+1, Nbin(2)+1);
                    Cnt    = histcounts2(X, Y, EdgeX, EdgeY);
                    Val    = quantile(double(Cnt(:)), Args.Quantiles(:)).';
                end
            end
        end

        if IsAstroImage && Args.UpdateHeader
            for Iq = 1:Nq
                Obj(Iobj).HeaderData.replaceVal(KeyNames{Iq}, Val(Iq), ...
                    'Comment',{sprintf('Q%02d of source counts in a %dx%d grid', ...
                                       round(100.*Args.Quantiles(Iq)), Nbin(1), Nbin(2))});
            end
            Obj(Iobj).HeaderData.replaceVal('SPATNBIN', Nbin(1).*Nbin(2), ...
                'Comment',{'Number of spatial-density grid cells'});
        end

        Result(Iobj).Quantiles = Args.Quantiles;
        Result(Iobj).Value     = Val;
        Result(Iobj).KeyNames  = KeyNames;
        Result(Iobj).Nsrc      = Nsrc;
        Result(Iobj).Nbin      = Nbin;
        if Args.ReturnCounts
            Result(Iobj).Counts = Cnt;
        end
    end
end


function Col = getColOrEmpty(Cat, ColName)
    % Return the named column, or [] if it is absent.
    Col = [];
    if ismember(ColName, Cat.ColNames)
        try
            Col = Cat.getCol(ColName);
        catch
            Col = [];
        end
    end
end


function CCDSEC = resolveCCDSEC(CCDSEC, Obj, Iobj, IsAstroImage, X, Y)
    % Binning area: explicit argument, else image size, else NAXIS keywords,
    % else the catalog's own extent.
    if ~isempty(CCDSEC)
        return;
    end
    if IsAstroImage
        Sz = size(Obj(Iobj).Image);
        if all(Sz > 0)
            CCDSEC = [1 Sz(2) 1 Sz(1)];
            return;
        end
        Nx = getNumKey(Obj(Iobj).HeaderData, 'NAXIS1');
        Ny = getNumKey(Obj(Iobj).HeaderData, 'NAXIS2');
        if isfinite(Nx) && isfinite(Ny)
            CCDSEC = [1 Nx 1 Ny];
            return;
        end
    end
    CCDSEC = [1 ceil(max(X)) 1 ceil(max(Y))];
end


function V = getNumKey(Header, Key)
    V = NaN;
    try
        V = Header.getVal(Key);
    catch
        V = NaN;
    end
    if ~isnumeric(V) || isempty(V)
        V = NaN;
    end
end
