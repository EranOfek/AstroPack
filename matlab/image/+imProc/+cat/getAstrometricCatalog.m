function [Result, RA, Dec] = getAstrometricCatalog(RA, Dec, Args)
    % Get Astrometric catalog from local/external database
    %   and optionally apply proper motion, parallax and units conversions.
    % Input  : - J2000.0 R.A. [rad, deg, [H M S], or sexagesimal string]
    %          - J2000.0 Dec. [rad, deg, [Sign D M S], or sexagesimal string]
    %          * ...,key,val,...
    %            'CatName' - Catalog name. Default is 'GAIAEDR3'.
    %                   If AstroCatalog, then will return the catalog as
    %                   is.
    %            'CatOrigin' - Catalog origin. Default is 'catsHTM'.
    %            'Radius' - Search radius. Default is 1000.
    %            'RadiusUnits' - Search radius units. Default is 'arcsec'.
    %            'CooUnits' - Search RA/Dec units (this isused only if
    %                   RA/Dec are numerical scalars). Default is 'deg'.
    %            'Shape' - Search shape. Not implemented. Currently will
    %                   return all sources in cone.
    %            'OutUnits' - Output catalog units. Default is 'rad'.
    %            'Con' - Search constraings for catsHTM.
    %                   E.g., {{'Mag_G',[15 16]},{'Plx',@(x) ~isnan(x)}}.
    %                   Default is {}.
    %            'UseIndex' - UseIndex paramter for catsHTM.
    %                   Default is false.
    %            'EpochOut' - Output epoch. Default units is 'JD' (see
    %                   imProc.cat.applyProperMotion for more options).
    %                   If empty, will not apply proper motion and
    %                   parallax.
    %                   This must be a scalar, and if not will use the first image
    %                   JD.
    %                   Default is [].
    %            'EpochIn' - If given, then will override catalog epoch.
    %                   Default units are 'JD'.
    %            'argsProperMotion' - A cell array of additional arguments
    %                   to pass to imProc.cat.applyProperMotion.
    %                   Default is {}.
    %            'ColNameMag' - Column name containing mag.
    %                   Default is {'phot_bp_mean_mag','phot_g_mean_mag'}
    %            'RangeMag' - Magnitude range to retrieve.
    %                   Default is [12 19.5].
    %            'MinFracIsolated' - If not empty, then this is the minimum
    %                   acceptable value of the ratio
    %                   (sources surviving the neighbour rejection) /
    %                   (sources within 'RangeMag'),
    %                   and the faint limit of 'RangeMag' is adapted to the
    %                   crowding of the field until the ratio is met.
    %                   Rationale: a reference source is usable only if no
    %                   other catalog source lies within
    %                   'RemoveNeighboorsRadius' (see 'RemoveNeighboors').
    %                   That step and the magnitude range fight each other,
    %                   because the faint sources gained by a deeper limit
    %                   are exactly the neighbours which disqualify the
    %                   brighter ones - so the number of usable sources is
    %                   not monotonic in depth. E.g., on the galactic bulge
    %                   [12 19.5] gives 143568 sources of which 196 are
    %                   isolated, while [10 17] gives 18347 of which 8359
    %                   are isolated.
    %                   How it works: the cone is searched once, and is then
    %                   re-filtered in memory at a ladder of trial faint
    %                   limits running from
    %                   RangeMag(2)-'AdaptMaxDeltaMag' up to RangeMag(2) in
    %                   steps of 'AdaptMagStep'. The scan runs bright to
    %                   faint and stops at the first trial failing the ratio,
    %                   so the large deep samples are never evaluated.
    %                   The limit which is used is the deepest trial
    %                   satisfying both this ratio and 'AdaptMinNsrc'.
    %                   If no trial satisfies the ratio, the brightest trial
    %                   is used instead, provided it has at least
    %                   'AdaptMinNsrc' isolated sources - every deeper trial
    %                   is worse, so it is the best limit available within
    %                   the cap. If that too fails, 'RangeMag' is left
    %                   untouched.
    %                   The adaptation only ever brightens the faint limit,
    %                   and never touches the bright limit, so a field which
    %                   already satisfies the ratio gets exactly the catalog
    %                   it would have got otherwise. In particular, it does
    %                   not replace an exposure time dependent 'RangeMag'
    %                   (see 'RefRangeMagExpTimeFun' of
    %                   imProc.astrometry.astrometrySubImages): such a range
    %                   is computed by the caller, and the adaptation is
    %                   applied on top of it.
    %                   If empty, no adaptation is done. Default is [].
    %            'AdaptMagStep' - Step [mag] in which the faint limit is
    %                   brightened by the 'MinFracIsolated' adaptation. Sets
    %                   the resolution of the scan and, with
    %                   'AdaptMaxDeltaMag', the maximal number of trials.
    %                   Default is 0.5.
    %            'AdaptMaxDeltaMag' - The largest amount [mag] by which the
    %                   'MinFracIsolated' adaptation is allowed to brighten
    %                   the faint limit. The brightest limit it can select
    %                   is therefore RangeMag(2)-'AdaptMaxDeltaMag', clipped
    %                   at RangeMag(1).
    %                   The cap is relative, not absolute, so that it scales
    %                   with the requested range. An absolute limit both
    %                   allows a deep request to be brightened much further
    %                   than a shallow one, and switches the adaptation off
    %                   completely for any range already brighter than it -
    %                   e.g. a 1s exposure, where the caller has already
    %                   shifted [10 17] to [7.7 14.7].
    %                   Use Inf for no cap. Default is 5.
    %            'AdaptMinNsrc' - The faint limit is never brightened to a
    %                   value leaving fewer than this number of isolated
    %                   sources. Default is 50.
    %            'ColNamePlx' - Parallax column name.
    %                   Default is {'Plx'}.
    %            'RangePlx' - Parllax range to retrieve.
    %                   Default is [-Inf 50].
    %            'UsePlxRange' - Boolian indicating if to constrain the
    %                   sources by Plx (true), or not (false). 
    %                   Defauls is true.
    %            'OutRADecUnits' - Output units for the RA and Dec output
    %                   arguments. Default is 'rad'.
    %            'RemoveNeighboors' - A logical indicating if to remove
    %                   sources with close neighboors. Default is true.
    %            'flagSrcWithNeighborsArgs' - A cell array of additional
    %                   arguments to pass to flagSrcWithNeighbors.
    %                   Default is {}.
    % Output : - An AstroCatalog object with the astrometric catalog.
    %          - The input RA [units from 'OutRADecUnits'].
    %          - The input Dec [units from 'OutRADecUnits'].
    % Author : Eran Ofek (Jun 2021)
    % Example: Result = imProc.cat.getAstrometricCatalog(1,1);
    
    
    arguments
        RA
        Dec
        Args.CatName                  = 'GAIADR3'; %'GAIAEDR3';   % or AstroCatalog
        Args.CatOrigin                = 'catsHTM';
        Args.Radius                   = 1000;
        Args.RadiusUnits              = 'arcsec';
        Args.CooUnits                 = 'deg';
        Args.Shape
        Args.OutUnits                 = 'rad';
        Args.Con cell                 = {};
        Args.UseIndex(1,1) logical    = false;
        Args.EpochOut                 = [];  % if empty - don't apply proper motion
        Args.EpochIn                  = [];  % if given - don't use catalog Epoch
        Args.argsProperMotion cell    = {};
        % queryRange
        Args.ColNameMag                = {'phot_bp_mean_mag','phot_g_mean_mag'}; % {'Mag_BP','Mag'};
        Args.RangeMag                  = [12 19.5];
        % Adaptive faint limit (crowded fields) - see help
        Args.MinFracIsolated               = [];
        Args.AdaptMagStep              = 0.5;
        Args.AdaptMaxDeltaMag          = 5;
        Args.AdaptMinNsrc              = 50;
        Args.ColNamePlx                = {'Plx'};
        Args.UsePlxRange               = true;
        Args.RangePlx                  = [-Inf 50];
        % OutRADec
        Args.OutRADecUnits             = 'rad';

        Args.RemoveNeighboors(1,1) logical      = true;
        Args.RemoveNeighboorsRadius             =10;
        Args.flagSrcWithNeighborsArgs cell      = {};
           
    end
    
    % convert RA/Dec to radians (if in degrees)
    if isnumeric(RA) && numel(RA)==1
        RA = convert.angular(Args.CooUnits, 'rad', RA);
    end
    if isnumeric(Dec) && numel(Dec)==1
        Dec = convert.angular(Args.CooUnits, 'rad', Dec);
    end        
    
    if ischar(Args.CatName)
        switch lower(Args.CatOrigin)
            case 'catshtm'
                % use catsHTM
                Result = catsHTM.cone_search(Args.CatName, RA, Dec, Args.Radius, 'Con', Args.Con,...
                                                                                 'RadiusUnits',Args.RadiusUnits,...
                                                                                 'UseIndex',Args.UseIndex,...
                                                                                 'OnlyCone',true,...
                                                                                 'OutType','astrocatalog');

                % Adapt the faint limit to the source density of the field.
                % The cone is searched once and the trials only re-filter it.
                if ~isempty(Args.MinFracIsolated) && Args.RemoveNeighboors
                    Args.RangeMag = adaptFaintLimit(Result, Args);
                end


                % Addtitional constraints on astrometric catalog
                % mag and parallax constraints
                % no output argument means that CreateNewObj=false
                if Args.UsePlxRange               
                    queryRange(Result, Args.ColNameMag, Args.RangeMag,...
                                    Args.ColNamePlx, Args.RangePlx);
                else
                    queryRange(Result, Args.ColNameMag, Args.RangeMag);
                end

                % apply proper motion
                if ~isempty(Args.EpochOut)
                    if isempty(Args.EpochIn)
                        % use EpochIn from catalog
                        EpochIn = getCol(Result, 'Epoch');
                        EpochInUnits = 'j';
                    else
                        % override catalog Epoch
                        EpochIn = Args.EpochIn;
                        EpochInUnits = 'jd';
                    end                    
                    Result = imProc.cat.applyProperMotion(Result, EpochIn(:), Args.EpochOut(1), Args.argsProperMotion{:},'EpochInUnits',EpochInUnits, 'CreateNewObj',false);
                end

                % coordinates are in radians
                % convert to OutUnits
                Result.convertCooUnits(Args.OutUnits);

            otherwise
                error('Unsupported CatOrigin option');
        end
        
        % perform catalog cleaning
        
        % filter Ref - remove sources with neighboors
        if Args.RemoveNeighboors
            % sort AstrometricCat
            Result = sortrows(Result, 'Dec');
            
            UseFlag = ~imProc.match.flagSrcWithNeighbors(Result, Args.flagSrcWithNeighborsArgs{:}, 'CooType','sphere',...
                'Radius',Args.RemoveNeighboorsRadius);
            Result  = selectRows(Result, UseFlag);
        end
       
    else
        % assume CatName contains an actual catalog
        Result = Args.CatName;   % no need to copy
        % FFU: add treatment for sexagesimal coordinates
%         if numel(RA)>1
%             error('FFU: Current version treat only RA/Dec deg/rad when CatName is AstroCatalog');
%         end
%         ConvFactor  = convert.angular(Args.CooUnits, 'rad');
%         RA          = ConvFactor .* RA;
%         Dec         = ConvFactor .* Dec;
        
        % convert catalog to OutUnits
        Result.convertCooUnits(Args.OutUnits);
        
    end
    
    % convert RA/Dec to OutRADecUnits units
    Factor = convert.angular('rad',Args.OutRADecUnits);
    RA     = RA.*Factor;
    Dec    = Dec.*Factor;

end


function RangeMag = adaptFaintLimit(Cone, Args)
    % Brighten the faint limit of Args.RangeMag until a sufficient fraction
    % of the in-range sources survives the neighbour rejection.
    % Input  : - An AstroCatalog with the full cone search result.
    %          - The Args structure of getAstrometricCatalog.
    % Output : - The magnitude range to use. Equal to Args.RangeMag unless
    %            the field is crowded enough to require brightening.
    % Author : Alexander Gioffe (Aug 2026)

    RangeMag = Args.RangeMag;

    % The faint limit may be brightened by at most AdaptMaxDeltaMag relative to
    % the requested one, and never past the bright limit. A relative cap scales
    % with the requested range, unlike an absolute one, which both lets a deep
    % request be brightened much further than a shallow one and switches the
    % adaptation off completely for a range already brighter than it.
    FloorMag = max(RangeMag(2) - Args.AdaptMaxDeltaMag, RangeMag(1));
    if FloorMag>=RangeMag(2)
        % no room to brighten
        return;
    end

    % Over the range that matters the surviving fraction decreases with the
    % faint limit, so scan upwards and stop at the first limit that fails.
    % Scanning upwards also means the large (deep, crowded) samples are never
    % evaluated. The fraction is not monotonic everywhere - it wiggles at the
    % bright end where the counts are small - but there it is far above any
    % sensible MinFracIsolated, so the scan is not stopped early by it.
    Ladder = (FloorMag:Args.AdaptMagStep:RangeMag(2));
    if Ladder(end)<RangeMag(2)
        Ladder = [Ladder, RangeMag(2)];
    end

    % countKept needs the catalogue sorted by Dec for the neighbour search, and
    % a magnitude cut preserves that order, so sort once here instead of once
    % per ladder step (issue #1257, observation 6)
    ConeSorted = sortrows(Cone.copy, 'Dec');

    BestFaint   = [];
    BrightestOK = false;   % the brightest trial has enough isolated sources
    for Ifaint=1:1:numel(Ladder)
        [Nin, Nkept] = countKept(ConeSorted, [RangeMag(1), Ladder(Ifaint)], Args);
        if Ifaint==1
            BrightestOK = Nkept>=Args.AdaptMinNsrc;
        end
        if Nin>0 && (Nkept./Nin)<Args.MinFracIsolated
            % over the range that matters the fraction only gets worse with
            % depth - stop here
            break;
        end
        if Nkept>=Args.AdaptMinNsrc
            % acceptable - remember it, but keep looking for a deeper limit
            % that is still acceptable
            BestFaint = Ladder(Ifaint);
        end
        % too few sources at this limit is a reason to go deeper, not to stop
    end

    if isempty(BestFaint) && BrightestOK
        % Not even the brightest allowed limit meets the fraction. Every deeper
        % trial is worse, so this is the best limit available within the cap,
        % and it is still far better than the requested one.
        BestFaint = Ladder(1);
    end

    if ~isempty(BestFaint)
        RangeMag(2) = BestFaint;
    end
    % if nothing was acceptable, leave the requested range untouched
end


function [Nin, Nkept] = countKept(Cone, RangeMag, Args)
    % Number of sources in a magnitude range, before and after the neighbour
    % rejection. Operates on a copy, so the input cone is not modified.
    % The input must already be sorted by Dec (adaptFaintLimit sorts once).

    Cat = Cone.copy;
    if Args.UsePlxRange
        queryRange(Cat, Args.ColNameMag, RangeMag, Args.ColNamePlx, Args.RangePlx);
    else
        queryRange(Cat, Args.ColNameMag, RangeMag);
    end
    Nin = sizeCatalog(Cat);
    if Nin==0
        Nkept = 0;
    else
        % No sortrows here: the cone was sorted by Dec once in adaptFaintLimit
        % and the magnitude cut above keeps the row order. It does clear the
        % IsSorted flag, because queryRange assigns to Catalog and that setter
        % resets it, so restore the flag rather than re-sort (#1257, obs. 6).
        % flagSrcWithNeighbors still verifies the order itself (issorted).
        Cat.IsSorted = true;
        UseFlag = ~imProc.match.flagSrcWithNeighbors(Cat, Args.flagSrcWithNeighborsArgs{:}, 'CooType','sphere',...
                                                     'Radius',Args.RemoveNeighboorsRadius);
        Nkept   = sum(UseFlag);
    end
end

