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
    %                   fraction of the in-range sources that must survive
    %                   the neighbour rejection ('RemoveNeighboors'). In
    %                   crowded fields nearly every source has a neighbour
    %                   within 'RemoveNeighboorsRadius', so a deep magnitude
    %                   range can be left with almost no usable sources.
    %                   When the fraction is below this value, the faint
    %                   limit of 'RangeMag' is brightened in steps until the
    %                   fraction is satisfied. The range is never deepened,
    %                   so a field in which the fraction is already
    %                   satisfied is unaffected.
    %                   If empty, no adaptation is done. Default is [].
    %            'AdaptMagStep' - Step [mag] by which the faint limit is
    %                   brightened. Default is 0.5.
    %            'AdaptMagMin' - The faint limit is never brightened below
    %                   this value. Default is 15.
    %            'AdaptMinNsrc' - The faint limit is never brightened to a
    %                   value leaving fewer than this number of sources.
    %                   Default is 50.
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
        Args.AdaptMagMin               = 15;
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
    if RangeMag(2)<=Args.AdaptMagMin
        % already at least as bright as we are ever willing to go
        return;
    end

    % The surviving fraction decreases monotonically with the faint limit, so
    % scan upwards and stop at the first limit that fails. Scanning upwards
    % also means the large (deep, crowded) samples are never evaluated.
    Ladder = (Args.AdaptMagMin:Args.AdaptMagStep:RangeMag(2));
    if Ladder(end)<RangeMag(2)
        Ladder = [Ladder, RangeMag(2)];
    end

    BestFaint = [];
    for Ifaint=1:1:numel(Ladder)
        [Nin, Nkept] = countKept(Cone, [RangeMag(1), Ladder(Ifaint)], Args);
        if Nin>0 && (Nkept./Nin)<Args.MinFracIsolated
            % the fraction only gets worse with depth - stop here
            break;
        end
        if Nkept>=Args.AdaptMinNsrc
            % acceptable - remember it, but keep looking for a deeper limit
            % that is still acceptable
            BestFaint = Ladder(Ifaint);
        end
        % too few sources at this limit is a reason to go deeper, not to stop
    end

    if ~isempty(BestFaint)
        RangeMag(2) = BestFaint;
    end
    % if nothing was acceptable, leave the requested range untouched
end


function [Nin, Nkept] = countKept(Cone, RangeMag, Args)
    % Number of sources in a magnitude range, before and after the neighbour
    % rejection. Operates on a copy, so the input cone is not modified.

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
        Cat     = sortrows(Cat, 'Dec');
        UseFlag = ~imProc.match.flagSrcWithNeighbors(Cat, Args.flagSrcWithNeighborsArgs{:}, 'CooType','sphere',...
                                                     'Radius',Args.RemoveNeighboorsRadius);
        Nkept   = sum(UseFlag);
    end
end

