function Result = getAstrometricCatalog(RA, Dec, Args)
    % Get Astrometric catalog from local/external database
    %   and optionally apply proper motion, parallax and units conversions.
    % Input  : - J2000.0 R.A. [rad, deg, [H M S], or sexagesimal string]
    %          - J2000.0 Dec. [rad, deg, [Sign D M S], or sexagesimal string]
    %          * ...,key,val,...
    %            'CatName' - Catalog name. Default is 'GAIAEDR3'.
    %            'CatOrigin' - Catalog origin. Default is 'catsHTM'.
    %            'Radius' - Search radius. Default is 1000.
    %            'RadiusUnits' - Search radius units. Default is 'arcsec'.
    %            'CooUnits' - Search RA/Dec units (this isused only if
    %                   RA/Dec are numerical scalars). Default is 'deg'.
    %            'Shape' - Search shape. Not implemented. Currently will
    %                   return all sources in cone.
    %            'OutUnits' - Output catalog units. Default is 'deg'.
    %            'Con' - Search constraings for catsHTM.
    %                   E.g., {{'Mag_G',[15 16]},{'Plx',@(x) ~isnan(x)}}.
    %                   Default is {}.
    %            'UseIndex' - UseIndex parameter for catsHTM.
    %                   Default is false.
    %            'EpochOut' - Output epoch. DEfault units is 'JD' (see
    %                   imProc.cat.applyProperMotion for more options).
    %                   If empty, will not apply proper motion and
    %                   parallax.
    %                   Default is [].
    %            'EpochIn' - If given, then will override catalog epoch.
    %                   Default units are 'JD'.
    %            'parsProperMotion' - A cell array of additional arguments
    %                   to pass to imProc.cat.applyProperMotion.
    %                   Default is {}.
    % Output : - An AstroCatalog object with the astrometric catalog.
    % Author : Eran Ofek (Jun 2021)
    % Example: Result = imProc.cat.getAstrometricCatalog(1,1);
    
    
    arguments
        RA
        Dec
        Args.CatName char             = 'GAIAEDR3';
        Args.CatOrigin                = 'catsHTM'
        Args.Radius                   = 1000;
        Args.RadiusUnits              = 'arcsec';
        Args.CooUnits                 = 'deg';
        Args.Shape
        Args.OutUnits                 = 'deg';
        Args.Con                      = {};
        Args.UseIndex(1,1) logical    = false;
        Args.EpochOut                 = [];  % if empty - don't apply proper motion
        Args.EpochIn                  = [];  % if given - don't use catalog Epoch
        Args.parsProperMotion cell    = {};
    end
    
    % convert RA/Dec to radians (if in degrees)
    if isnumeric(RA) && numel(RA)==1
        RA = convert.angular(Args.CooUnits, 'rad', RA);
    end
    if isnumeric(Dec) && numel(Dec)==1
        Dec = convert.angular(Args.CooUnits, 'rad', Dec);
    end        
    
    switch lower(Args.CatOrigin)
        case 'catshtm'
            % use catsHTM
            Result = catsHTM.cone_search(Args.CatName, RA, Dec, Args.Radius, 'Con', Args.Con,...
                                                                             'RadiusUnits',Args.RadiusUnits,...
                                                                             'UseIndex',Args.UseIndex,...
                                                                             'OnlyCone',true,...
                                                                             'OutType','astrocatalog');
                                                                
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
                Result = imProc.cat.applyProperMotion(Obj, EpochIn, Args.EpochOut, Args.parsProperMotion{:},'EpochInUnits',EpochInUnits);
            end
            
            % coordinates are in radians
            % convert to OutUnits
            Result.convertCooUnits(Args.OutUnits);
            
        otherwise
            error('Unsupported CatOrigin option');
    end
    
    
end

