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

