function Result=astrometryQualityData(Obj, Args)
    % Check the quality of astrometric solution in respect to GAIA-DR3
    %   Given a catalog, in AstroCatalog or AstroImage object, with J2000
    %   coordinates, match the sources coordinates with a reference catalog
    %   coordinates (e.g., GAIADR3) including proper motion and parallax.
    %   The function return, for each catalog, the RA, Dec, coordinate
    %   diffreences, X/Y positions, and magnitude and colors.
    %   These data can be used to test the quality of the astrometric
    %   solution as a function of some parameters (e.g., color, pixel
    %   phase, magnitude, X, Y positions).
    % Input  : - An AstroImage or AstroCatalog object containing the
    %            catalogs to check. The catalogs must contain RA, Dec
    %            (e.g., populated using the WCS).
    %          * ...,key,val,...
    %            'CatName' - catsHTM catalog name (e.g., 'GAIADR3').
    %                   See catsHTM.catalogs for possible options.
    %                   Alternatively, this can be an Astrocatalog object
    %                   containing the catsHTM catalog of the field after applying
    %                   proper motion, but before matching.
    %
    %            'MagFromRef' - Get magnitude from reference catalog.
    %                   If false, then get it from input catalog.
    %                   Default is true.
    %            'ColMag' - Column name containing the magnitude.
    %                   Default is AstroCatalog.DefNamesMag
    %            'ColColor' - Column name containing the color if sources.
    %                   This can be a single column name containing the
    %                   color (mag) or a cell array of two column names
    %                   containing the bands from which the color will be
    %                   calculated (first col - second col).
    %                   Default is {'phot_bp_mean_mag','phot_rp_mean_mag'}
    %            'ColRefRA' - Column name in the Ref catalog containing the
    %                   RA. Default is AstroCatalog.DefNamesRA
    %            'ColRefDec' - Column name in the Ref catalog containing the
    %                   Dec. Default is AstroCatalog.DefNamesDec
    %            'ColCatRA' - Column name in the input catalog containing the
    %                   RA. Default is AstroCatalog.DefNamesRA
    %            'ColCatDec' - Column name in the input catalog containing the
    %                   Dec. Default is AstroCatalog.DefNamesDec
    %
    %            'cell2d_statArgs' - A cell array of arguments to pass to
    %                   tools.math.stat.cell2d_stat
    %                   Default is {}.
    %            'MagLimit' - Mag limit to use when calculating cell
    %                   statistics.
    %                   Default is 17.
    %            'Nbin' - Number of binx to use in [X,Y] for cell
    %                   statistics.
    %                   Default is [3 3].
    %    Argumnets passed to: imProc.match.returnMatched_catsHTM
    %            'Coo' - [RA, Dec] of coordinates to search.
    %                   If empty, then will attempt to find this
    %                   from the catalog itself. Default is [].
    %            'CooUnits' - Units of coordinates. Object default
    %                   is 'deg'.
    %            'Radius' - Matching radius. Default is 3.
    %            'RadiusUnits' - Matchin radius units.
    %                   Default is 'arcsec'.
    %            'CatRadius' - The search radius of the catsHTM
    %                   catalog. If not given this is taken as the
    %                   bounding circle radius of the inout
    %                   AstroCatalog. Default is [].
    %            'CatRadiusUnits' - CatRadius units.
    %                   Default is 'arcsec'.
    %            'Con' - A cell array of additional
    %                  constraints to apply to output catalog.
    %                  See catsHTM.cone_search for options.
    %                  E.g., {{'Mag_G',[15 16]},{'Plx',@(x) ~isnan(x)}}
    %                  Default is {}.
    %            'catsHTMisRef' - A logical indicating if the
    %                   catsHTM catalog is treated as the reference
    %                   catalog. Default is false.
    %                   If true, then the output is the same but for the catsHTM catalog.
    %            'ApplyPM' - A logical indicating if to correct the
    %                   coordinates in the catsHTM catalog to proper motion and
    %                   parallax. This may work for some catalogs (e.g.,
    %                   'GAIADR3'). Default is true.
    %            'applyProperMotionArgs' - A cell array of arguments to
    %                   pass to imProc.cat.applyProperMotion
    %                   Default is {}.
    %            'ColEpoch' - Column name containing the epoch of the
    %                   catalog. Default is 'Epoch'.
    %            'EpochUnits' - Units of the epoch (in 'ColEpoch').
    %                   See convert.time for option.
    %                   Default is 'J' (i.e., Julian years).
    %
    % Output : A structure array of data from which the astrometric quality
    %          can be derived. One element per input catalog element.
    %          The following fields are available:
    %          (All coordinates are in radians)
    %          .DeltaRA - RA_cat - RA_ref [arcsec]
    %          .DeltaDec - Dec_cat - Dec_ref [arcsec]
    %          .CatRADec - [RA, Dec] of input cat.
    %          .RefRADec - [RA, Dec] of ref.
    %          .CatXY    - [X,Y] of input cat.
    %          .Dist     - Ang. dist. between cat and ref.
    %          .PA       - PA between sources in cat and ref.
    %          .Mag      - Magnitudes.
    %          .Color    - Colors.
    %          .PixelPhaseX - Pixel phase in X.
    %          .PixelPhaseY - Pixel phase in Y.
    %          .MedShiftRAas - Median RA shift in arcsec.
    %          .MedShiftDecas - Median Dec shift in arcsec.
    %          .ResidRA_XY - Statistics of residual in RA as a function of Y,X. [arcsec]
    %          .ResidDec_XY - Statistics of residual in Dec as a function of Y,X. [arcsec]
    % Author : Eran Ofek (May 2023)
    % Example: R=imProc.astrometry.astrometryQualityData(AI);
    
    arguments
        Obj    % AstroImage | AstroCatalog
        Args.CatName             = 'GAIADR3';
        
        % arguments of imProc.match.returnMatched_catsHTM
        Args.Coo                 = [];
        Args.CooUnits            = 'deg';
        Args.Radius              = 3;
        Args.RadiusUnits         = 'arcsec';
        Args.CatRadius           = [];
        Args.CatRadiusUnits      = 'arcsec';
        Args.Con                 = {};
        Args.catsHTMisRef        = false;
        Args.ApplyPM logical     = true; % will work for GAIA only
        Args.applyProperMotionArgs cell = {};
        Args.ColEpoch                   = 'Epoch';   % epoch col name in catsHTM catalog
        Args.EpochUnits                 = 'J';  % 'J' for Julian years, 'JD' for JD,... see convert.time
        
        % internal argumnets
        Args.MagFromRef logical    = true;
        Args.ColMag                = 'phot_bp_mean_mag';
        Args.ColColor              = {'phot_bp_mean_mag','phot_rp_mean_mag'};  % single column or two columns
        Args.cell2d_statArgs       = {};
        Args.MagLimit              = 17;
        Args.Nbin                  = [3 3];

        Args.ColRefRA      = AstroCatalog.DefNamesRA;
        Args.ColRefDec     = AstroCatalog.DefNamesDec;
        Args.ColCatRA      = AstroCatalog.DefNamesRA;
        Args.ColCatDec     = AstroCatalog.DefNamesDec;
        Args.ColCatX       = AstroCatalog.DefNamesX;
        Args.ColCatY       = AstroCatalog.DefNamesY;
        
    end
    
    RAD = 180./pi;
    ARCSEC_DEG = 3600;

    
    
    % Calc statistics and quality
    Nobj = numel(Obj);
    for Iobj=1:1:Nobj
        % Match the input catalog against the reference catalog
        [MatchedRefCat(Iobj), ResInd(Iobj), CatH] = imProc.match.returnMatched_catsHTM(Obj(Iobj), Args.CatName,...
                                                        'Coo',Args.Coo,...
                                                        'CooUnits',Args.CooUnits,...
                                                        'Radius',Args.Radius,...
                                                        'RadiusUnits',Args.RadiusUnits,...
                                                        'CatRadius',Args.CatRadius,...
                                                        'CatRadiusUnits',Args.CatRadiusUnits,...
                                                        'Con',Args.Con,...
                                                        'catsHTMisRef',Args.catsHTMisRef,...
                                                        'ApplyPM',Args.ApplyPM,...
                                                        'applyProperMotionArgs',Args.applyProperMotionArgs,...
                                                        'ColEpoch',Args.ColEpoch,...
                                                        'EpochUnits',Args.EpochUnits);

        % get RA/Dec
        if isa(Obj, 'AstroImage')
            CatRADec = Obj(Iobj).CatData.getLonLat('rad', 'ColLon',Args.ColCatRA, 'ColLat',Args.ColCatDec);
            CatXY    = Obj(Iobj).CatData.getXY('ColX',Args.ColCatX, 'ColY',Args.ColCatY);
        else
            % assuming Obj is AstroCatalog
            CatRADec = Obj(Iobj).getLonLat('rad', 'ColLon',Args.ColCatRA, 'ColLat',Args.ColCatDec);
            CatXY    = Obj(Iobj).getXY('ColX',Args.ColCatX, 'ColY',ColCatY);
        end
        RefRADec = MatchedRefCat(Iobj).getLonLat('rad', 'ColLon',Args.ColRefRA, 'ColLat',Args.ColRefDec);
        
        
        Result(Iobj).DeltaRA  = CatRADec(:,1) - RefRADec(:,1);
        Result(Iobj).DeltaDec = CatRADec(:,2) - RefRADec(:,2);
        % make sure DeltaRA is not larger than pi
        FlagPI = Result(Iobj).DeltaRA>pi;
        Result(Iobj).DeltaRA(FlagPI) = Result(Iobj).DeltaRA(FlagPI) - 2.*pi;
        
        [Result(Iobj).DeltaDist, Result(Iobj).PA] = celestial.coo.sphere_dist(CatRADec(:,1), CatRADec(:,2), RefRADec(:,1), RefRADec(:,2));
        Result(Iobj).CatRADec = CatRADec;
        Result(Iobj).RefRADec = RefRADec;
        Result(Iobj).CatXY    = CatXY;
        
        % get magnitude from ref catalog
        if Args.MagFromRef
            Mag   = MatchedRefCat(Iobj).getCol(Args.ColMag);
            Color = MatchedRefCat(Iobj).getCol(Args.ColColor);
        else
            Mag   = Obj(Iobj).getCol(Args.ColMag);
            Color = MatchedRefCat(Iobj).getCol(Args.ColColor);
        end
        if size(Color,2)>1
            % assuming color was given as two independet bands
            Color = Color(:,1) - Color(:,2);
        end
        Result(Iobj).Mag   = Mag;
        Result(Iobj).Color = Color;
        
        % rms vs. magnitude
        
        % rms vs. color
        
        % rms vs. pixel phase
        Result(Iobj).PixelPhaseX = mod(Result(Iobj).CatXY(:,1), 1);
        Result(Iobj).PixelPhaseY = mod(Result(Iobj).CatXY(:,2), 1);
        
        % rms vs. position
        Result(Iobj).DeltaRA         = Result(Iobj).DeltaRA  .* RAD.*ARCSEC_DEG;      % [arcsec]
        Result(Iobj).DeltaDec        = Result(Iobj).DeltaDec .* RAD.*ARCSEC_DEG;      % [arcsec]

        F = Mag<Args.MagLimit;
        if sum(F)>10
            Result(Iobj).ResidRA_XY      = tools.math.stat.cell2d_stat(Result(Iobj).CatXY(F,1), Result(Iobj).CatXY(F,2), Result(Iobj).DeltaRA(F), Args.cell2d_statArgs{:},  'NbinX',Args.Nbin(1), 'NbinY',Args.Nbin(2));
            Result(Iobj).ResidDec_XY     = tools.math.stat.cell2d_stat(Result(Iobj).CatXY(F,1), Result(Iobj).CatXY(F,2), Result(Iobj).DeltaDec(F), Args.cell2d_statArgs{:}, 'NbinX',Args.Nbin(1), 'NbinY',Args.Nbin(2));
        end
        
        % Summary
        Result(Iobj).MedShiftRAas   = median(Result(Iobj).DeltaRA, 1, 'omitnan');     % [arcsec]
        Result(Iobj).MedShiftDecas  = median(Result(Iobj).DeltaDec, 1, 'omitnan');    % [arcsec]
    
    end
    
    
    
    
end
