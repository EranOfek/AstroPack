function Result=astrometryQuality(Obj, Args)
    %
    
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
        Args.ColMag                = 'phot_g_mean_mag';
        Args.ColColor      = {'phot_bp_mean_mag','phot_rp_mean_mag'};  % single column or two columns
        
        Args.ColRefRA      = AstroCatalog.DefNamesRA;
        Args.ColRefDec     = AstroCatalog.DefNamesDec;
        Args.ColCatRA      = AstroCatalog.DefNamesRA;
        Args.ColCatDec     = AstroCatalog.DefNamesDec;
        
    end
    
    % Match the input catalog against the reference catalog
    [MatchedRefCat, ResInd, CatH] = imProc.match.returnMatched_catsHTM(Obj, 'CatName',Args.CatName,...
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
    
    % Calc statistics and quality
    Nobj = numel(Obj);
    for Iobj=1:1:Nobj
        % get RA/Dec
        if isa(Obj, 'AstroImage')
            CatRADec = Obj(Iobj).CatData.getLonLat('rad', 'ColLon',Args.ColCatRA, 'ColLat',ColCatDec);
            CatXY    = Obj(Iobj).CatData.getXY('ColX',Args.ColCatX, 'ColY',ColCatY);
        else
            % assuming Obj is AstroCatalog
            CatRADec = Obj(Iobj).getLonLat('rad', 'ColLon',Args.ColCatRA, 'ColLat',ColCatDec);
            CatXY    = Obj(Iobj).getXY('ColX',Args.ColCatX, 'ColY',ColCatY);
        end
        RefRADec = MatchedRefCat(Iobj).getLonLat('rad', 'ColLon',Args.ColRefRA, 'ColLat',ColRefDec);
        
        
        Result(Iobj).DeltaRA  = CatRADec(:,1) - RefRADec(:,1);
        Result(Iobj).DeltaDec = CatRADec(:,2) - RefRADec(:,2);
        % make sure DeltaRA is not larger than pi
        FlagPI = DeltaRA>pi;
        Result(Iobj).DeltaRA(FlagPI) = Result(Iobj).DeltaRA(FlagPI) - 2.*pi;
        
        [Result(Iobj).DeltaDist, Result(Iobj).PA] = celestial.coo.sphere_dist_fast(CatRADec(:,1), CatRADec(:,2), RefRADec(:,1), RefRADec(:,2));
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
    
    end
    
    
    
    
end
