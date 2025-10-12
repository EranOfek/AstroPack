function [Obj, Data, ColNames, ColUnits]=matchStars(Obj, Args)
    % Match input catalog in AstroCatalog/AstroImage/AstroZOGY object to known stars and add information to catalogs.
    %   This function is replacing: imProc.match.match2Stars
    %   For each catalog of sources contained in the input object:
    %     - Match **GAIA** catalog sources (proper-motion aware).
    %     - Match **unWISE** catalog sources.
    %     - Match **PS1** (Pan-STARRS1) catalog sources.
    %   Add to the input catalog new columns describing the nearest associations and selected
    %   catalog attributes. The order of the output columns is:
    %     {'GAIA_Dist', Args.ColRefMag, Args.ColRefColor, Args.ColRefInfo{:}, ...
    %      'GAIA_SumMag', 'GAIA_NmatchSmall', 'GAIA_NmatchLarge', ...
    %      'unWISE_Dist', Args.ColCatIR{:}, 'PS1_Dist', Args.ColCatPS1{:}}
    %   where:
    %     - 'GAIA_Dist'        : Distance [arcsec] to nearest GAIA association (within Args.Radius; default 60").
    %     - Args.ColRefMag     : Reference magnitude column from GAIA (default 'phot_bp_mean_mag').
    %     - Args.ColRefColor   : Reference color from GAIA (default 'bp_rp').
    %     - Args.ColRefInfo    : Additional GAIA columns (default {'Plx','ErrPlx'}).
    %     - 'GAIA_SumMag'      : Blended/summed magnitude of non-associated GAIA neighbors
    %                            computed by Args.funSumMag using zero point Args.KeyZP.
    %     - 'GAIA_NmatchSmall' : Number of GAIA sources within **Args.AssocRadius** (association radius).
    %     - 'GAIA_NmatchLarge' : Total GAIA matches within **Args.Radius** (search radius).
    %     - 'unWISE_Dist'      : Distance [arcsec] to nearest unWISE association.
    %     - Args.ColCatIR      : Selected unWISE columns (default {'MagAB_w1','MagAB_w2'}).
    %     - 'PS1_Dist'         : Distance [arcsec] to nearest PS1 association.
    %     - Args.ColCatPS1     : Selected PS1 columns (default {'gPSFMag','rPSFMag','iPSFMag','zPSFMag','yPSFMag'}).
    %
    % Input  : - Obj : AstroCatalog / AstroImage / AstroZOGY object (scalar or array). For each element,
    %                   sources are matched to external catalogs via catsHTM cone searches.
    %          * ...,key,val,... control arguments (Args), in the **same order** as the arguments block:
    %            % -------- GAIA catalog (primary reference) --------
    %            'CatName'           : GAIA catalog name for catsHTM. Default: 'GAIADR3'.
    %            'Radius'            : GAIA search radius (units set by Args.RadiusUnits). Default: 60.
    %            'AssocRadius'       : GAIA association radius used to define the nearest/associated source
    %                                  and to count GAIA_NmatchSmall (in Args.RadiusUnits). Default: 2.
    %            'ApplyPM'           : Logical. Apply GAIA proper motion in the matching. Default: true.
    %            'ColRefMag'         : GAIA magnitude column to attach. Default: 'phot_bp_mean_mag'.
    %            'ColRefColor'       : GAIA color column to attach. Default: 'bp_rp'.
    %            'ColRefInfo'        : Cellstr of additional GAIA columns to attach. Default: {'Plx','ErrPlx','astrometric_excess_noise'}.
    %            'ColRefPlx'         : Alias/name used for parallax in outputs. Default: 'Plx'.
    %            'ColRefPlxErr'      : Alias/name used for parallax error in outputs. Default: 'ErrPlx'.
    %            'funSumMag'         : Function handle for blended-magnitude estimator taking
    %                                  (MagVector, DistArcsecVector, ZP) → SumMag. Default:
    %                                  @(Mag,Dist,ZP) ZP - 2.5.*log10(sum(0.03.*(10.^(-0.4.*(Mag-ZP)))./(Dist.^2))).
    %            'KeyZP'             : Photometric zero point; either a numeric value or a header
    %                                  keyword name found via Obj(I).getStructKey. Default: 'PH_ZP'.
    %            % -------- unWISE (IR) --------
    %            'CatNameIR'         : IR catalog name. Default: 'unWISE'.
    %            'RadiusIR'          : unWISE search radius (in Args.RadiusUnits). Default: 5.
    %            'ColCatIR'          : Cellstr of unWISE columns to attach. Default: {'MagAB_w1','MagAB_w2'}.
    %            % -------- PS1 --------
    %            'CatNamePS1'        : PS1 catalog name. Default: 'PS1'.
    %            'RadiusPS1'         : PS1 search radius (in Args.RadiusUnits). Default: 5.
    %            'ColCatPS1'         : Cellstr of PS1 columns to attach. Default: {'gPSFMag','rPSFMag','iPSFMag','zPSFMag','yPSFMag'}.
    %            % -------- Coordinates, units, and matching flow --------
    %            'CooUnits'          : Coordinate units of inputs passed to catsHTM ('deg'|'rad'). Default: 'deg'.
    %            'RadiusUnits'       : Units for *all* radii above ('arcsec'|'deg'|'rad'). Default: 'arcsec'.
    %            'Con'               : catsHTM.cone_search constraints (cell array of name/value). Default: {}.
    %            'CheckIsSorted'     : Verify input catalog is sorted for faster cone searches. Default: true.
    %            'SortCol'           : Column name used for sorting (e.g., 'Dec'). Default: 'Dec'.
    %            'KeyJD'             : Header keyword for observation mid-time (e.g., 'MIDJD'); if empty,
    %                                  will be derived from header info when possible. Default: 'MIDJD'.
    %            'InsertCols'        : If true, insert the generated columns into each catalog inside Obj.
    %                                  If false, return Data/ColNames/ColUnits only (no insertion). Default: true.
    %
    % Output : - Obj      : Same type as input, updated in-place (when Args.InsertCols=true) with
    %                       additional columns described above for each contained catalog.
    %          - Data     : Numeric matrix of the newly computed columns for the *last* processed element
    %                       (nearest distances/magnitudes, counts, and selected catalog fields).
    %          - ColNames : Cell array of column names corresponding to Data (exact order as listed above).
    %          - ColUnits : Cell array of units for each column in Data. Distances in 'arcsec';
    %                       magnitudes in 'mag'; counts unitless; other units as per catalog fields.
    %
    % Notes  : - Distances reported as 'GAIA_Dist', 'unWISE_Dist', 'PS1_Dist' are by default in **arcsec**.
    %          - 'GAIA_SumMag' excludes the associated (nearest) GAIA source; neighbors are weighted by
    %            1/r^2 with a scale factor (see Args.funSumMag). Requires a valid zero point (Args.KeyZP).
    %          - Matching is performed via imProc.match.matchMulti_catsHTM for each catalog independently.
    %
    % Author : Eran Ofek (Oct 2025)
    % Example: imProc.match.matchStars(AD.New, 'InsertCols', false);


    arguments
        Obj

        % GAIA catalog
        Args.CatName             = 'GAIADR3';   % GAIA catalog name
        Args.Radius              = 60;    % search radius for nearby sources
        Args.AssocRadius         = 2;     % search radius for source association
        Args.ApplyPM             = true;
        Args.ColRefMag           = 'phot_bp_mean_mag';
        Args.ColRefColor         = 'bp_rp';
        Args.ColRefInfo          = {'Plx', 'ErrPlx', 'astrometric_excess_noise'}

        Args.ColRefPlx           = 'Plx';
        Args.ColRefPlxErr        = 'ErrPlx';
        
        Args.funSumMag           = @(Mag, Dist, ZP) ZP-2.5.*log10(sum(  0.03.*(10.^(-0.4.*(Mag-ZP)))./(Dist.^2)  ) );
        Args.KeyZP               = 'PH_ZP';   % photometric ZP: header keyword or value

        % IR catalog
        Args.CatNameIR           = 'unWISE';
        Args.RadiusIR            = 5;   % search radius for unWISE catalog
        Args.ColCatIR            = {'MagAB_w1','MagAB_w2'};
        
        % PS1 catalog
        Args.CatNamePS1          = 'PS1';
        Args.RadiusPS1           = 5;   % search radius for PS1 catalog
        Args.ColCatPS1           = {'gPSFMag','rPSFMag','iPSFMag','zPSFMag','yPSFMag'};

        Args.CooUnits            = 'deg';
        
        Args.RadiusUnits         = 'arcsec';
        Args.Con                 = {};    % catsHTM.cone_search constraints
        Args.CheckIsSorted       = true;  % Verify the input catalog is sorted
        Args.SortCol             = 'Dec'; % Column to sort by.
        Args.KeyJD               = 'MIDJD';  % if empty will calculate from header info.
        Args.InsertCols          = true;
        
    end
    RAD = 180./pi;
    ARCSEC_DEG = 3600;
    ARCSEC_RAD = RAD.*ARCSEC_DEG;
    Args.AssocRadius = convert.angular(Args.RadiusUnits, 'arcsec', Args.AssocRadius); % [arcsec]

    Nobj = numel(Obj);

    % Match eas sourrce in each catalog with the external sources
    [ResInd,CatGAIA] = imProc.match.matchMulti_catsHTM(Obj, Args.CatName, 'Coo',[],...
                                                       'CooUnits',Args.CooUnits,...
                                                       'Radius',Args.Radius,...
                                                       'RadiusUnits',Args.RadiusUnits,...
                                                       'Con',Args.Con,...
                                                       'CheckIsSorted',Args.CheckIsSorted,...
                                                       'SortCol',Args.SortCol,...
                                                       'catsHTMisRef',false,...
                                                       'ApplyPM',Args.ApplyPM,...
                                                       'KeyJD',Args.KeyJD);

    % Matches with PS1
    [ResIndPS1,CatPS1] = imProc.match.matchMulti_catsHTM(Obj, Args.CatNamePS1, 'Coo',[],...
                                                       'CooUnits',Args.CooUnits,...
                                                       'Radius',Args.RadiusPS1,...
                                                       'RadiusUnits',Args.RadiusUnits,...
                                                       'Con',Args.Con,...
                                                       'CheckIsSorted',Args.CheckIsSorted,...
                                                       'SortCol',Args.SortCol,...
                                                       'catsHTMisRef',false,...
                                                       'ApplyPM',false);


    % Matches with unWISE
    [ResIndIR,CatIR] = imProc.match.matchMulti_catsHTM(Obj, Args.CatNameIR, 'Coo',[],...
                                                       'CooUnits',Args.CooUnits,...
                                                       'Radius',Args.RadiusIR,...
                                                       'RadiusUnits',Args.RadiusUnits,...
                                                       'Con',Args.Con,...
                                                       'CheckIsSorted',Args.CheckIsSorted,...
                                                       'SortCol',Args.SortCol,...
                                                       'catsHTMisRef',false,...
                                                       'ApplyPM',false);


    % Order has meaning! 
    ColNames = ['GAIA_Dist', Args.ColRefMag, Args.ColRefColor, Args.ColRefInfo, 'GAIA_SumMag', 'GAIA_NmatchSmall', 'GAIA_NmatchLarge', 'unWISE_Dist', Args.ColCatIR, 'PS1_Dist', Args.ColCatPS1];
    ColUnits = {'arcsec', 'mag', 'mag', cell(1, numel(Args.ColRefInfo)), 'mag', '', '', 'arcsec', numel(Args.ColCatIR), 'arcsec', numel(Args.ColCatPS1)};

    for Iobj=1:1:Nobj
        % for each element in the input object: AstroCatalog, AstroImage,
        % AstroZOGY,...

        Nsrc = numel(ResInd(Iobj).Ind);
        
        TempMag    = CatGAIA(Iobj).getCol(Args.ColRefMag);
        TempColor  = CatGAIA(Iobj).getCol(Args.ColRefColor);
        TempInfo   = CatGAIA(Iobj).getCol(Args.ColRefInfo);
        TempIR     = CatIR(Iobj).getCol(Args.ColCatIR);
        TempPS1    = CatPS1(Iobj).getCol(Args.ColCatPS1);

        if isnumeric(Args.KeyZP)
            ZP = Args.KeyZP;
        else
            Temp = Obj(Iobj).getStructKey(Args.KeyZP);
            ZP   = Temp.(Args.KeyZP);
        end

        % allocate memory
        % GAIA
        NearestDist    = nan(Nsrc,1);
        NearestMag     = nan(Nsrc,1);
        NearestColor   = nan(Nsrc,1);
        NearestInfo    = nan(Nsrc,numel(Args.ColRefInfo));
        SumMag         = nan(Nsrc,1);   % sum of mag based on formular Args.funSumMag
        NmatchSmall    = nan(Nsrc,1);   % number of GAIA sources within Args.AssocRadius
        NmatchLarge    = nan(Nsrc,1);   % number of GAIA sources within Args.Radius
        % IR
        DistIR         = nan(Nsrc,1);
        InfoIR         = nan(Nsrc, numel(Args.ColCatIR));
        % PS1
        DistPS1        = nan(Nsrc,1);
        InfoPS1        = nan(Nsrc, numel(Args.ColCatPS1));
        

        for Isrc=1:1:Nsrc
            %Isrc
            % search for matched source - typicall small search radius
            
            % store information about the nearest match:
            % GAIA
            if ~isempty(ResInd(Iobj).Ind(Isrc).Dist)
                [MinDist, MinInd]   = min(ResInd(Iobj).Ind(Isrc).Dist);
                NearestDist(Isrc)   = MinDist.*ARCSEC_RAD;
                NearestMag(Isrc)    = TempMag(MinInd);
                NearestColor(Isrc)  = TempColor(MinInd);
                NearestInfo(Isrc,:) = TempInfo(MinInd,:);
                NmatchSmall(Isrc)   = sum(ResInd(Iobj).Ind(Isrc).Dist<=Args.AssocRadius);
                NmatchLarge(Isrc)   = ResInd(Iobj).Ind(Isrc).Nmatch;
    
                % weighted sum of mags
                AllDist = ResInd(Iobj).Ind(Isrc).Dist.*ARCSEC_RAD;
                AllMag  = TempMag(ResInd(Iobj).Ind(Isrc).Ind);
                % remove association
                IndNotAssoc = find(AllDist>Args.AssocRadius);
                SumMag(Isrc)  = Args.funSumMag(AllMag(IndNotAssoc), AllDist(IndNotAssoc), ZP);
            end

            % IR
            if ~isempty(ResIndIR(Iobj).Ind(Isrc).Dist)
                [MinDistIR, MinIndIR] = min(ResIndIR(Iobj).Ind(Isrc).Dist);
                DistIR(Isrc)        = MinDistIR.*ARCSEC_RAD;
                InfoIR(Isrc,:)      = TempIR(MinIndIR,:);
            end

            % PS1
            if ~isempty(ResIndPS1(Iobj).Ind(Isrc).Dist)
                [MinDistPS1, MinIndPS1] = min(ResIndPS1(Iobj).Ind(Isrc).Dist);
                DistPS1(Isrc)   = MinDistPS1.*ARCSEC_RAD;
                InfoPS1(Isrc,:) = TempPS1(MinIndPS1,:);
            end

            
        end

        % Output type
        Data = [NearestDist, NearestMag, NearestColor, NearestInfo, SumMag, NmatchSmall, NmatchLarge, DistIR, InfoIR, DistPS1, InfoPS1];
        if Args.InsertCols
            % insert columns to catalog of sources
            if isa(Obj(Iobj), 'AstroCatalog')
                Obj(Iobj).insertCol(Data, Inf, ColNames, ColUnits);
            else
                Obj(Iobj).CatData.insertCol(Data, Inf, ColNames, ColUnits);
            end
        end 

    end

end