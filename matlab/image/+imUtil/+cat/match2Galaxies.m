function [Result] = match2Galaxies(RA, Dec, Args)
    % Match positions to known galaxies and redsift
    %   The matching is done effectively one by one.
    %   I.e., this function is good when the tested coordinates are spread
    %   over the sky.
    % Input  : - J2000 RA [name|sexagesimal|rad|deg].
    %            See celestial.convert.cooResolve for options.
    %          - J2000 Dec.
    %          * ...,key,val,... 
    %            'InUnits' - Default is 'deg'.
    %            'Server' - If input is object name, then this is the name
    %                   server that will be used: @VO.name.server_simbad|
    %                   @VO.name.server_ned.
    %                   Default is @VO.name.server_ned
    %            'SearchRadPGC' - Initial search radius for PGC.
    %                   The actual search is done using the galaxy size.
    %                   Default is 1000 arcsec.
    %            'ColPGCZ' - Column index of redshift in the PGC catalog.
    %                   Default is 19.
    %            'SearchRadGlade' - Search radius for GLADE.
    %                   Default is 10 arcsec.
    %            'ColGladeZ' - Column index of redshift in the GLADE catalog.
    %                   Default is XX
    % Output : - A structure with the following fields:
    %            .IsInLMC - A logical array indicating if target is
    %                   associated with the LMC.
    %            .IsInSMC - Same for SMC.
    %            .IsInM31 - Same for M31.
    %            .IsInM33 - Same for M33.
    %            .Npgc - Number of PGC matches.
    %            .Z_PGC - Min redshift of PGC galaxy in matched radius.
    %            .Nglade - Number of GLADE matches.
    %            .Z_GLADE - Min redshift of GLADE galaxy in matched radius.
    % Author : Eran Ofek (2026 Mar) 
    % Example: R=imUtil.cat.match2Galaxies(RA,Dec);

    arguments
        RA
        Dec
        Args.InUnits           = 'deg';  % 'deg'|'rad'|'sex'|'ned'|'simbad'|
        Args.Server            = @VO.name.server_ned;

        Args.SearchRadPGC      = 1000;  % arcsec
        Args.ColPGCZ           = 19;
        Args.SearchRadGlade    = 10;    % arcsec
        Args.ColGladeZ         = [];

    end

    % read coordinates / convert to radinas
    [RA, Dec] = celestial.convert.cooResolve(RA, Dec, 'InUnits',Args.InUnits, 'OutUnits','rad', 'Server',Args.Server);
    Ntarget = numel(RA);

    % Search LMC/SMC
    Result.IsInLMC = celestial.galaxies.isInsideLMC(RA, Dec, 'rad');
    Result.IsInSMC = celestial.galaxies.isInsideSMC(RA, Dec, 'rad');
    Result.IsInM31 = celestial.galaxies.isInsideM31(RA, Dec, 'rad');
    Result.IsInM33 = celestial.galaxies.isInsideM33(RA, Dec, 'rad');

    % Search PGC with galaxy radius
    Npgc  = zeros(Ntarget,1);
    Z_PGC = zeros(Ntarget,1);
    for Itarget=1:1:Ntarget
        CatPGC     = catsHTM.cone_search('PGC', RA(Itarget), Dec(Itarget), Args.SearchRadPGC);
        GalRadius  = 3.*10.^CatPGC(:,4);
        GalZ       = CatPGC(:,Args.ColPGCZ);
        DistPGC    = celestial.coo.sphere_dist_fast(RA(Itarget), Dec(Itarget), CatPGC(:,1), CatPGC(:,2));
        Igal       = find(DistPGC<GalRadius);
        if isempty(Igal)
            Npgc  = 1;
            Z_PGC = NaN;
        else
            [~,Imin] = DistPGC(Igal);
            Npgc     = numel(Igal);
            Z_PGC    = GalZ(Igal(Imin));
        end
    end

    % Search Glade with constant radius
    Nglade  = zeros(Ntarget,1);
    Z_GLADE = nan(Ntarget,1);
    for Itarget=1:1:Ntarget
        CatGlade = catsHTM.cone_search('GLADEp', RA(Itarget), Dec(Itarget), Args.SearchRadGlade);
        Nglade(Itarget)   = size(CatGlade);
        Z_GLADE(Itarget)  = min(CatGlade(:,Args.ColGladeZ));
    end

    % search SDSS

    % search NED

    % search DESI

    % search extended 


    % Prep output:
    Result.Npgc    = Npgc;
    Result.Z_PGC   = Z_PGC;
    Result.Nglade  = Nglade;
    Result.Z_GLADE = Z_GLADE;

end
