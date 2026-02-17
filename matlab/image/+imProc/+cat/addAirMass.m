function [Result, AirMass] = addAirMass(AI, Args)
    % Add/update airmass (and Healpix) column to AstroCatalog or AstroImage based on time and position.
    %   Optionally also add healpix information.
    % Input  : - An AstroImage object.
    %          * ...,key,val,... 
    %            'ColAM' - Column name in which to store (or
    %                   replace) the calculated airmass.
    %            'JD' - A vector/scalar of JD (one per image, or scalar) to
    %                   use in the airmass calculation (UTC).
    %                   If empty, then attempt to read the JD from the
    %                   header.
    %                   Default is [].
    %            'KeyJD' - An header keyword containing the mid JD.
    %                   If empty, will try to calculate it from available
    %                   information in the header (see AstroImage/julday).
    %                   Default is 'MIDJD'.
    %            'Coo' - A two column matrix of [RA, Dec] coordinates of
    %                   images (preferably center). One line per image.
    %                   If empty, then read from header.
    %                   Default is [].
    %            'ColCoo' - A two element cell/string array containing the
    %                   RA and Dec column names from which to read
    %                   the image coordinates.
    %                   Default is {'RA','DEC'}.
    %            'CooUnits' - Coordinates (RA/Dec and Geodetic) units.
    %                   Default is 'deg'.
    %            'GeoPos' - Geodetic position [Lon, Lat, Height[m]].
    %                   Default is [35.04, 30.05 415].
    %
    %            --- Add UPIX* --- Insert Healpix ID to header
    %            'HealpixType' - 'nested'|'ring'.
    %                   If [], then do not insert UPIX to header
    %                   Default is 'nested'.
    %            'HealpixLevel' - Default is 2.^[3, 8, 16]
    %            'KeyHealpix' - Default is ["NSIDE_PARTITION", "NSIDE_LOW", "NSIDE_HIGH"]
    %            'UniqueID' - A logical flag indicating if to return a
    %                   unique ID (using celestial.healpix.pix2uniqueId).
    %                   Default is false.
    %
    %            'UseDict' - A logical indicating if to use keywords
    %                   dictionary. Default is false.
    %            'CreateNewObj' - A logical indicating if to create a new
    %                   copy of the input object. Default is false.
    % Output : - The input AstroImage object in which the airmass haeder
    %            keyword value is updated or added.
    %          - Array of airmass per image (same shape as input
    %            AstroImage).
    % Author : Eran Ofek (2026 Feb) 
    % Example: AI=imProc.cat.addAirMass(AI)

    arguments
        AI
        Args.ColAM             = 'AIRMASS'
        Args.JD                = [];
        Args.KeyJD             = 'MIDJD';
        Args.Coo               = [];
        Args.ColCoo            = {'RA','Dec'};
        Args.CooUnits          = 'deg';
        Args.GeoPos            = [35.04, 30.05 415]; % [deg deg m]

        % Add UPIX keywords (healpix indexing)
        Args.HealpixType       = 'nested';  % [] for none
        Args.HealpixLevel      = 2.^[3, 8, 16];   % diamater ~ 13 deg, 0.4 deg, 5.7"
        Args.KeyHealpix        = ["UPIX_PAR", "UPIX_LOW", "UPIX_HIG"];
        Args.UniqueID logical = true;

        Args.UseDict           = false;
        Args.CreateNewObj      = false;
    end
    Nlevel = numel(Args.HealpixLevel);

    if Args.CreateNewObj
        Result = AI.copy;
    else
        Result = AI;
    end

    Conv = convert.angular(Args.CooUnits, 'rad');
   
    Nai = numel(AI);

    if isempty(Args.JD)
        % read from header
        Args.JD = AI.julday('KeyJD',Args.KeyJD, 'UseDict',Args.UseDict);
    else
        if isscalar(Args.JD) && Nai>1
            Args.JD = repmat(Args.JD, Nai, 1);
        end
    end

    % 
    % if isempty(Args.Coo)
    %     % read RA/Dec from header
    %     TmpCoo   = AI.getStructKey(Args.KeyCoo, 'UseDict',Args.UseDict);
    %     Args.Coo = [[TmpCoo.(Args.KeyCoo{1})].', [TmpCoo.(Args.KeyCoo{2})].'];
    % end
    % Args.Coo         = Args.Coo.*Conv;   % convert to radians
    Args.GeoPos(1:2) = Args.GeoPos(1:2).*Conv; 
    
    % calculate Hardie airmass
    %

    for Iai=1:1:Nai
        % get CatData from AstroCatalog or AstroImage
        Cat = AI(Iai).getCatData();

        % Calculate AirMass
        Coo = Cat.getCol(Args.ColCoo).*Conv;  % read RA, Dec and convert to radians
        AirMass = celestial.coo.airmass(Args.JD(Iai), Coo(:,1), Coo(:,2), Args.GeoPos);

        % insert airmass to catalog
        Cat.replaceCol(AirMass, Args.ColAM, Inf, '');

        if ~isempty(Args.HealpixType)
            Ncoo    = size(Coo,1);
            UpixVal = zeros(Ncoo, Nlevel, 'int64');
            for Ilevel=1:1:Nlevel
                UpixVal(:,Ilevel) = celestial.healpix.ang2pix(Args.HealpixLevel(Ilevel), Coo(:,1), Coo(:,2), 'Type',Args.HealpixType, 'CooUnits','rad', 'UniqueID', Args.UniqueID);
            end
            UpixVal = cast(UpixVal, "like",Cat.Catalog);
            Cat  = Cat.insertCol(UpixVal, Inf, {Args.KeyHealpix{:}});
        end

        % Insert Cat into Result
        Result(Iai).setCatData(Cat);
    end

end
