function [Result] = addAirMass(AI, Args)
    % Add/update airmass keyword to AstroImage header based on time and position.
    % Input  : - An AstroImage object.
    %          * ...,key,val,... 
    %            'KeyAM' - Header keyword name in which to store (or
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
    %            'KeyCoo' - A two element cell/string array containing the
    %                   RA and Dec header keyword names from which to read
    %                   the image coordinates.
    %                   Default is {'RA','DEC'}.
    %            'CooUnits' - Coordinates (RA/Dec and Geodetic) units.
    %                   Default is 'deg'.
    %            'GeoPos' - Geodetic position [Lon, Lat, Height[m]].
    %                   Default is [35.04, 30.05 415].
    %            'UseDict' - A logical indicating if to use keywords
    %                   dictionary. Default is false.
    %            'CreateNewObj' - A logical indicating if to create a new
    %                   copy of the input object. Default is false.
    % Output : - The input AstroImage object in which the airmass haeder
    %            keyword value is updated or added.
    % Author : Eran Ofek (2026 Feb) 
    % Example: AI=imProc.header.addAirMass(AI)

    arguments
        AI
        Args.KeyAM             = 'AIRMASS'
        Args.JD                = [];
        Args.KeyJD             = 'MIDJD';
        Args.Coo               = [];
        Args.KeyCoo            = {'RA','DEC'};
        Args.CooUnits          = 'deg';
        Args.GeoPos            = [35.04, 30.05 415]; % [deg deg m]

        Args.UseDict           = false;
        Args.CreateNewObj      = false;
    end
    
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
    if isempty(Args.Coo)
        % read RA/Dec from header
        TmpCoo   = AI.getStructKey(Args.KeyCoo, 'UseDict',Args.UseDict);
        Args.Coo = [TmpCoo.(Args.KeyCoo{1}), TmpCoo.(Args.KeyCoo{2})];
    end
    Args.Coo         = Args.Coo.*Conv;   % convert to radians
    Args.GeoPos(1:2) = Args.GeoPos(1:2).*Conv; 
    
    for Iai=1:1:Nai
        % calculate Hardie airmass
        AirMass = celestial.coo.airmass(Args.JD(:), Args.Coo(:,1), Args.Coo(:,2), Args.GeoPos);

        % insert airmass to header
        Result(Iai).HeaderData.replaceVal(Args.KeyAM, AirMass);
    end

end
