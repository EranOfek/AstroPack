function [Result, RA, Dec] = applyProperMotionSimple(Obj, OutEpoch, Args)
    % Apply proper motion without radial velocity and parallax.
    %   Propagate source coordinates from an input epoch to an output epoch
    %   using only the proper-motion components in RA and Dec. The function
    %   ignores parallax, radial velocity, and perspective acceleration.
    %
    %   The propagated coordinates are written back into the RA and Dec
    %   columns of the output AstroCatalog object.
    %
    %   See also: imProc.cat.applyProperMotion
    %
    % Input  : - An AstroCatalog object, or an array of AstroCatalog objects.
    %          - Output epoch to which the coordinates will be propagated.
    %            By default, this is interpreted as JD. See the
    %            'OutEpochUnits' argument.
    %          * ...,key,val,...
    %            'OutUnits' - Units of output coordinates.
    %                   Options include any angular units supported by
    %                   convert.angular, e.g., 'deg' or 'rad'.
    %                   Default is 'rad'.
    %            'OutEpochUnits' - Units of the output epoch.
    %                   Units should be supported by convert.time.
    %                   Default is 'JD'.
    %            'InEpoch' - Input epoch, or name of the catalog column
    %                   containing the input epoch for each source.
    %                   If numeric, this value is used as the input epoch.
    %                   If a string/char, the value is interpreted as a
    %                   column name in the AstroCatalog object.
    %                   Default is 'Epoch'.
    %            'InEpochUnits' - Units of the input epoch, used when
    %                   'InEpoch' is numeric. If 'InEpoch' is a column name,
    %                   the units are read from the column units.
    %                   Default is [].
    %            'ColRA' - Name of the right-ascension column.
    %                   Default is 'RA'.
    %            'ColDec' - Name of the declination column.
    %                   Default is 'Dec'.
    %            'ColPMRA' - Name of the proper-motion-in-RA column.
    %                   This is assumed to be dRA/dt*cos(Dec), i.e.,
    %                   mu_alpha_cos_delta, in proper-motion units.
    %                   Default is 'PMRA'.
    %            'ColPMDec' - Name of the proper-motion-in-Dec column.
    %                   Default is 'PMDec'.
    %            'CooUnits' - Units of the input RA and Dec coordinates.
    %                   If empty, the units are read from the catalog column
    %                   units. Units should be supported by convert.angular.
    %                   Default is [].
    %            'PMUnits' - Units of the input proper motions.
    %                   If empty, the units are read from the catalog column
    %                   units. Units should be supported by
    %                   convert.proper_motion.
    %                   Internally, proper motions are converted to mas/yr.
    %                   Default is [].
    %            'CreateNewObj' - Logical flag indicating whether to create
    %                   a copy of the input AstroCatalog object before
    %                   modifying the RA and Dec columns.
    %                   If false, the input object is modified in place.
    %                   If true, a copied object is returned.
    %                   Default is false.
    %
    % Output : - AstroCatalog object with propagated RA and Dec coordinates.
    %            If 'CreateNewObj' is true, this is a copy of the input
    %            object with updated coordinates. Otherwise, this is the
    %            modified input object.
    %          - Propagated right ascension of the last AstroCatalog element
    %            processed, in degrees before conversion to 'OutUnits'.
    %          - Propagated declination of the last AstroCatalog element
    %            processed, in degrees before conversion to 'OutUnits'.
    %
    % Notes  : - The propagation is performed using:
    %              RA  = RA  + DT .* PMRA  ./ cos(Dec)
    %              Dec = Dec + DT .* PMDec
    %            where DT is the epoch difference in Julian years and the
    %            proper motions are converted to mas/yr.
    %          - The RA proper-motion column is assumed to contain
    %            mu_alpha_cos_delta. Therefore, the code divides by
    %            cos(Dec) to obtain the change in RA.
    %          - This simple propagation is appropriate for short time
    %            baselines or cases where parallax and radial velocity are
    %            negligible.
    %
    % Author : Eran Ofek (2026 May)
    %
    % Example: 
    %   Result = imProc.cat.applyProperMotionSimple(Obj, 2460000.5);
    %
    %   Result = imProc.cat.applyProperMotionSimple(Obj, 2026.0, ...
    %                  'OutEpochUnits','J', ...
    %                  'InEpoch','Epoch', ...
    %                  'OutUnits','deg');

    arguments
        Obj
        OutEpoch               
        Args.OutUnits          = 'rad';
        Args.OutEpochUnits     = 'JD';
        Args.InEpoch           = 'Epoch';
        Args.InEpochUnits      = [];
        Args.ColRA             = 'RA';
        Args.ColDec            = 'Dec';
        Args.ColPMRA           = 'PMRA';
        Args.ColPMDec          = 'PMDec';
        Args.CooUnits          = [];
        Args.PMUnits           = [];

        Args.CreateNewObj      = false;
    end
    JYear = 365.25;  % Julian year [day]
    InvMARCSEC = 1./3600000;

    if Args.CreateNewObj
        Result = Obj.copy;
    else
        Result = Obj;
    end

    OutEpoch = convert.time(OutEpoch, Args.OutEpochUnits, 'JD');

    Nobj = numel(Obj);
    for Iobj=1:1:Nobj
        [Data, Units]= Obj(Iobj).getCol({Args.ColRA, Args.ColDec, Args.ColPMRA, Args.ColPMDec});
        if isempty(Args.CooUnits)
            % use column units
            Conv = convert.angular(Units{1}, 'deg');
        else
            Conv = convert.angular(Args.CooUnits, 'deg');
        end
        RA  = Data(:,1).*Conv;
        Dec = Data(:,2).*Conv;
        
        if isempty(Args.PMUnits)
            % use column units
            ConvPM = convert.proper_motion(Units{3}, 'mas/yr');
        else
            ConvPM = convert.proper_motion(Args.PMUnits, 'mas/yr');
        end
        PM_RA  = Data(:,3).*ConvPM;
        PM_Dec = Data(:,4).*ConvPM;

        if isnumeric(Args.InEpoch)
            InEpoch = Args.InEpoch;
            InEpochUnits = Args.InEpochUnits;
        else
            [InEpoch, InEpochUnits] = Obj(Iobj).getCol(Args.InEpoch);
        end

        InEpoch = convert.time(InEpoch, InEpochUnits{1}, 'JD');

        DT_Year = (OutEpoch - InEpoch)/JYear;

        RA  = RA + DT_Year.*InvMARCSEC.*PM_RA./cosd(Dec);
        Dec = Dec + DT_Year.*InvMARCSEC.*PM_Dec;

        Conv = convert.angular('deg', Args.OutUnits);
        Result(Iobj).replaceCol([RA, Dec].*Conv, {Args.ColRA, Args.ColDec}, Inf, {Args.OutUnits, Args.OutUnits});
    end


end
