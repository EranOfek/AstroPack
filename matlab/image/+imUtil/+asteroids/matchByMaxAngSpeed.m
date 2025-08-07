function [Result] = matchByMaxAngSpeed(Data, Args)
    % Search [JD, RA, Dec] by time and cone-search position
    %     Given a table of [JD, RA, Dec], search for events detected at
    %     roughly the same time and position.
    % Input  : - 
    %          - 
    %          * ...,key,val,... 
    % Output : - 
    % Author : Eran Ofek (2025 Jul) 
    % Example: R=imUtil.asteroids.matchByMaxAngSpeed([T.jd, T.ra, T.dec]);

    arguments
        Data
        
        Args.MaxAngSpeed       = 0.3;    % [deg/day]
        Args.ColJD             = 1;
        Args.ColRA             = 2;
        Args.ColDec            = 3;
        Args.CooUnits          = 'deg';

        Args.RangeJD           = [];

    end
    
    RAD = 180./pi;

    JD   = Data(:,Args.ColJD);
    RA   = Data(:,Args.ColRA);
    Dec  = Data(:,Args.ColDec);

    ConvFactor =  convert.angular(Args.CooUnits, 'rad');
    RA         = RA  .* ConvFactor;
    Dec        = Dec .* ConvFactor;


    if ~isempty(Args.RangeJD)
        Ijd = find(Time>=Args.RangeJD(1) & Time<Args.RangeJD(2));
        JD   = JD(Ijd);
        RA   = RA(Ijd);
        Dec  = Dec(Ijd);
    end

    [Dec,SI] = sort(Dec);
    RA       = RA(SI);
    JD       = JD(SI);

    Nobj = numel(JD);

    K           = 0;
    Result      = struct('Nmatch',cell(Nobj,1), 'IndObj',cell(Nobj,1), 'IndMatch',cell(Nobj,1));
    
    MaxDeltaJD  = range(JD);
    MaxSearchRadius = MaxDeltaJD.*Args.MaxAngSpeed;
    Result=VO.search.search_sortedlat_multi([RA, Dec], RA, Dec, MaxSearchRadius);

    'a'

    % for Iobj=1:1:Nobj
    %     %[Iobj, Nobj]
    %     DeltaJD      = JD(Iobj) - JD;
    %     SearchRadius = Args.MaxAngSpeed.*DeltaJD;  % [deg]
    % 
    % 
    %     Ind=VO.search.search_sortedlat_multi([RA, Dec], RA, Dec, MaxSearchRadius);
    % 
    %     AngDist = celestial.coo.sphere_dist_fast(RA(Iobj), Dec(Iobj), RA, Dec);  % [rad]
    %     IndMatch = find(AngDist<(SearchRadius./RAD));
    % 
    %     Result(Iobj).Nmatch     = numel(IndMatch);
    %     Result(Iobj).IndObj     = Iobj;
    %     Result(Iobj).IndMatch   = IndMatch;
    % 
    % end



end
