function [Flag] = findInBox(RA, Dec, CornersRA, CornersDec, Args)
    % Given a list of corners of multiple fields, check if a given coordinate (scalar) is within these fields.
    %   See also: celestial.coo.in_box, celestial.coo.inside_celestial_box,
    %   celestial.htm.in_polysphere
    % Input  : - RA
    %          - Dec
    %          - A 4 columns matrix of RA corners.
    %          - A 4 columns matrix of Dec corners.
    %          * ...,key,val,... 
    %            'InUnits' - Default is 'deg'.
    %            'SearchRadius' - This should be twice the fields size.
    %                   Default is 2.
    % Output : - A vector of flags indicating if the coordinate is in each
    %            field.
    % Author : Eran Ofek (2024 Jul) 
    % Example: Flag=celestial.coo.findInBox(RA, Dec, [OT.RAU1, OT.RAU2, OT.RAU3, OT.RAU4], [OT.DECU1, OT.DECU2, OT.DECU3, OT.DECU4]);

    arguments
        RA
        Dec
        CornersRA
        CornersDec
        Args.InUnits      = 'deg';
        Args.SearchRadius = 2;
    end

    Factor  = convert.angular(Args.InUnits, 'rad');
    RA      = RA.*Factor;
    Dec     = Dec.*Factor;
    CornersRA  = CornersRA.*Factor;
    CornersDec = CornersDec.*Factor;
    Args.SearchRadius = Args.SearchRadius.*Factor;

    D = celestial.coo.sphere_dist_fast(RA, Dec, CornersRA(:,1), CornersDec(:,2));
    Icand = find(D<Args.SearchRadius);
    Ncand = numel(Icand);

    Ncor = size(CornersRA,1);
    Flag = false(Ncor,1);
    for I=1:1:Ncand
        Icor = Icand(I);
        CornersIcor = [CornersRA(Icor,:).', CornersDec(Icor,:).'];
        Flag(Icor) = celestial.htm.in_polysphere([RA, Dec],CornersIcor);
    end
end
