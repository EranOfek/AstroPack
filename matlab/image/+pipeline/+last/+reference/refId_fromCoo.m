function [Ind, Dist] = refId_fromCoo(RA, Dec, Args)
    % Search for LAST Reference ID from coordinates.
    % Input  : - J2000 RA, or target name.
    %            See: celestial.convert.cooResolve for options.
    %          - J2000 Dec.
    %          * ...,key,val,...
    %            'RefTable' - The LAST reference image table ID and coo
    %                   list. If empty, then will read from repository.
    %                   Default is [].
    %            'RefTableName' - File Name containing reference table.
    %                   Default is 'LAST_RefIm_Grid.mat'.
    %            'InUnits' - Default is 'deg'.
    %            'Server' - If input is object name, then this is the name
    %                   server that will be used: @VO.name.server_simbad|
    %                   @VO.name.server_ned.
    %                   Default is @VO.name.server_simbad
    %            'InitSearchRadius' - Search radius fort initial search.
    %                   Default is 1 deg.
    % Output : - Ref ID indices covering the requested coordinates (may be
    %            more than one).
    %          - For each Ref ID, the angular distance (rad) from the
    %            requested coordinates.
    % Author : Eran Ofek (2026 Jun) 
    % Example: Ind=pipeline.last.reference.refId_fromCoo(1,1)
    %          Ind=pipeline.last.reference.refId_fromCoo('M51')

    arguments
        RA
        Dec                    = [];
        Args.RefTable          = [];
        Args.RefTableName      = 'LAST_RefIm_Grid.mat';

        Args.InUnits           = 'deg';
        Args.Server            = @VO.name.server_simbad;

        Args.InitSearchRadius  = 1;
    end
    RAD = 180./pi;

    InitSearchRadius = convert.angular(Args.InUnits, 'rad', Args.InitSearchRadius);
    [RA, Dec] = celestial.convert.cooResolve(RA, Dec, 'InUnits',Args.InUnits, 'OutUnits','rad', 'Server',Args.Server);

    if isempty(Args.RefTable)
        RefTbl = io.files.load2(Args.RefTableName);
    else
        RefTbl = Args.RefTable;
    end


    Dist = celestial.coo.sphere_dist_fast(RA, Dec, RefTbl.RA./RAD, RefTbl.Dec./RAD);
    Ind = find(Dist<InitSearchRadius);
    Dist = Dist(Ind);
    Nind = numel(Ind);
    IndFlag = false(Nind,1);
    for I=1:1:Nind
        IndC = Ind(I);
        PolyVert = [RefTbl.RA1(IndC), RefTbl.Dec1(IndC); RefTbl.RA2(IndC), RefTbl.Dec2(IndC); RefTbl.RA3(IndC), RefTbl.Dec3(IndC); RefTbl.RA4(IndC), RefTbl.Dec4(IndC)]./RAD;
        
        [PolyCooDirX, PolyCooDirY, PolyCooDirZ] = celestial.coo.coo2cosined(PolyVert(:,1), PolyVert(:,2));
        IndFlag(I)  = celestial.htm.in_polysphere([RA, Dec], [PolyCooDirX, PolyCooDirY, PolyCooDirZ]);
    end

    Ind = Ind(IndFlag);
    Dist = Dist(IndFlag);
    

end
