function Dist = sphere_dist_fastThresh(RA1, Dec1, RA2, Dec2, Thresh)
    % Calculate angular distances only for sources with Dec diff below threshold.
    %       This function is somewhat faster than
    %       celestial.coo.sphere_dist_fast.
    % Input  : - RA1 [rad]
    %          - Dec1 [rad]
    %          - RA2 [rad]
    %          - Dec2 [rad]
    %          - Threshold [rad]. Calculate angular distances only for
    %            sources which Declination difference is smaller than this
    %            number.
    % Output : - Angualr distance between points [radians].
    %            Sources with abs(DecDiff)>Thresh are populated with Inf.
    % Author : Eran Ofek (Dec 2021)
    % Example: R1=rand(1000,1)./RAD; R2=rand(1000,1)./RAD;
    %          D1=rand(1000,1)./RAD; D2=rand(1000,1)./RAD;
    %          Dist=celestial.coo.sphere_dist_fastThresh(R1,D1,R2,D2,0.003);
    
    Flag = abs(Dec1 - Dec2)<Thresh;

    Dist = inf(size(Flag));
  
    if numel(Dec1)==1
        Flag1 = true;
    else
        Flag1 = Flag;
    end
    if numel(Dec2)==1
        Flag2 = true;
    else
        Flag2 = Flag;
    end
    
    Dist(Flag) = acos(sin(Dec1(Flag1)).*sin(Dec2(Flag2)) + cos(Dec1(Flag1)).*cos(Dec2(Flag2)).*cos(RA1(Flag1)-RA2(Flag2)));
end