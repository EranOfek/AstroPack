function [Dist, Ang, PA] = sphere_dist_fast_threshDist(RA1, Dec1, RA2, Dec2, Thresh)
    % Fast spherical angular distance calculation with distance threshold (no calculation above threshold)
    %   The function is working only on RA/Dec vectors.
    % Input  : - Vector (only) of RA1 [rad]
    %          - Vector of Dec1 [rad]
    %          - Vector of RA2 [rad]
    %          - Vector of Dec2 [rad]
    %          - Angular threshold [rad]. Distances will be calculated only
    %            for points which abs(Dec2-Dec1)<Threshold.
    %            All the rest will return NaN.
    %            Default is 4.8481e-5;   % 10 [arcsec] in radians
    % Output : - Angular distance between points [rad].
    %            NaN if abs(Dec2-Dec1)>Threshold.
    % Author : Eran Ofek (2024 May) 
    % Example: [aRD,aP1,aP2]=celestial.coo.sphere_dist_fast_threshDist(R1,R2,R3,R4);

    arguments
        RA1
        Dec1
        RA2
        Dec2
        Thresh      = 4.8481e-5;   % 10 [arcsec] in radians
    end

    
    Flag       = find(abs(Dec2-Dec1)<Thresh);
    Dist       = nan(size(Flag));
    Dist(Flag) = celestial.coo.sphere_dist_fast(RA1(Flag),Dec1(Flag),RA2(Flag),Dec2(Flag));
    
    if nargout>1
        dRA = RA1(Flag)-RA2(Flag);
        SinPA = sin(dRA).*cos(Dec2(Flag))./sin(Dist(Flag));
        CosPA = (sin(Dec2(Flag)).*cos(Dec1(Flag)) - cos(Dec2(Flag)).*sin(Dec1(Flag)).*cos(dRA))./sin(Dist(Flag));

        Ang       = nan(size(Flag));
        Ang(Flag) = atan2(real(SinPA),real(CosPA));

        I     = Ang<0;
        Ang(I) = 2.*pi + Ang(I);

        if nargout>2
            PA       = nan(size(Flag));
            PA(Flag) = atan2(real(SinPA),-real(CosPA));
            PA(PA<0) = PA(PA<0) + 2.*pi;
        end
    end
end
