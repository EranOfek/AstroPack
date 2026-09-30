function Result = coneCovers(Cone, RA, Dec, Radius)
    % Check whether a raw catalog cone covers a circle on the sky.
    %   A raw cone is the 4th output of imProc.cat.getAstrometricCatalog
    %   (issue #1348): a structure with the fields .Cat (AstroCatalog) and
    %   .Circle ([RA, Dec, Radius] of the searched circle, in rad).
    %   The cone can serve a consumer instead of a new catalog search if
    %   the consumer's circle lies entirely inside it.
    % Input  : - A raw cone structure, or [].
    %          - Circle center R.A. [rad].
    %          - Circle center Dec. [rad].
    %          - Circle radius [rad].
    % Output : - A logical: true if the cone is not empty and covers the
    %            circle.
    % Author : Alexander Gioffe (Sep 2026)
    % Example: [~,~,~,Cone] = imProc.cat.getAstrometricCatalog(1, 0.5, 'Radius',1800, 'CooUnits','rad');
    %          imProc.cat.coneCovers(Cone, 1, 0.5, 0.004)

    arguments
        Cone
        RA(1,1) double
        Dec(1,1) double
        Radius(1,1) double
    end

    Result = false;
    if isstruct(Cone) && isscalar(Cone) && isfield(Cone, 'Circle') && isfield(Cone, 'Cat') && ...
       ~isempty(Cone.Cat) && numel(Cone.Circle)==3
        Dist   = celestial.coo.sphere_dist_fast(RA, Dec, Cone.Circle(1), Cone.Circle(2));
        Result = (Dist + Radius) <= Cone.Circle(3);
    end
end
