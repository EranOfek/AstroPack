function Cone = findCone(Cones, CatName, RA, Dec, Radius)
    % Find, among raw catalog cones, one that covers a circle on the sky.
    %   Raw cones are the 4th output of imProc.cat.getAstrometricCatalog
    %   (issue #1348). A consumer that would search the catalog over a
    %   circle can instead use a cone of the same catalog covering that
    %   circle, e.g. the cones kept by the astrometry of the visit
    %   (pipeline.last.pipes.pipelineI GaiaCone output). The cones are
    %   selected by coverage, so their order does not matter.
    % Input  : - A structure array of raw cones (fields .Cat and .Circle),
    %            or [].
    %          - Catalog name. Only cones whose .Cat.Name equals it are used.
    %          - Circle center R.A. [rad].
    %          - Circle center Dec. [rad].
    %          - Circle radius [rad].
    % Output : - The first covering cone (a scalar structure), or [] if none.
    % Author : Alexander Gioffe (Sep 2026)
    % Example: Cone = imProc.cat.findCone(GaiaCone, 'GAIADR3', RA, Dec, Radius);

    arguments
        Cones
        CatName
        RA(1,1) double
        Dec(1,1) double
        Radius(1,1) double
    end

    Cone = [];
    if ~isstruct(Cones) || isempty(Cones) || ~isfield(Cones, 'Cat')
        return;
    end

    Ncone = numel(Cones);
    for Icone=1:1:Ncone
        if isempty(Cone) && ~isempty(Cones(Icone).Cat) && strcmp(char(Cones(Icone).Cat.Name), char(CatName)) && ...
                imProc.cat.coneCovers(Cones(Icone), RA, Dec, Radius)
            Cone = Cones(Icone);
        end
    end
end
