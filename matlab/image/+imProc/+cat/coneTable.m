function Tab = coneTable(Cones, Obj, Args)
    % RA, Dec and selected columns of raw catalog cones, as one table per cone.
    %   Raw cones are the 4th output of imProc.cat.getAstrometricCatalog
    %   (issue #1348), e.g. the cones kept by the astrometry of the visit
    %   (pipeline.last.pipes.pipelineI GaiaCone output). A consumer that
    %   needs only a few columns of them, e.g. the Gaia contamination audit
    %   of the photometric calibrators (issue #1454, 'AuditContamCat' of
    %   imProc.calib.fitPhotCalibTrans), gets a light table instead of the
    %   full cone. Works element by element on an array of any size.
    % Input  : - A structure array of raw cones (fields .Cat and .Circle),
    %            an AstroCatalog array, or [].
    %          - An AstroImage or AstroCatalog array with an element per
    %            cone, or []. If given, a cone is returned only if it covers
    %            the bounding circle of the sources of its element (an
    %            element without sky coordinates is not checked).
    %            Default is [].
    %          * ...,key,val,...
    %            'Cols' - Columns to return after RA and Dec.
    %                   Default is {'phot_g_mean_mag'}.
    %            'CooUnits' - Units of the RA, Dec columns. Default is 'deg'.
    % Output : - A cell array of the size of the cones, each element a table
    %            with RA, Dec and the Cols, or [] where the cone is empty,
    %            does not cover its element, or lacks one of the Cols.
    % Author : D. Kovaleva (Oct 2026)
    % Example: Tab = imProc.cat.coneTable(GaiaCone, Coadd);
    %          Tab = imProc.cat.coneTable(GaiaCone(3), Coadd(3), 'Cols',{'phot_g_mean_mag','bp_rp'});

    arguments
        Cones
        Obj                 = [];
        Args.Cols cell      = {'phot_g_mean_mag'};
        Args.CooUnits       = 'deg';
    end

    Tab = cell(size(Cones));
    if ~isempty(Obj) && numel(Obj)~=numel(Cones)
        error('The second input must have an element per cone');
    end

    Ncone = numel(Cones);
    for Icone=1:1:Ncone
        if isstruct(Cones)
            Cone = Cones(Icone);
            Cat  = [];
            if isfield(Cone, 'Cat')
                Cat = Cone.Cat;
            end
        else
            Cone = [];
            Cat  = Cones(Icone);
        end

        Use = ~isempty(Cat) && ~isemptyCatalog(Cat) && all(ismember(Args.Cols, Cat.ColNames));
        if Use && ~isempty(Obj) && isstruct(Cone)
            Use = covers(Cone, Obj(Icone));
        end

        if Use
            [RA, Dec] = Cat.getLonLat(Args.CooUnits);
            Tab{Icone} = [table(RA, Dec), array2table(double(Cat.getCol(Args.Cols)), 'VariableNames', Args.Cols)];
        end
    end
end


function Result = covers(Cone, Obj)
    % true if the cone covers the bounding circle of the sources of Obj, or Obj has no sky coordinates
    if isa(Obj, 'AstroImage')
        Cat = Obj.CatData;
    else
        Cat = Obj;
    end
    Result = true;
    if ~isemptyCatalog(Cat) && all(ismember({'RA','Dec'}, Cat.ColNames))
        [CircRA, CircDec, CircR] = Cat.boundingCircle('OutUnits','rad', 'CooType','sphere');
        if isfinite(CircRA) && isfinite(CircDec) && isfinite(CircR)
            Result = imProc.cat.coneCovers(Cone, CircRA, CircDec, CircR);
        end
    end
end
