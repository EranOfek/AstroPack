function Result = addFcontGAIADR3spec(OutDir, Args)
    % Append the Gaia contamination column FCONT to GAIADR3spec in one pass (same name).
    % Description: For every GAIADR3spec source, FCONT is the light of its
    %              Gaia DR3 neighbors within 30" falling into the LAST
    %              photometric aperture, relative to its own, for a fixed
    %              Gaussian PSF (VO.prep.GAIA.dr3.gaiaFcont). The calibrator
    %              audit of PhotCalibTrans rejects calibrators with a large
    %              FCONT (issue #1454). The column is appended after the
    %              existing ones (position 701 of the 700-column catalog).
    %
    %              Mechanics are delegated to catsHTM.insertColumns, as in
    %              VO.prep.GAIA.dr3.mergeGAIADR3spec: BaseDir is read-only,
    %              the rewritten catalog is written under OutDir with the same
    %              name, and the HTM index copies over unchanged. Move OutDir
    %              to the production mount manually afterwards.
    %
    %              GAIADR3 must be resolvable by catsHTM (on the path).
    % Input  : - OutDir : Writable directory where the rewritten
    %                     GAIADR3spec is placed.
    %          * ...,key,val,...
    %            'CatName'     - Catalog to enrich. Default 'GAIADR3spec'.
    %            'MagCol'      - G magnitude column of CatName.
    %                            Default 'phot_g_mean_mag'.
    %            'ColName'     - Name of the new column. Default 'FCONT'.
    %            'FWHM'        - PSF FWHM [arcsec]. Default 5.
    %            'AperRad'     - Aperture radius [arcsec]. Default 7.5.
    %            'NeighRad'    - Neighbor radius [arcsec]. Default 30.
    %            'GaiaCatName' - catsHTM of the neighbors. Default 'GAIADR3'.
    %            'BaseDir'     - Source catsHTM root (read-only). Default ''
    %                            (resolved by catsHTM.insertColumns).
    %            'CatRelDir'   - Catalog subdir under BaseDir. Default ''.
    %            'NfilesInHDF' - Cells per data file. Default 100.
    %            'SortCol'     - Existing SortCol (Dec). Default 2.
    %            'StepRows'    - save_cat index step. Default 30.
    %            'SkipExisting'- Resume: skip cells already written in OutDir.
    %                            Default false.
    %            'DryRun'      - List affected files, no writes. Default false.
    %            'Verbose'     - Print progress. Default true.
    % Output : - Result struct of catsHTM.insertColumns.
    % Author : D. Kovaleva (Oct 2026)
    % Example: R = VO.prep.GAIA.dr3.addFcontGAIADR3spec('/data/tmp/GAIADR3spec_fcont');

    arguments
        OutDir                 char
        Args.CatName           char    = 'GAIADR3spec'
        Args.MagCol            char    = 'phot_g_mean_mag'
        Args.ColName           char    = 'FCONT'
        Args.FWHM              double  = 5
        Args.AperRad           double  = 7.5
        Args.NeighRad          double  = 30
        Args.GaiaCatName       char    = 'GAIADR3'
        Args.BaseDir           char    = ''
        Args.CatRelDir         char    = ''
        Args.NfilesInHDF       double  = 100
        Args.SortCol           double  = 2
        Args.StepRows          double  = 30
        Args.SkipExisting      logical = false
        Args.DryRun            logical = false
        Args.Verbose           logical = true
    end

    ColCell = catsHTM.load_colcell(Args.CatName);
    MagIdx  = find(strcmpi(ColCell, Args.MagCol), 1);
    if isempty(MagIdx)
        error('VO:prep:GAIA:dr3:addFcontGAIADR3spec:MissingMagCol', ...
            'Column "%s" not found in %s.', Args.MagCol, Args.CatName);
    end
    if any(strcmpi(ColCell, Args.ColName))
        error('VO:prep:GAIA:dr3:addFcontGAIADR3spec:ColumnExists', ...
            'Column "%s" already exists in %s.', Args.ColName, Args.CatName);
    end

    FcontArgs = {'FWHM',Args.FWHM, 'AperRad',Args.AperRad, 'NeighRad',Args.NeighRad, ...
                 'CatName',Args.GaiaCatName};
    FillFun = @(M) VO.prep.GAIA.dr3.gaiaFcont(M(:,1), M(:,2), M(:,MagIdx), FcontArgs{:});

    Result = catsHTM.insertColumns(Args.CatName, {Args.ColName}, {''}, OutDir, ...
        'BaseDir',     Args.BaseDir, ...
        'CatRelDir',   Args.CatRelDir, ...
        'FillValue',   FillFun, ...
        'Position',    'end', ...
        'SortCol',     Args.SortCol, ...
        'StepRows',    Args.StepRows, ...
        'NfilesInHDF', Args.NfilesInHDF, ...
        'SkipExisting',Args.SkipExisting, ...
        'DryRun',      Args.DryRun, ...
        'Verbose',     Args.Verbose);
end
