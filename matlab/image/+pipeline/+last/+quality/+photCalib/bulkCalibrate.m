function bulkCalibrate(Args)
    % Bulk-calibrate a set of LAST coadd catalogs and write the calibrated
    % catalogs (FITS, header preserved) into an output directory.
    % Resumable: skips outputs that already exist (unless 'Force' is set).
    %
    % Each input _Cat_1.fits is paired with its _Image_1.fits, loaded via
    % AstroImage.readProducts, calibrated with imProc.calib.fitPhotCalibTrans,
    % and the calibrated CatData is written to OutDir under the same basename
    % with the original header attached. This function calibrates every input
    % regardless of AIRMASS or seeing — quality filtering (AIRMASS, FWHM, ...)
    % belongs to the downstream stability/MS step (e.g. stabilityN3), so a
    % "bad" visit's data never contaminates the calibration itself.
    %
    % Input  : * ...,key,val,...
    %            'InGlob'       - Shell glob expanded via `ls -1` to discover the
    %                             source _Cat_1.fits files. Ignored when
    %                             'CatListFile' is given.
    %                             Default is the LAST.01.05.03 / 1716.c / crop 10
    %                             coadd tree.
    %            'OutDir'       - Output directory for calibrated catalogs and the
    %                             discovered-catalog-list snapshot. Created if it
    %                             does not exist.
    %                             Default '/home/dana/tmp/N3/newpath_AtmSecond'.
    %            'FitArgs'      - Cell array of name-value arguments forwarded
    %                             verbatim to imProc.calib.fitPhotCalibTrans.
    %                             Set this to change the calibrated magnitude
    %                             column prefix ('MagColPrefix'; pass 'MAG_' to
    %                             overwrite the instrumental columns in place),
    %                             selection method, optimisation sequence, Tran2D
    %                             model, CalibArgs, etc.
    %                             Default: MagColPrefix='MAGAB__', AddMagErr=false,
    %                             computed airmass + nutation, and the current
    %                             fitPhotCalibTrans recipe (SelectionMethod='main',
    %                             RefSpecSlope=1.5, OptSeqName='LAST_Joint_2Iter_
    %                             AtmosFirst_Split3', Tran2DType='cheby1_1',
    %                             XPixel/YPixel from CCDSEC).
    %            'Force'        - Overwrite existing outputs. Default false.
    %            'CatListFile'  - When non-empty, load the catalog list from this
    %                             file (one absolute _Cat_1.fits path per line)
    %                             instead of globbing 'InGlob'. No snapshot is
    %                             written in this mode. Default '' (glob).
    % Output : - null (writes calibrated FITS files to OutDir).
    % Author : Dana Kovaleva
    % Example:
    %   % Defaults: glob the source tree, skip-if-exists, main/NonlinFC:
    %   pipeline.last.quality.photCalib.bulkCalibrate
    %
    %   % Point at a different field / crop and a different output directory:
    %   pipeline.last.quality.photCalib.bulkCalibrate( ...
    %       'InGlob', '/euclid/last/data/LAST.01.10.01/2025/0*/*/proc/*/LAST*_clear_351.a_*_001_004_sci_coadd_Cat_1.fits', ...
    %       'OutDir', '/home/dana/tmp/N3/field351a_crop4');
    %
    %   % Overwrite instrumental MAG_<suffix> columns in place (MagColPrefix
    %   % lives inside FitArgs — repeat the other defaults you want to keep):
    %   pipeline.last.quality.photCalib.bulkCalibrate( ...
    %       'FitArgs', {'MagColPrefix', 'MAG_', 'UseTypicalX', true, ...
    %                   'SelectionMethod','main', ...
    %                   'RefSpecSlope', 0, 'AddMagErr', false});
    %
    %   % Force a rerun with a linear optimisation sequence (Astropy-style):
    %   pipeline.last.quality.photCalib.bulkCalibrate('Force', true, ...
    %       'FitArgs', {'MagColPrefix', 'MAGAB__', 'UseTypicalX', true, 'SelectionMethod','main', ...
    %                   'RefSpecSlope', 1.5, 'AddMagErr', false, ...
    %                   'CalibArgs', {'AirmassSource','compute', 'ApplyNutation', true, ...
    %                                 'SelectionMethod','main', 'OptSeqName','LAST_NormLin_Astropy', ...
    %                                 'SigmaClipMethod','median', ...
    %                                 'XPixel', 1726, 'YPixel', 1726, 'Tran2DType','cheby1_4_xt'}});
    %
    %   % Reuse a previously saved catalog-list snapshot (skip the slow glob):
    %   pipeline.last.quality.photCalib.bulkCalibrate( ...
    %       'CatListFile', '/home/dana/tmp/N3/newpath_AtmSecond/catlist_20260716T101500.txt');

    arguments
        Args.InGlob       (1,:) char = '/euclid/last/data/LAST.01.05.03/2025/0*/*/proc/*/LAST*_clear_1716.c_*_001_010_sci_coadd_Cat_1.fits'
        Args.OutDir       (1,:) char = '/home/dana/tmp/N3/newpath_AtmSecond'
        Args.FitArgs      cell       = {'MagColPrefix', 'MAGAB__', ...
                                        'UseTypicalX', true, 'SelectionMethod','main', ...
                                        'RefSpecSlope', 1.5, 'AddMagErr', false, ...
                                        'CalibArgs', {'AirmassSource','compute', 'ApplyNutation', true, ...
                                                      'SelectionMethod','main', 'OptSeqName','LAST_Joint_2Iter_AtmosFirst_Split3', ...
                                                      'SigmaClipMethod','median', ...
                                                      'Tran2DType','cheby1_1', ...
                                                      'Tran2DPerturbStd', 1e-3, 'Tran2DRngSeed', 6}}
        Args.Force        logical    = false     % overwrite existing outputs
        Args.CatListFile  (1,:) char = ''        % load CatFiles from this snapshot instead of globbing
    end

    OutDir = Args.OutDir;
    if ~exist(OutDir, 'dir'); mkdir(OutDir); end

    if ~isempty(Args.CatListFile)
        if ~isfile(Args.CatListFile)
            error('bulkCalibrate:NoSuchList', ...
                'CatListFile not found: %s', Args.CatListFile);
        end
        Lines    = string(splitlines(fileread(Args.CatListFile)));
        CatFiles = strtrim(Lines);
        CatFiles = CatFiles(strlength(CatFiles) > 0);
        Nfile    = numel(CatFiles);
        fprintf('Loaded %d catalogs from %s\n', Nfile, Args.CatListFile);
    else
        [~, out] = system(['ls -1 ', Args.InGlob, ' 2>/dev/null']);
        CatFiles = strtrim(splitlines(string(out)));
        CatFiles = CatFiles(strlength(CatFiles) > 0);
        Nfile    = numel(CatFiles);
        fprintf('Found %d catalogs (glob)\n', Nfile);

        % Persist the discovered catalog list immediately, before any
        % calibration starts. Naming includes a UTC timestamp so reruns
        % accumulate snapshots instead of clobbering each other.
        Stamp    = datestr(now, 'yyyymmddTHHMMSS');                  %#ok<DATST,TNOW1>
        ListFile = fullfile(OutDir, ['catlist_', Stamp, '.txt']);
        Fid = fopen(ListFile, 'w');
        if Fid > 0
            fprintf(Fid, '%s\n', CatFiles);
            fclose(Fid);
            fprintf('Catalog list saved: %s (%d entries)\n', ListFile, Nfile);
        else
            warning('bulkCalibrate:ListFile', ...
                'Could not write %s — continuing without list snapshot', ListFile);
        end
    end

    for I = 1:Nfile
        CatFile        = char(CatFiles(I));
        ImageFile      = strrep(CatFile, '_Cat_', '_Image_');
        [~, Base, Ext] = fileparts(CatFile);
        OutFile        = fullfile(OutDir, [Base, Ext]);

        if isfile(OutFile) && ~Args.Force
            fprintf('[%4d/%d] skip : %s\n', I, Nfile, [Base, Ext]);
        else
            try
                AI = AstroImage.readProducts(ImageFile, 'ExtraOutProduct', "Cat");

                [Result, ~, FR] = imProc.calib.fitPhotCalibTrans(AI, Args.FitArgs{:});

                Result.CatData.write1(OutFile, ...
                    'FileType',  'fits', ...
                    'Header',    AI.HeaderData.Data, ...
                    'OverWrite', true);

                fprintf('[%4d/%d] done : %s  RMS=%.4f  NCal=%d\n', ...
                    I, Nfile, [Base, Ext], FR.RMS, FR.NCalUsed);
            catch ME
                fprintf('[%4d/%d] FAIL : %s -- %s\n', I, Nfile, [Base, Ext], ME.message);
            end
        end
    end
end
