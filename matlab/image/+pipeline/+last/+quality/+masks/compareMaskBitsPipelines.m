function Report = compareMaskBitsPipelines(OldPath, NewPath, Args)
    % Compare aggregate per-bit mask statistics between two pipeline outputs
    % Description: Compares two pipeline outputs at the bitmask-aggregate
    %              level, WITHOUT pairing individual files. It runs
    %              pipeline.last.quality.masks.maskBitStatistics on each of
    %              OldPath and NewPath (all '*<FileType>_Mask_*.fits' files
    %              found there), then joins the two per-bit Aggregate tables
    %              on BitName to produce a per-bit comparison.
    %
    %              Use this when file-by-file pairing between the two
    %              pipelines is unreliable (e.g. differing timestamps or
    %              filename token counts) and only the aggregate bitmask
    %              behaviour matters. For pixel-level paired comparison use
    %              pipeline.last.quality.masks.compareMasksPipelines instead.
    %
    %              Note on metrics: TotalBitPix and DiffCount are raw pixel
    %              counts and therefore scale with the number of files in
    %              each folder. OverallFrac (= TotalPixelsWithBit/TotalPixels)
    %              and the derived DiffFrac / RatioFrac are normalised and so
    %              are the file-count-independent metrics to trust when the
    %              two folders hold different numbers of files.
    % Input  : - OldPath: directory containing old-pipeline mask FITS files.
    %          - NewPath: directory containing new-pipeline mask FITS files.
    %          * ...,key,val,...
    %            'FileType'     - File type pattern, globbed as
    %                             '*<FileType>_Mask_*.fits'. Default 'sci_coadd'.
    %            'BitNames'     - Cell array of bit names to compare. If empty,
    %                             all bits in the dictionary. Default {}.
    %            'BitDictName'  - BitDictionary name. Default
    %                             'BitMask.Image.Default'.
    %            'VisitPattern' - If non-empty, restrict the scan to visit
    %                             subdirectories matching this glob (e.g.
    %                             '*v[0-9]*'). Passed through to
    %                             maskBitStatistics. Default ''.
    %            'Recursive'    - Recurse into subdirectories (used only when
    %                             VisitPattern is empty). Default true.
    %            'FieldId'      - Keep only files whose AstroFileName.FieldID
    %                             matches. Char/string/numeric. Default ''
    %                             (no filter).
    %            'CropID'       - Keep only files whose AstroFileName.CropID
    %                             slot matches. Integer scalar; formatted to
    %                             '%03d' internally. Default [] (no filter).
    %            'ReportFile'   - Optional .mat path to save the report.
    %                             Default '' (no save).
    %            'Verbose'      - Print progress. Default true.
    % Output : - Report: struct with fields:
    %            .Timestamp   - datetime of the run
    %            .OldPath     - old pipeline path
    %            .NewPath     - new pipeline path
    %            .FileType    - file type pattern used
    %            .OldReport   - full maskBitStatistics report for OldPath
    %            .NewReport   - full maskBitStatistics report for NewPath
    %            .Comparison  - per-bit comparison table with columns:
    %                           BitName, BitIndex,
    %                           NumFiles_Old, NumFiles_New,
    %                           TotalBitPix_Old, TotalBitPix_New, DiffCount,
    %                           OverallFrac_Old, OverallFrac_New,
    %                           DiffFrac, RatioFrac
    %                           where Diff = New - Old and Ratio = New / Old.
    %            .Summary     - text summary
    % Author : Dana Kovaleva (May 2026)
    % Example: OldPath = '/bigdata2/projects/last/testNewPipe/222635v0/';
    %          NewPath = '/bigdata2/projects/last/testNewPipe/222625v1/';
    %          Report = pipeline.last.quality.masks.compareMaskBitsPipelines( ...
    %                       OldPath, NewPath, 'FileType', 'sci_coadd');
    %          disp(Report.Comparison);

    arguments
        OldPath          (1,:) char
        NewPath          (1,:) char
        Args.FileType    (1,:) char   = 'sci_coadd'
        Args.BitNames    cell         = {}
        Args.BitDictName (1,:) char   = 'BitMask.Image.Default'
        Args.VisitPattern (1,:) char  = ''
        Args.Recursive   (1,1) logical = true
        Args.FieldId                  = ''
        Args.CropID      double {mustBeInteger, mustBeNonnegative} = []
        Args.ReportFile  (1,:) char   = ''
        Args.Verbose     (1,1) logical = true
    end

    % --- Run maskBitStatistics on each pipeline output ------------------
    MBSArgs = {'FileType',     Args.FileType, ...
               'BitNames',     Args.BitNames, ...
               'BitDictName',  Args.BitDictName, ...
               'VisitPattern', Args.VisitPattern, ...
               'Recursive',    Args.Recursive, ...
               'FieldId',      Args.FieldId, ...
               'CropID',       Args.CropID, ...
               'Verbose',      Args.Verbose};

    if Args.Verbose
        fprintf('[compareMaskBitsPipelines] OLD scan: %s\n', OldPath);
    end
    OldReport = pipeline.last.quality.masks.maskBitStatistics(OldPath, MBSArgs{:});

    if Args.Verbose
        fprintf('[compareMaskBitsPipelines] NEW scan: %s\n', NewPath);
    end
    NewReport = pipeline.last.quality.masks.maskBitStatistics(NewPath, MBSArgs{:});

    % --- Join per-bit Aggregate tables on BitName -----------------------
    O = OldReport.Aggregate(:, {'BitName','BitIndex','NumFiles', ...
                                'TotalPixelsWithBit','OverallFraction'});
    O.Properties.VariableNames = {'BitName','BitIndex','NumFiles_Old', ...
                                  'TotalBitPix_Old','OverallFrac_Old'};

    N = NewReport.Aggregate(:, {'BitName','NumFiles', ...
                                'TotalPixelsWithBit','OverallFraction'});
    N.Properties.VariableNames = {'BitName','NumFiles_New', ...
                                  'TotalBitPix_New','OverallFrac_New'};

    % outerjoin so a bit present in only one dictionary still appears
    % (with NaN on the missing side) rather than being silently dropped.
    Comparison = outerjoin(O, N, 'Keys', 'BitName', 'MergeKeys', true);

    % --- Derived per-bit differences ------------------------------------
    Comparison.DiffCount = Comparison.TotalBitPix_New - Comparison.TotalBitPix_Old;
    Comparison.DiffFrac  = Comparison.OverallFrac_New - Comparison.OverallFrac_Old;
    Comparison.RatioFrac = Comparison.OverallFrac_New ./ Comparison.OverallFrac_Old;

    Comparison = Comparison(:, {'BitName','BitIndex', ...
        'NumFiles_Old','NumFiles_New', ...
        'TotalBitPix_Old','TotalBitPix_New','DiffCount', ...
        'OverallFrac_Old','OverallFrac_New','DiffFrac','RatioFrac'});
    Comparison = sortrows(Comparison, 'BitIndex');

    % --- Summary --------------------------------------------------------
    Summary = sprintf(['Mask Bit Aggregate Comparison\n', ...
                       '=============================\n', ...
                       'Old path : %s\n', ...
                       'New path : %s\n', ...
                       'File type: %s\n', ...
                       'Old files: %d   New files: %d\n', ...
                       'Bits compared: %d\n'], ...
                       OldPath, NewPath, Args.FileType, ...
                       OldReport.NumFiles, NewReport.NumFiles, ...
                       height(Comparison));

    % --- Assemble report ------------------------------------------------
    Report = struct();
    Report.Timestamp  = datetime('now');
    Report.OldPath    = OldPath;
    Report.NewPath    = NewPath;
    Report.FileType   = Args.FileType;
    Report.OldReport  = OldReport;
    Report.NewReport  = NewReport;
    Report.Comparison = Comparison;
    Report.Summary    = Summary;

    if Args.Verbose
        fprintf('\n%s\n', Summary);
        disp(Comparison);
    end

    if ~isempty(Args.ReportFile)
        save(Args.ReportFile, 'Report');
        if Args.Verbose
            fprintf('Report saved to: %s\n', Args.ReportFile);
        end
    end
end
