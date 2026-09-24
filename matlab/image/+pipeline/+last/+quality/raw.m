function [Result, Files] = raw(Dirs, Args)
    % Measure quality metrics of LAST RAW images and flag anomalous frames.
    %   Runs a set of cheap quality tests on every RAW image found in the
    %   requested directories (typically the 'failed' directories of the
    %   LAST archive) and returns one table row per image:
    %   FWHM (ACF based), background level, pixel variance, and the
    %   background/variance anomaly flags derived from them.
    %
    %   The tests follow those applied by pipeline.generic.prePrep, which
    %   is the code that moved these images into 'failed' in the first
    %   place, so the numbers here can be compared with the pipeline's own
    %   decisions.
    %
    %   By default the image is not read in full. Instead a small number of
    %   full-width horizontal bands are read (see 'Nband'/'BandHeight').
    %   Each band is a contiguous byte range of the FITS file, so it is
    %   cheap to read, and it covers both the light region and the overscan
    %   strip in a single read. Measured on a LAST node this is ~6 times
    %   faster than reading the whole frame (0.10 vs 0.62 s per file) while
    %   reproducing the full-frame background and variance to a few percent.
    %   Set 'FullFrame' to true to read the entire image instead.
    %
    %   The variance test compares the measured pixel variance in the light
    %   region with the variance expected from the overscan (read noise
    %   only) plus the Poisson noise of the sky:
    %       VarRatio = Var / (VarOver + Sky/Gain),  Sky = Back - BackOver
    %   For healthy frames this ratio is close to unity.
    %
    %   Note on the background: the variance and background are measured as
    %   the median over BlockSize x BlockSize blocks, and NOT over the
    %   region as a whole. A single robust variance of the whole light
    %   region is dominated by the large scale illumination gradient: on
    %   real frames it was found to scatter by a factor of ~3 between
    %   consecutive images of the same field, whereas the block median is
    %   stable to a few percent.
    %
    % Input  : - Directories to scan. Either a char array (optionally
    %            containing wild cards), a cell array of char arrays, or a
    %            string array.
    %            If empty, the directories are discovered automatically on
    %            the local machine, using the 'BasePath'/'DirTemplate'/
    %            'SubDir' arguments. The default discovery is anchored to
    %            the machine's own root directory (/<hostname>), because on
    %            a LAST node a /last*/ glob would silently pull in the
    %            partner machine's autofs-mounted disks and measure its
    %            images as well.
    %            Default is [].
    %          * ...,key,val,...
    %            'BasePath' - Root directory used when Dirs is empty.
    %                   If empty, use /<hostname -s>. Default is [].
    %            'ArchiveDir' - Name of the archive directory inside each
    %                   data disk. Default is 'archive'.
    %            'DataDirRegExp' - Regular expression the name of a data
    %                   disk directory must match. The default excludes the
    %                   plain 'data' directory that sits next to data1,
    %                   data2. Default is '^data\d+$'.
    %            'CamDirRegExp' - Regular expression the name of a camera
    %                   directory must match. The default excludes
    %                   reprocessing trees such as LAST.01.10.04_re.
    %                   Default is '^LAST\.\d\d\.\d\d\.\d\d$'.
    %            'SubDir' - Sub directory of the camera directory holding
    %                   the images. Default is 'failed'.
    %            'FileTemplate' - Image file template. Default is '*.fits'.
    %            'MaxFiles' - If not empty, measure at most this number of
    %                   images per directory, spread evenly over the
    %                   directory listing. Useful for calibration runs.
    %                   Default is [].
    %            'ListOnly' - Only discover the directories and their image
    %                   lists, without measuring anything. The first output
    %                   is then an empty table. Useful for driving a long
    %                   run camera by camera. Default is false.
    %            % ---------- geometry ----------
    %            'LIGHTSEC' - CCDSEC [Xmin Xmax Ymin Ymax] of the light
    %                   (illuminated) region. Default is [1 6388 25 9600].
    %            'OVERSCAN' - CCDSEC of the overscan region.
    %                   Default is [6389 6422 1 9600].
    %            'Nband' - Number of full-width horizontal bands to read.
    %                   Ignored if 'FullFrame' is true. Default is 3.
    %            'BandHeight' - Height [pix] of each band. Must be at least
    %                   2*ACF_HalfSize for the FWHM measurement.
    %                   Default is 1024.
    %            'FullFrame' - Read the entire image instead of bands.
    %                   Slower, and does not change the results materially.
    %                   Note that in this mode there is a single region, so
    %                   FWHM_Min, FWHM and FWHM_Max collapse to one value
    %                   and the FWHM spread diagnostic is not available.
    %                   Default is false.
    %            'BlockSize' - Size [pix] of the square blocks over which
    %                   the background and variance are measured before
    %                   taking their median. Default is 256.
    %            % ---------- measurements ----------
    %            'Gain' - Detector gain [e-/ADU]. If empty, take it from
    %                   the GAIN header keyword of each image.
    %                   Default is [].
    %            'ACF_HalfSize' - Half size [pix] of the square cutout used
    %                   for the ACF based FWHM. Default is 500.
    %            'MaxRadius' - Max radius [pix] for the ACF.
    %                   Default is 50.
    %            'ThresholdBack' - Pixel value above which a pixel counts
    %                   as "high" for the high-pixel-fraction test.
    %                   Default is 4000.
    %            'histAnomalyArgs' - Cell array of additional arguments for
    %                   imUtil.image.histAnomaly. Note that the light
    %                   region is passed to it already trimmed, so its
    %                   'CCDSEC' argument is neither needed nor used - this
    %                   is what avoids the overscan bias peak being
    %                   mistaken for a bimodality (issue #1216).
    %                   Default is {}.
    %            % ---------- anomaly thresholds ----------
    %            'MaxPixFraction' - Fraction of pixels above
    %                   'ThresholdBack' above which HighPixAnomaly is set.
    %                   Default is 0.4.
    %            'SkyRange' - [Min Max] allowed sky level [ADU] above the
    %                   overscan. Outside this range BackLevelAnomaly is
    %                   set. If empty, the test is not applied.
    %                   Default is [] - see the calibration note below.
    %            'VarRatioRange' - [Min Max] allowed VarRatio. Outside this
    %                   range VarAnomaly is set. If empty, the test is not
    %                   applied. Default is [0.5 5].
    %
    %            Calibration of the two ranges above, measured on 880 RAW
    %            images from the 'failed' directories of six mounts and 880
    %            from their 'new' directories (2026 Aug):
    %              VarRatio is bimodal - a narrow peak at 1.08 (90% of the
    %                images are below 1.4) and a well separated tail above
    %                ~5. Any threshold between 2 and 5 selects the same
    %                images, which is why the default is insensitive to its
    %                exact value.
    %              Sky is continuous and depends on the site, the season
    %                and the moon, so no default is imposed. For reference,
    %                Sky>2000 selected 6.7% of the 'failed' images and 1.0%
    %                of the 'new' ones, Sky>5000 selected 3.1% and 0.4%.
    %              Note that on that sample the variance test selected no
    %                image that the background tests had not already
    %                selected: it is a consistency check on the noise
    %                model rather than an independent discriminant.
    %            % ---------- output ----------
    %            'OutFile' - If not empty, write the result table to this
    %                   tab separated text file. Default is ''.
    %            'OutColumns' - Cell array of column names to write to
    %                   'OutFile'. If empty, write all columns. The set
    %                   requested for a compact report is
    %                   {'Mount','Camera','FileName','FWHM','Back','Var',
    %                    'BackAnomaly','VarAnomaly'}. Default is {}.
    %            'CheckpointEvery' - Rewrite 'OutFile' with the rows
    %                   measured so far every this many images. 0 disables
    %                   it, and the file is written only at the end.
    %                   A single camera can hold tens of thousands of
    %                   images (one of them holds ~33000, some 12 hours of
    %                   measurement), so without a checkpoint an
    %                   interruption loses the whole camera.
    %                   Default is 0.
    %            'Verbosity' - 0 - silent, 1 - one line per directory,
    %                   2 - one line per image. Default is 1.
    % Output : - A table with one row per image. Columns:
    %            ProjName, Node, Mount, Camera - identification, taken from
    %              the *directory* path, never from the file name (the
    %              camera field of a LAST file name is occasionally empty).
    %            Path, FileName - location of the image.
    %            JD, ExpTime, Gain - from the header.
    %            FWHM, FWHM_Min, FWHM_Max - ACF based FWHM in PIXELS: the
    %              median over the bands, and its range. A large spread
    %              indicates the value is not trustworthy (e.g., a
    %              satellite streak crossing one band). This is the same
    %              measure used by pipeline.generic.prePrep, which rejects
    %              images above 5 pixels. On the calibration sample the
    %              median was 2.2 pix and 5.5% of the 'failed' images were
    %              above 8 pix.
    %            Back, BackOver, Sky - background of the light region, of
    %              the overscan, and their difference [ADU].
    %            Var, VarOver, VarRatio - variance of the light region, of
    %              the overscan [ADU^2], and their ratio (see above).
    %            FracPixAbove - fraction of light pixels above
    %              'ThresholdBack'.
    %            HistAnomaly, HighPixAnomaly, BackLevelAnomaly - the three
    %              individual background tests (1/0).
    %            BackAnomaly - 1 if any of the three above is 1.
    %            VarAnomaly - 1 if VarRatio is outside 'VarRatioRange'.
    %            Error - error message if the image could not be measured,
    %              empty otherwise. Such rows carry NaN measurements and
    %              have all anomaly flags set to NaN.
    %          - A structure array of the directories that were scanned,
    %            with the number of files found in each.
    % Author : Sasha (2026 Aug)
    % Example: T = pipeline.last.quality.raw;   % all failed dirs of this node
    %          T = pipeline.last.quality.raw('/last01e/data1/archive/LAST.01.01.01/failed', 'OutFile','q.txt');

    arguments
        Dirs                      = [];

        Args.BasePath             = [];
        Args.ArchiveDir           = 'archive';
        Args.DataDirRegExp        = '^data\d+$';
        Args.CamDirRegExp         = '^LAST\.\d\d\.\d\d\.\d\d$';
        Args.SubDir               = 'failed';
        Args.FileTemplate         = '*.fits';
        Args.MaxFiles             = [];
        Args.ListOnly logical     = false;

        Args.LIGHTSEC             = [1 6388 25 9600];
        Args.OVERSCAN             = [6389 6422 1 9600];
        Args.Nband                = 3;
        Args.BandHeight           = 1024;
        Args.FullFrame logical    = false;
        Args.BlockSize            = 256;

        Args.Gain                 = [];
        Args.ACF_HalfSize         = 500;
        Args.MaxRadius            = 50;
        Args.ThresholdBack        = 4000;
        Args.histAnomalyArgs      = {};

        Args.MaxPixFraction       = 0.4;
        Args.SkyRange             = [];
        Args.VarRatioRange        = [0.5 5];

        Args.OutFile              = '';
        Args.OutColumns           = {};
        Args.CheckpointEvery      = 0;
        Args.Verbosity            = 1;
    end

    % ---- resolve the list of directories ----
    Files = findImageDirs(Dirs, Args);
    Ndir  = numel(Files);
    if Ndir==0
        warning('pipeline:last:quality:raw:noDirs', 'No image directories found');
        Result = emptyResultTable;
        return
    end

    if Args.ListOnly
        Result = emptyResultTable;
        return
    end

    % ---- measure ----
    Rows = cell(Ndir,1);
    for Idir=1:1:Ndir
        Nim = numel(Files(Idir).List);
        if Args.Verbosity>0
            fprintf('pipeline.last.quality.raw: %s (%d images)\n', Files(Idir).Dir, Nim);
        end

        R = repmat(emptyRow, Nim, 1);
        for Iim=1:1:Nim
            FileName = Files(Idir).List(Iim).name;
            FullName = fullfile(Files(Idir).List(Iim).folder, FileName);

            R(Iim).ProjName = string(Files(Idir).ProjName);
            R(Iim).Node     = Files(Idir).Node;
            R(Iim).Mount    = Files(Idir).Mount;
            R(Iim).Camera   = Files(Idir).Camera;
            R(Iim).Path     = string(Files(Idir).List(Iim).folder);
            R(Iim).FileName = string(FileName);

            % Pipeline style: a single unreadable or corrupted image must not
            % stop a run over ~10^5 files. Record the error in the table and
            % carry on; such rows keep NaN measurements and NaN flags.
            try
                R(Iim) = measureImage(R(Iim), FullName, Args);
            catch ME
                R(Iim).Error = string(ME.message);
            end

            if Args.Verbosity>1
                fprintf('   %s FWHM=%.2f Back=%.1f Var=%.1f BackAnom=%d VarAnom=%d %s\n', ...
                        FileName, R(Iim).FWHM, R(Iim).Back, R(Iim).Var, ...
                        R(Iim).BackAnomaly, R(Iim).VarAnomaly, R(Iim).Error);
            end

            if Args.CheckpointEvery>0 && ~isempty(Args.OutFile) && mod(Iim, Args.CheckpointEvery)==0
                writeResultTable(struct2table(cat(1, Rows{1:Idir-1}, R(1:Iim))), Args);
            end
        end
        Rows{Idir} = R;
    end

    Result = struct2table(cat(1, Rows{:}));

    if Args.Verbosity>0
        Nerr = sum(strlength(Result.Error)>0);
        fprintf('pipeline.last.quality.raw: %d images measured, %d errors, %d background anomalies, %d variance anomalies\n', ...
                height(Result), Nerr, sum(Result.BackAnomaly==1), sum(Result.VarAnomaly==1));
    end

    % ---- write table ----
    if ~isempty(Args.OutFile)
        writeResultTable(Result, Args);
        if Args.Verbosity>0
            fprintf('pipeline.last.quality.raw: table written to %s\n', Args.OutFile);
        end
    end

end

% ------------------------------------------------------------------------
function writeResultTable(T, Args)
    % Write the result table, restricted to the requested columns.
    if ~isempty(Args.OutColumns)
        T = T(:, Args.OutColumns);
    end
    writetable(T, Args.OutFile, 'Delimiter','tab', 'FileType','text');
end

% ------------------------------------------------------------------------
function Row = measureImage(Row, FullName, Args)
    % Measure all quality metrics of a single RAW image.

    LS = Args.LIGHTSEC;
    OS = Args.OVERSCAN;

    % header first: Gain is needed for the variance test
    HeadCell = FITS.readHeader1(FullName, 1);
    AH       = AstroHeader;
    AH.Data  = HeadCell;
    Row.JD      = AH.julday;
    Row.ExpTime = AH.getVal('EXPTIME');
    if isempty(Args.Gain)
        Row.Gain = AH.getVal('GAIN');
    else
        Row.Gain = Args.Gain;
    end

    % ---- read the image, either in bands or in full ----
    if Args.FullFrame
        Im    = single(FITS.read1(FullName, 1));
        Light = {Im(LS(3):LS(4), LS(1):LS(2))};
        Over  = {Im(OS(3):OS(4), OS(1):OS(2))};
    else
        Yc    = bandCenters(Args.Nband, Args.BandHeight, LS);
        Light = cell(Args.Nband,1);
        Over  = cell(Args.Nband,1);
        for Ib=1:1:Args.Nband
            Y1  = Yc(Ib) - Args.BandHeight./2;
            Y2  = Yc(Ib) + Args.BandHeight./2 - 1;
            Bnd = single(FITS.read1(FullName, 1, 'CCDSEC',[1 OS(2) Y1 Y2]));
            Light{Ib} = Bnd(:, LS(1):LS(2));
            Over{Ib}  = Bnd(:, OS(1):OS(2));
        end
    end
    Nb = numel(Light);

    % ---- per band statistics ----
    BackB   = nan(Nb,1);
    BackOB  = nan(Nb,1);
    FWHM_B  = nan(Nb,1);
    FracB   = nan(Nb,1);
    HistB   = false(Nb,1);
    VarList  = [];
    VarOList = [];
    for Ib=1:1:Nb
        BackB(Ib)  = median(Light{Ib}, 'all', 'omitnan');
        BackOB(Ib) = median(Over{Ib},  'all', 'omitnan');

        VarList  = [VarList;  blockStat(Light{Ib}, [Args.BlockSize Args.BlockSize])]; %#ok<AGROW>
        VarOList = [VarOList; blockStat(Over{Ib},  [Args.BlockSize size(Over{Ib},2)])]; %#ok<AGROW>

        FracB(Ib) = sum(Light{Ib}(:)>Args.ThresholdBack)./numel(Light{Ib});

        % The light region is passed already trimmed - this is the point of
        % issue #1216: histogramming the frame together with its overscan
        % strip makes the bias peak look like a second mode and rejects
        % perfectly healthy frames.
        HistB(Ib) = imUtil.image.histAnomaly(Light{Ib}, Args.histAnomalyArgs{:});

        FWHM_B(Ib) = fwhmFromBand(Light{Ib}, BackB(Ib), Args);
    end

    Row.Back     = median(BackB);
    Row.BackOver = median(BackOB);
    Row.Sky      = Row.Back - Row.BackOver;
    Row.Var      = median(VarList,  'omitnan');
    Row.VarOver  = median(VarOList, 'omitnan');

    % The ACF FWHM of a single 1000x1000 cutout is noisy - a satellite
    % streak or a sparse field can shift it by a factor of ~2. Take the
    % median over the bands and report the spread so a suspicious value
    % can be recognised.
    Row.FWHM     = median(FWHM_B, 'omitnan');
    Row.FWHM_Min = min(FWHM_B);
    Row.FWHM_Max = max(FWHM_B);

    Row.FracPixAbove = max(FracB);

    % expected variance: read noise (from the overscan) + sky Poisson noise
    if isfinite(Row.Gain) && Row.Gain>0
        VarExp       = Row.VarOver + max(Row.Sky,0)./Row.Gain;
        Row.VarRatio = Row.Var./VarExp;
    else
        Row.VarRatio = NaN;
    end

    % ---- anomaly flags ----
    Row.HistAnomaly    = double(any(HistB));
    Row.HighPixAnomaly = double(Row.FracPixAbove > Args.MaxPixFraction);
    if isempty(Args.SkyRange)
        Row.BackLevelAnomaly = 0;
    else
        Row.BackLevelAnomaly = double(Row.Sky<Args.SkyRange(1) | Row.Sky>Args.SkyRange(2));
    end
    Row.BackAnomaly = double(Row.HistAnomaly==1 | Row.HighPixAnomaly==1 | Row.BackLevelAnomaly==1);

    if isempty(Args.VarRatioRange) || ~isfinite(Row.VarRatio)
        Row.VarAnomaly = 0;
    else
        Row.VarAnomaly = double(Row.VarRatio<Args.VarRatioRange(1) | Row.VarRatio>Args.VarRatioRange(2));
    end
end

% ------------------------------------------------------------------------
function FWHM = fwhmFromBand(Light, Back, Args)
    % ACF based FWHM from a square cutout at the centre of a band.
    HS = Args.ACF_HalfSize;
    [Ny, Nx] = size(Light);
    if Ny<2*HS || Nx<2*HS
        HS = floor(min(Ny,Nx)./2);
    end
    Cy  = round(Ny./2);
    Cx  = round(Nx./2);
    Sub = Light(Cy-HS+1:Cy+HS, Cx-HS+1:Cx+HS) - Back;

    FWHM = imUtil.psf.fwhm_fromACF(Sub, 'CCDSEC',[], 'MaxRadius',Args.MaxRadius, ...
                                   'UseMex',true, 'Back',[]);
end

% ------------------------------------------------------------------------
function VL = blockStat(Im, Blk)
    % Robust variance per block. Blocks are used instead of the region as a
    % whole so that the large scale illumination gradient does not inflate
    % the result. Partial blocks at the edges are dropped, so that every
    % block carries the same number of pixels and the median is unbiased.
    Blk = min(Blk, size(Im));
    Nby = floor(size(Im,1)./Blk(1));
    Nbx = floor(size(Im,2)./Blk(2));
    VL  = nan(Nby.*Nbx, 1);
    K   = 0;
    for Iy=1:1:Nby
        for Ix=1:1:Nbx
            K = K + 1;
            VL(K) = clipVar(Im((Iy-1).*Blk(1)+1:Iy.*Blk(1), (Ix-1).*Blk(2)+1:Ix.*Blk(2)));
        end
    end
end

% ------------------------------------------------------------------------
function V = clipVar(X)
    % Sigma clipped variance. A plain IQR based variance is quantised on
    % integer RAW data (the overscan IQR is only ~5 ADU, so its variance can
    % take only a few discrete values); clipping the outliers and taking the
    % ordinary variance of the rest is continuous and still robust to stars
    % and cosmic rays.
    X  = X(:);
    X  = X(isfinite(X));
    if isempty(X)
        V = NaN;
        return
    end
    Med = median(X);
    Sig = 1.4826.*median(abs(X-Med));
    if Sig<=0
        V = var(X);
    else
        V = var(X(abs(X-Med) < 3.*Sig));
    end
end

% ------------------------------------------------------------------------
function Yc = bandCenters(Nband, BandHeight, LIGHTSEC)
    % Centres of the horizontal bands, spread over the light region and kept
    % far enough from its edges to hold a full band.
    Half = BandHeight./2;
    Y1   = LIGHTSEC(3) + Half;
    Y2   = LIGHTSEC(4) - Half;
    if Nband==1
        Yc = round((Y1+Y2)./2);
    else
        Yc = round(linspace(Y1, Y2, Nband));
    end
end

% ------------------------------------------------------------------------
function Files = findImageDirs(Dirs, Args)
    % Build the list of directories to scan, and their image lists.

    if isempty(Dirs)
        % Discovery is anchored to this machine's own root: on a LAST node a
        % /last*/ glob triggers the autofs mounts of the partner machine and
        % would silently include its cameras as well.
        BasePath = Args.BasePath;
        if isempty(BasePath)
            [Status, Host] = system('hostname -s');
            if Status~=0
                error('pipeline:last:quality:raw:hostname', 'Could not determine the host name; supply Dirs or BasePath');
            end
            BasePath = fullfile(filesep, strtrim(Host));
        end
        % The two levels are walked explicitly and filtered by regular
        % expression rather than by a dir() glob: dir() understands only
        % '*' and '?', and the directories that must be excluded cannot be
        % excluded by a glob anyway - the data disks sit next to a plain
        % 'data' directory, and the camera directories next to
        % reprocessing trees such as LAST.01.10.04_re.
        Data = listDirs(BasePath, 'data*', Args.DataDirRegExp);
        Cand = {};
        for Id=1:1:numel(Data)
            Cam  = listDirs(fullfile(Data{Id}, Args.ArchiveDir), 'LAST.*', Args.CamDirRegExp);
            Cand = [Cand, fullfile(Cam, Args.SubDir)]; %#ok<AGROW>
        end
    else
        if ischar(Dirs)
            Dirs = {Dirs};
        end
        Dirs = cellstr(Dirs);
        Cand = {};
        for I=1:1:numel(Dirs)
            if contains(Dirs{I}, {'*','?','['})
                DirList = dir(Dirs{I});
                DirList = DirList([DirList.isdir]);
                Cand    = [Cand, fullfile({DirList.folder}, {DirList.name})]; %#ok<AGROW>
            else
                Cand = [Cand, Dirs(I)]; %#ok<AGROW>
            end
        end
    end

    Files = struct('Dir',{}, 'ProjName',{}, 'Node',{}, 'Mount',{}, 'Camera',{}, 'List',{});
    for I=1:1:numel(Cand)
        List = [];
        if isfolder(Cand{I})
            List = dir(fullfile(Cand{I}, Args.FileTemplate));
            List = List(~[List.isdir]);
        end

        if ~isempty(List)
            if ~isempty(Args.MaxFiles) && numel(List)>Args.MaxFiles
                List = List(round(linspace(1, numel(List), Args.MaxFiles)));
            end

            % The node/mount/camera are taken from the directory path. The
            % camera field of a LAST file name is sometimes empty, so the
            % path is the only reliable source.
            [ProjName, Node, Mount, Camera] = parseCamPath(Cand{I});

            Files(end+1) = struct('Dir',Cand{I}, 'ProjName',ProjName, 'Node',Node, ...
                                  'Mount',Mount, 'Camera',Camera, 'List',List); %#ok<AGROW>
        end
    end
end

% ------------------------------------------------------------------------
function List = listDirs(Parent, Glob, RegExp)
    % Sub directories of Parent matching Glob whose *name* also matches
    % RegExp. Returns a cell array of full paths.
    D    = dir(fullfile(Parent, Glob));
    D    = D([D.isdir]);
    Keep = ~cellfun(@isempty, regexp({D.name}, RegExp, 'once'));
    List = fullfile({D(Keep).folder}, {D(Keep).name});
end

% ------------------------------------------------------------------------
function [ProjName, Node, Mount, Camera] = parseCamPath(Path)
    % Extract LAST.<node>.<mount>.<camera> from a directory path.
    Tok = regexp(Path, 'LAST\.(\d\d)\.(\d\d)\.(\d\d)', 'tokens', 'once');
    if numel(Tok)<3
        ProjName = '';
        Node     = NaN;
        Mount    = NaN;
        Camera   = NaN;
    else
        ProjName = sprintf('LAST.%s.%s.%s', Tok{1}, Tok{2}, Tok{3});
        Node     = str2double(Tok{1});
        Mount    = str2double(Tok{2});
        Camera   = str2double(Tok{3});
    end
end

% ------------------------------------------------------------------------
function Row = emptyRow
    % One result row with all measurements unset.
    Row = struct('ProjName',"", 'Node',NaN, 'Mount',NaN, 'Camera',NaN, ...
                 'Path',"", 'FileName',"", ...
                 'JD',NaN, 'ExpTime',NaN, 'Gain',NaN, ...
                 'FWHM',NaN, 'FWHM_Min',NaN, 'FWHM_Max',NaN, ...
                 'Back',NaN, 'BackOver',NaN, 'Sky',NaN, ...
                 'Var',NaN, 'VarOver',NaN, 'VarRatio',NaN, ...
                 'FracPixAbove',NaN, ...
                 'HistAnomaly',NaN, 'HighPixAnomaly',NaN, 'BackLevelAnomaly',NaN, ...
                 'BackAnomaly',NaN, 'VarAnomaly',NaN, ...
                 'Error',"");
end

% ------------------------------------------------------------------------
function T = emptyResultTable
    T = struct2table(emptyRow);
    T(1,:) = [];
end
