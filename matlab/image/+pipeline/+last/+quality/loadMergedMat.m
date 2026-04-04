function MS = loadMergedMat(Args)
    % Load pre-built MergedMat HDF5 files into MatchedSources per crop
    % Description: Reads *_sci_merged_MergedMat_1.hdf5 files (one per crop)
    %              from a directory. These contain already cross-matched
    %              sources across epochs with relative photometry (no MAG_AB).
    %              The HDF5 datasets are [Nsrc x Nepochs]; they are transposed
    %              to [Nepochs x Nsrc] to match MatchedSources convention.
    %
    % Input  : * ...,key,val,...
    %            'MergedMatDir' - Directory with MergedMat HDF5 files. Required.
    %            'CropsToAnalyze' - Crop indices to load. Default is [] (all found).
    %            'Ncrop'     - Expected number of crops. Default is 24.
    %            'Mode'      - Mode name for the output struct. Default is 'percrop'.
    %            'Verbose'   - Print progress. Default is true.
    % Output : - MS struct with MS.(Mode){Ic} = MatchedSources.
    % Author : D. Kovaleva (Apr 2026)
    % Example: MS = pipeline.last.quality.loadMergedMat('MergedMatDir', '~/222635v0');
    %          pipeline.last.quality.plotPhotScatter(MS, 'Modes', {'percrop'}, ...
    %              'MagFields', {'MAG_PSF','MAG_APER_3'}, 'TwoPanels', false);

    arguments
        Args.MergedMatDir
        Args.CropsToAnalyze = []
        Args.Ncrop          = 24
        Args.Mode           = 'percrop'
        Args.Verbose logical = true
    end

    % Find MergedMat HDF5 files
    FileList = io.files.filelist(fullfile(Args.MergedMatDir, '*_sci_merged_MergedMat_*.hdf5'));
    if isempty(FileList)
        warning('loadMergedMat:NoFiles', 'No MergedMat files found in %s', Args.MergedMatDir);
        MS = struct();
        MS.(Args.Mode) = cell(1, Args.Ncrop);
        return;
    end

    if Args.Verbose
        fprintf('Found %d MergedMat files in %s\n', numel(FileList), Args.MergedMatDir);
    end

    % Extract crop ID from filename: ..._NNN_sci_merged_...
    % Crop ID is the last 3-digit number before _sci_merged_
    CropIDs = zeros(numel(FileList), 1);
    for If = 1:numel(FileList)
        [~, Name] = fileparts(FileList{If});
        Tokens = strsplit(Name, '_');
        % Find 'sci' token, crop ID is 3 tokens before it
        SciIdx = find(strcmp(Tokens, 'sci'), 1);
        if ~isempty(SciIdx) && SciIdx >= 4
            CropIDs(If) = str2double(Tokens{SciIdx - 1});
        end
    end

    if isempty(Args.CropsToAnalyze)
        Args.CropsToAnalyze = 1:Args.Ncrop;
    end

    MS.(Args.Mode) = cell(1, Args.Ncrop);

    for Ic = Args.CropsToAnalyze
        FileIdx = find(CropIDs == Ic, 1);
        if isempty(FileIdx)
            if Args.Verbose
                fprintf('  Crop %02d: no file found, skipping\n', Ic);
            end
            continue;
        end

        % Read via MatchedSources.read
        MSobj = MatchedSources.read(FileList{FileIdx});

        % Transpose: HDF5 stores [Nsrc x Nepochs], MS expects [Nepochs x Nsrc]
        Flds = fieldnames(MSobj.Data);
        for Ifl = 1:numel(Flds)
            D = MSobj.Data.(Flds{Ifl});
            if ismatrix(D) && size(D, 1) > size(D, 2)
                MSobj.Data.(Flds{Ifl}) = D.';
            end
        end

        MS.(Args.Mode){Ic} = MSobj;

        if Args.Verbose
            fprintf('  Crop %02d: %d sources x %d epochs from %s\n', ...
                Ic, MSobj.Nsrc, MSobj.Nepoch, FileList{FileIdx});
        end
    end
end
