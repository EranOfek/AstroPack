function MS = loadMergedMat(Args)
    % Load pre-built MergedMat HDF5 files into a MatchedSources array
    % Description: Reads *_sci_merged_MergedMat_*.hdf5 files (one per crop)
    %              from a directory. These contain already cross-matched
    %              sources across epochs with relative photometry (no MAG_AB).
    %              The HDF5 datasets are [Nsrc x Nepochs]; they are
    %              transposed to [Nepochs x Nsrc] to match the
    %              MatchedSources convention.
    %
    %              The output matches loadVisit's MS branch: a flat
    %              MatchedSources array, no positional padding for missing
    %              crops. Files are sorted by ascending crop ID (or by the
    %              order requested via CropsToAnalyze).
    %
    % Input  : * ...,key,val,...
    %            'MergedMatDir'   - Directory with MergedMat HDF5 files.
    %                               Required.
    %            'CropsToAnalyze' - Crop IDs to load, in the desired output
    %                               order. Default is [] (load all found,
    %                               sorted by crop ID).
    %            'Verbose'        - Print progress. Default is true.
    % Output : - MS - 1xNkeep MatchedSources array.
    % Author : D. Kovaleva (Apr 2026)
    % Example: MS = pipeline.last.load.loadMergedMat('MergedMatDir', '~/222635v0');
    %          pipeline.last.quality.photCalib.plotPhotStability(MS, ...
    %              'Quantities', {'MAG_PSF','MAG_APER_3'});

    arguments
        Args.MergedMatDir
        Args.CropsToAnalyze = []
        Args.Verbose logical = true
    end

    MS = MatchedSources.empty(1, 0);

    FileList = io.files.filelist(fullfile(Args.MergedMatDir, ...
        '*_sci_merged_MergedMat_*.hdf5'));
    if isempty(FileList)
        warning('loadMergedMat:NoFiles', ...
            'No MergedMat files found in %s', Args.MergedMatDir);
        return;
    end

    if Args.Verbose
        fprintf('Found %d MergedMat files in %s\n', ...
            numel(FileList), Args.MergedMatDir);
    end

    % Extract crop ID from filename: ..._NNN_sci_merged_...
    % Crop ID is the token immediately before 'sci'.
    CropIDs = zeros(numel(FileList), 1);
    for If = 1:numel(FileList)
        [~, Name] = fileparts(FileList{If});
        Tokens = strsplit(Name, '_');
        SciIdx = find(strcmp(Tokens, 'sci'), 1);
        if ~isempty(SciIdx) && SciIdx >= 4
            CropIDs(If) = str2double(Tokens{SciIdx - 1});
        end
    end

    % Decide which crops (and in what order) to load
    if isempty(Args.CropsToAnalyze)
        [SortedIDs, Order] = sort(CropIDs);
        Valid    = SortedIDs > 0;
        KeepIdx  = Order(Valid);
        KeepCrop = SortedIDs(Valid);
    else
        Nreq     = numel(Args.CropsToAnalyze);
        KeepIdx  = zeros(Nreq, 1);
        KeepCrop = zeros(Nreq, 1);
        Nkeep    = 0;
        for Iic = 1:Nreq
            Ic = Args.CropsToAnalyze(Iic);
            FileIdx = find(CropIDs == Ic, 1);
            if ~isempty(FileIdx)
                Nkeep = Nkeep + 1;
                KeepIdx(Nkeep)  = FileIdx;
                KeepCrop(Nkeep) = Ic;
            elseif Args.Verbose
                fprintf('  Crop %02d: no file found, skipping\n', Ic);
            end
        end
        KeepIdx  = KeepIdx(1:Nkeep);
        KeepCrop = KeepCrop(1:Nkeep);
    end

    if isempty(KeepIdx); return; end

    % Batched read — MatchedSources.read accepts a cell array of names
    % and returns a 1xN MatchedSources array.
    MS = MatchedSources.read(FileList(KeepIdx));

    % Transpose: HDF5 stores [Nsrc x Nepochs], MS expects [Nepochs x Nsrc]
    for Ik = 1:numel(MS)
        Flds = fieldnames(MS(Ik).Data);
        for Ifl = 1:numel(Flds)
            D = MS(Ik).Data.(Flds{Ifl});
            if ismatrix(D) && size(D, 1) > size(D, 2)
                MS(Ik).Data.(Flds{Ifl}) = D.';
            end
        end
        if Args.Verbose
            fprintf('  Crop %02d: %d sources x %d epochs from %s\n', ...
                KeepCrop(Ik), MS(Ik).Nsrc, MS(Ik).Nepoch, FileList{KeepIdx(Ik)});
        end
    end
end
