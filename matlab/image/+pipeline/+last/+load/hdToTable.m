function T = hdToTable(HD, Files, Args)
    % Flatten an extractHeaderData struct + a filename grid into a table.
    % Description: Companion to pipeline.last.load.extractHeaderData for
    %              the one-row-per-file use case (header sweep -> CSV).
    %              Each field of HD (a [Nepoch x Ncrop] matrix per keyword)
    %              is flattened to a Nepoch*Ncrop column; Files (a matching
    %              cell(Nepoch, 1) of Ncrop-length cellstr, produced as the
    %              second output of loadVisitCatHdr) supplies per-row
    %              Filename and (optionally parsed) CropId columns.
    % Input  : - HD    struct from extractHeaderData: each field is a
    %                  [Nepoch x Ncrop] numeric matrix.
    %          - Files cell(Nepoch, 1) of Ncrop-long cellstr file paths.
    %                  Empty ({} at position Iv) marks visits that returned
    %                  no files; those rows are dropped from the table.
    %          * ...,key,val,...
    %            'CropIdRegex' - Regex with one capture group returning the
    %                            crop number from each filename's basename.
    %                            Default '_(\d+)_sci_coadd_' (LAST).
    %                            Set '' to omit the NCrop column.
    %            'Basename'    - true to strip directory and keep filename
    %                            only in the Filename column. Default true.
    % Output : - T table with columns Filename, [NCrop,] and one per HD field
    %              (in field-declaration order). Rows sorted by Filename.
    % Author : D. Kovaleva (Aug 2026)
    % See also: pipeline.last.load.loadVisitCatHdr,
    %           pipeline.last.load.extractHeaderData.
    % Example:
    %   [AI, Files] = pipeline.last.load.loadVisitCatHdr( ...
    %                    'DataDir', BaseDir, 'FileType', 'coadd', 'CatHDU', 3);
    %   HD = pipeline.last.load.extractHeaderData(AI, ...
    %                    'HeaderKeys', {'JD','AIRMASS','FWHM','TEMP', ...
    %                                   'PT_RMS','PT_ARMS','PT_1_N', ...
    %                                   'PT_CHI2','PT_DOF','PT_NCALI', ...
    %                                   'PT_3_V2','PT_5_V2','PT_8_V2'});
    %   T = pipeline.last.load.hdToTable(HD, Files);
    %   writetable(T, '/home/dana/tmp/1679c_reconstructed.csv');

    arguments
        HD    struct
        Files cell
        Args.CropIdRegex (1,:) char    = '_(\d+)_sci_coadd_'
        Args.Basename    (1,1) logical = true
    end

    Fn = fieldnames(HD);
    if isempty(Fn)
        T = table();
        return;
    end
    [Nep, Ncr] = size(HD.(Fn{1}));

    % Flat filename column, aligned to HD's row-major [Nep x Ncr] layout.
    FileFlat = cell(Nep * Ncr, 1);
    for Iv = 1:Nep
        Row = Files{Iv};
        if isempty(Row); continue; end
        NN = min(numel(Row), Ncr);
        for Ic = 1:NN
            FileFlat{(Iv-1)*Ncr + Ic} = Row{Ic};
        end
    end

    Present = ~cellfun(@isempty, FileFlat);
    FileFlat = FileFlat(Present);

    if Args.Basename
        [~, N, E] = cellfun(@fileparts, FileFlat, 'UniformOutput', false);
        Filename  = strcat(N, E);
    else
        Filename  = FileFlat;
    end

    T = table(Filename);

    if ~isempty(Args.CropIdRegex)
        NCrop = nan(numel(FileFlat), 1);
        for I = 1:numel(FileFlat)
            [~, N, ~] = fileparts(FileFlat{I});
            Tok = regexp(N, Args.CropIdRegex, 'tokens', 'once');
            if ~isempty(Tok); NCrop(I) = str2double(Tok{1}); end
        end
        T.NCrop = NCrop;
    end

    for K = 1:numel(Fn)
        Vals = HD.(Fn{K});
        Vec  = reshape(Vals.', [], 1);   % transpose then flatten to match row-major (Iv, Ic)
        T.(Fn{K}) = Vec(Present);
    end
end
