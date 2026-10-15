function Tbl = read_ZTF_inventory(FileName, Args)
    % Read an IRSA ZTF inventory CSV and key it by product file name.
    % Description: ZTF's per-epoch catalogs carry no exposure time and no
    %              airmass - of the two, the airmass genuinely matters,
    %              because it is the one header quantity the transmission
    %              model cannot absorb into its fitted normalization. Both
    %              live in the inventory table IRSA serves next to the data
    %              (inventory_*.csv), one row per product. This reads it and
    %              adds the psfcat/sexcat file names so a row can be looked
    %              up by the file actually being calibrated.
    % Input  : - FileName - path to an IRSA ZTF inventory CSV. It must have
    %            the columns filefracday, field, filtercode, ccdid, qid and
    %            (for the lookup names) imgtypecode.
    %          * ...,key,val,...
    %            'FilterCode' - Keep only rows of this filter ('zg'|'zr'|'zi').
    %                   Default '' (keep all).
    %            'AddNames' - Add the PsfcatFile/SexcatFile columns.
    %                   Default is true.
    % Output : - Tbl - the inventory as a MATLAB table, with PsfcatFile and
    %            SexcatFile appended when AddNames is true.
    % Author : D. Kovaleva (Oct 2026)
    % Example: Inv = VO.ZTF.read_ZTF_inventory('inventory_astropack_sex.csv', 'FilterCode','zr');
    %          Row = Inv(strcmp(Inv.SexcatFile, MyFile), :);

    arguments
        FileName
        Args.FilterCode (1,:) char = ''
        Args.AddNames logical      = true
    end

    Tbl = readtable(FileName);

    Need = {'filefracday','field','filtercode','ccdid','qid'};
    Miss = Need(~ismember(Need, Tbl.Properties.VariableNames));
    if ~isempty(Miss)
        error('VO:ZTF:read_ZTF_inventory:MissingColumns', ...
              'Inventory %s is missing the column(s): %s', FileName, strjoin(Miss, ', '));
    end

    if ~isempty(Args.FilterCode)
        Tbl = Tbl(strcmp(string(Tbl.filtercode), Args.FilterCode), :);
    end

    if Args.AddNames
        Nrow = height(Tbl);
        Psf  = cell(Nrow,1);
        Sex  = cell(Nrow,1);
        if ismember('imgtypecode', Tbl.Properties.VariableNames)
            Type = string(Tbl.imgtypecode);
        else
            Type = repmat("o", Nrow, 1);
        end
        FFD = Tbl.filefracday;
        if ~isnumeric(FFD)
            FFD = str2double(string(FFD));
        end
        Filt = string(Tbl.filtercode);
        for Ir = 1:Nrow
            Stem = sprintf('ztf_%014.0f_%06d_%s_c%02d_%s_q%d', ...
                           FFD(Ir), Tbl.field(Ir), Filt(Ir), Tbl.ccdid(Ir), Type(Ir), Tbl.qid(Ir));
            Psf{Ir} = [Stem, '_psfcat.fits'];
            Sex{Ir} = [Stem, '_sexcat.fits'];
        end
        Tbl.PsfcatFile = Psf;
        Tbl.SexcatFile = Sex;
    end
end
