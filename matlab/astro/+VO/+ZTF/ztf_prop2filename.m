function FileName = ztf_prop2filename(Prop, FileType, Args)
    % Construct a ZTF per-epoch product file name from its properties.
    % Package: +VO/+ZTF
    % Description: The inverse of VO.ZTF.ztf_filename2prop. Its use is to
    %              reach a sibling product of the same exposure - the sexcat
    %              that goes with a psfcat, or the inventory row that carries
    %              this exposure's airmass, which the files themselves do not.
    % Input  : - Prop - structure array as returned by VO.ZTF.ztf_filename2prop,
    %            or any structure with the fields FileFracDay (or Year, Month,
    %            Day, Frac), Field, Filter, CCDID, QuadID and optionally
    %            ImgTypeCode (default 'o').
    %          - FileType - product to name, e.g. 'psfcat', 'sexcat', 'sciimg'.
    %            Default is the FileType in Prop, else 'psfcat'.
    %          * ...,key,val,...
    %            'Dir' - Directory to prepend. Default is '' .
    %            'Ext' - Extension. Default is '.fits'.
    % Output : - FileName - char, or a cell array of char when Prop has more
    %            than one element.
    % Author : D. Kovaleva (Oct 2026)
    % Example: Prop = VO.ZTF.ztf_filename2prop(PsfFile);
    %          Sex  = VO.ZTF.ztf_prop2filename(Prop, 'sexcat');

    arguments
        Prop
        FileType  = ''
        Args.Dir  = ''
        Args.Ext  = '.fits'
    end

    Nprop    = numel(Prop);
    FileName = cell(1, Nprop);
    for Ip = 1:Nprop
        P = Prop(Ip);

        if isfield(P, 'FileFracDay') && ~isempty(P.FileFracDay)
            FFD = P.FileFracDay;
            if isnumeric(FFD)
                FFD = sprintf('%014.0f', FFD);
            end
        else
            % Rebuild it from the date fields. Frac carries 6 digits in every
            % ZTF name seen so far.
            FFD = sprintf('%04d%02d%02d%06.0f', P.Year, P.Month, P.Day, P.Frac.*1e6);
        end

        if isfield(P, 'ImgTypeCode') && ~isempty(P.ImgTypeCode)
            Type = P.ImgTypeCode;
        else
            Type = 'o';
        end

        Kind = FileType;
        if isempty(Kind)
            if isfield(P, 'FileType') && ~isempty(P.FileType)
                Kind = P.FileType;
            else
                Kind = 'psfcat';
            end
        end

        FileName{Ip} = sprintf('ztf_%s_%06d_%s_c%02d_%s_q%d_%s%s', ...
                               char(FFD), P.Field, char(P.Filter), P.CCDID, ...
                               char(Type), P.QuadID, char(Kind), Args.Ext);
        if ~isempty(Args.Dir)
            FileName{Ip} = fullfile(Args.Dir, FileName{Ip});
        end
    end

    if Nprop == 1
        FileName = FileName{1};
    end
end
