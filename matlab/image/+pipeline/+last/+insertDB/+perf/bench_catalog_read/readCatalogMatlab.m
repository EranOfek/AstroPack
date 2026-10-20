function A = readCatalogMatlab(FileName)
% readCatalogMatlab  Pure-MATLAB reader of the first all-double (TFORM '1D') BINTABLE of a FITS file.
% Input  : - FITS file name
% Output : - [Nrows x Ncols] double matrix (same as table2array(struct2table(read_catalog(...))))
% Notes  : no MEX, so it runs on process and thread-based parpools. Errors on any other table layout.
% Example: A = readCatalogMatlab('LAST...proc_Cat_1.fits');

    BLOCK = 2880;
    Fid = fopen(FileName, 'r', 'ieee-be');
    if Fid < 0
        error('readCatalogMatlab:open', 'cannot open %s', FileName);
    end
    Off = 0;
    while true
        Hdr = fread(Fid, BLOCK * 16, '*uint8');       % up to 16 blocks of header; one syscall for the common case
        Cards = reshape(Hdr(1:floor(numel(Hdr)/80)*80), 80, []);
        IsEnd = find(all(Cards(1:8, :) == uint8('END     ')', 1), 1);
        if isempty(IsEnd)
            fclose(Fid);
            error('readCatalogMatlab:header', 'END card not found in %s', FileName);
        end
        HdrLen = ceil(IsEnd * 80 / BLOCK) * BLOCK;     % header padded to 2880
        Txt = char(Cards(:, 1:IsEnd)).';               % IsEnd x 80 cards as rows
        if strncmp(Txt(1, :), 'XTENSION= ''BINTABLE''', 20)
            N1 = cardInt(Txt, 'NAXIS1');
            N2 = cardInt(Txt, 'NAXIS2');
            Tf = cardInt(Txt, 'TFIELDS');
            NumD = sum(all(Txt(:, 1:5) == 'TFORM', 2) & all(Txt(:, 11:20) == '''1D      ''', 2));
            if N1 ~= 8 * Tf || NumD ~= Tf
                fclose(Fid);
                error('readCatalogMatlab:format', 'not an all-double table: %s', FileName);
            end
            fseek(Fid, Off + HdrLen, 'bof');
            A = fread(Fid, [Tf, N2], '*double').';
            fclose(Fid);
            return
        end
        % skip this HDU's data
        Nax = cardInt(Txt, 'NAXIS');
        Bp  = abs(cardInt(Txt, 'BITPIX'));
        Nel = 1;
        for K = 1:Nax
            Nel = Nel * cardInt(Txt, sprintf('NAXIS%d', K));
        end
        if Nax > 0
            DataLen = ceil(Nel * Bp / 8 / BLOCK) * BLOCK;
        else
            DataLen = 0;
        end
        Off = Off + HdrLen + DataLen;
        fseek(Fid, Off, 'bof');
    end
end

function V = cardInt(Txt, Key)
    Row = find(all(Txt(:, 1:8) == sprintf('%-8s', Key), 2) & Txt(:, 9) == '=', 1);
    if isempty(Row)
        error('readCatalogMatlab:card', 'missing card %s', Key);
    end
    V = sscanf(strtok(Txt(Row, 10:80), '/'), '%d');
end
