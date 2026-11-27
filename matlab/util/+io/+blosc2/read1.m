function [Image, Header] = read1(File, Args)
    % Read a 2-D float32 image stored as a Blosc2 NDArray file (.b2nd), with its FITS header.
    %   Blosc2 packs the image losslessly, block by block (byte reordering + zstd or lz4); all-zero blocks take
    %   almost no space. The files are written by python-blosc2 (see io/+blosc2/README.md); the FITS header, if
    %   any, is stored as the variable-length metadata 'FITS_HEADER' (80-character cards).
    % Input  : - File name, or the whole file already read into memory as a uint8 vector.
    %          * ...,key,val,...
    %            'ReadWhole' - Read the whole file with fread, then unpack it. Faster over the network (one read
    %                   instead of many small ones); about the same from a local disk. Default is true.
    %            'HeaderType' - 'cell' (3-column cell array: key, value, comment) or 'char' (the header text).
    %                   Default is 'cell'.
    % Output : - Image (single), rows = the array's first index, as in numpy and FITS readers.
    %          - Header, as set by 'HeaderType'; empty if the file has none.
    % Author : Dana Kovaleva (Nov 2026)
    % Example: Im = io.blosc2.read1('image.b2nd');
    %          [Im, H] = io.blosc2.read1('image.b2nd');
    %          fid = fopen('image.b2nd'); B = fread(fid,Inf,'*uint8'); fclose(fid); Im = io.blosc2.read1(B);

    arguments
        File
        Args.ReadWhole(1,1) logical = true;
        Args.HeaderType             = 'cell';
    end

    if isstring(File)
        File = char(File);
    end
    if ischar(File) && Args.ReadWhole
        Fid = fopen(File, 'r');
        if Fid < 0
            error('io:blosc2:read1:open', 'Cannot open %s', File);
        end
        File = fread(Fid, Inf, '*uint8');
        fclose(Fid);
    end
    if ~(ischar(File) || isa(File,'uint8'))
        error('io:blosc2:read1:input', 'Input must be a file name or a uint8 vector');
    end

    if nargout > 1
        [Image, HeaderText] = io.blosc2.mex.mex_blosc2_read(File);
        switch lower(Args.HeaderType)
            case 'char'
                Header = HeaderText;
            case 'cell'
                Header = headerText2cell(HeaderText);
            otherwise
                error('io:blosc2:read1:HeaderType', 'HeaderType must be ''cell'' or ''char''');
        end
    else
        Image = io.blosc2.mex.mex_blosc2_read(File);
    end
    Image = Image.';   % C (row-major) order -> MATLAB (column-major)
end

function Header = headerText2cell(Text)
    % FITS header text (80-character cards) -> 3-column cell {Key, Value, Comment}. Values: number, logical,
    % or char (quotes removed, '' -> '); long strings continued with CONTINUE cards are joined.
    Ncard  = floor(numel(Text)/80);
    Header = cell(0,3);
    for I = 1:Ncard
        Card = Text((I-1)*80 + (1:80));
        Key  = strtrim(Card(1:8));
        if strcmp(Key,'END')
            break;
        end
        if strcmp(Key,'CONTINUE') && ~isempty(Header) && ischar(Header{end,2})
            [Val, Com] = parseValue(Card(11:end));
            Prev = Header{end,2};
            if endsWith(Prev,'&')
                Prev = Prev(1:end-1);
            end
            Header{end,2} = [Prev Val];
            if ~isempty(Com)
                Header{end,3} = Com;
            end
            continue;
        end
        if numel(Card) >= 10 && strcmp(Card(9:10),'= ')
            [Val, Com] = parseValue(Card(11:end));
        else
            Val = '';
            Com = strtrim(Card(9:end));
        end
        Header(end+1,:) = {Key, Val, Com}; %#ok<AGROW>
    end
end

function [Val, Com] = parseValue(S)
    % value and comment of a card's value field
    S = strtrim(S);
    Com = '';
    if startsWith(S,'''')
        % quoted string: '' inside means one quote
        I = 2; Val = '';
        while I <= numel(S)
            if S(I) == ''''
                if I < numel(S) && S(I+1) == ''''
                    Val(end+1) = ''''; I = I + 2; %#ok<AGROW>
                    continue;
                end
                break;
            end
            Val(end+1) = S(I); I = I + 1; %#ok<AGROW>
        end
        Val  = deblank(Val);
        Rest = S(min(I+1,numel(S)+1):end);
        J = strfind(Rest,'/');
        if ~isempty(J)
            Com = strtrim(Rest(J(1)+1:end));
        end
    else
        J = strfind(S,'/');
        if ~isempty(J)
            Com = strtrim(S(J(1)+1:end));
            S   = strtrim(S(1:J(1)-1));
        end
        switch S
            case 'T'
                Val = true;
            case 'F'
                Val = false;
            otherwise
                Val = str2double(S);
                if isnan(Val) && ~strcmpi(S,'nan')
                    Val = S;
                end
        end
    end
end
