function [OK, Reason] = verifyTextFile(FileName, Args)
    % Verify that a text file is readable, is plain text, and is well formed.
    %   Intended for files that several programs append to -- status files,
    %   receipts, append-only logs -- where a stray binary or truncating write
    %   leaves the file silently useless to every later reader. Never throws:
    %   a problem is reported through OK/Reason so a caller in a service loop
    %   can log and continue.
    % Input  : - File name.
    %          * ...,key,val,...
    %            'MustContain' - char/string that must appear somewhere in the
    %                   file. Default is '' (no content requirement).
    %            'LinePattern' - regular expression that every non-empty line
    %                   must match. Use it to express the file's own grammar,
    %                   e.g. '^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}' for a file
    %                   whose lines begin with an ISO timestamp. Checking the
    %                   grammar catches printable garbage that a later append
    %                   would otherwise mask. Default is '' (no line check).
    %            'AllowEmpty' - Is a zero-length file acceptable.
    %                   Default is false.
    %            'MaxBytes' - Refuse to read a file larger than this, so the
    %                   function cannot be pointed at a data product by
    %                   mistake. Default is 1e6.
    % Output : - OK: true when every requested check passed.
    %          - Reason: short description of the first failure, '' when OK.
    % Author : Alexander Krassilchtchikov (Oct 2026)
    % Example: Line = '2026-10-05T14:00:00 ready-for-transfer';
    %          [OK, Reason] = io.files.verifyTextFile('.status', ...
    %                             'MustContain', Line, ...
    %                             'LinePattern', '^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}');

    arguments
        FileName
        Args.MustContain = '';
        Args.LinePattern = '';
        Args.AllowEmpty  = false;
        Args.MaxBytes    = 1e6;
    end

    OK     = false;
    Reason = '';

    Dir = dir(FileName);
    if isempty(Dir)
        Reason = 'does not exist';
        return;
    end
    if Dir(1).bytes > Args.MaxBytes
        Reason = sprintf('%d bytes exceeds MaxBytes=%d', Dir(1).bytes, Args.MaxBytes);
        return;
    end

    try
        Txt = fileread(FileName);
    catch ME
        Reason = sprintf('unreadable (%s)', ME.identifier);
        return;
    end

    if isempty(Txt)
        if Args.AllowEmpty
            OK = true;
        else
            Reason = 'empty';
        end
        return;
    end

    % Plain text: allow TAB, LF, CR and printable ASCII, nothing else.
    Byte    = double(Txt);
    NonText = Byte<9 | (Byte>13 & Byte<32) | Byte>126;
    if any(NonText)
        Reason = sprintf('not text: %d of %d bytes outside printable ASCII', sum(NonText), numel(Byte));
        return;
    end

    if ~isempty(Args.MustContain) && ~contains(Txt, Args.MustContain)
        Reason = sprintf('required content absent (%d bytes present)', numel(Byte));
        return;
    end

    if ~isempty(Args.LinePattern)
        Lines = strsplit(strtrim(Txt), newline);
        for Iline=1:1:numel(Lines)
            Line = strtrim(Lines{Iline});
            if isempty(Line)
                continue;
            end
            if isempty(regexp(Line, Args.LinePattern, 'once'))
                Reason = sprintf('line %d does not match LinePattern: "%s"', ...
                                 Iline, Line(1:min(60,end)));
                return;
            end
        end
    end

    OK = true;
end
