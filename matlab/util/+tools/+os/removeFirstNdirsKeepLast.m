function Out = removeFirstNdirsKeepLast(In, N, Args)
    % Remove everything up to and including the N-th delimiter.
    % Input  : - Cell array or string array.
    %          - Number of delimiters to remove.
    %          - Delimiter. Default is '/'
    % Output : - Cleaned cell or string array.
    % Author : ChatGPT + Eran Ofek (Jun 2026)
    % Example:
    %   In = 'https://cxc.cfa.harvard.edu/cdaftp/byobsid/7/47/secondary/aspect';
    %   Out = tools.os.removeFirstNdirsKeepLast(In, 4)

    arguments
        In
        N (1,1) double {mustBeNonnegative, mustBeInteger}
        Args.Delimiter char = '/'
    end

    Delimiter = Args.Delimiter;

    if iscell(In)
        S = string(In);
    elseif ischar(In)
        S = string({In});
    elseif isstring(In)
        S = In;
    else
        error('Input must be char, string array, or cell array of char.');
    end

    Out = strings(size(S));

    for I = 1:numel(S)
        Str = char(S(I));

        Pos = strfind(Str, Delimiter);

        if N == 0
            Out(I) = string(Str);

        elseif numel(Pos) >= N
            Out(I) = string(Str(Pos(N) + length(Delimiter):end));

        else
            Out(I) = "";
        end
    end
end