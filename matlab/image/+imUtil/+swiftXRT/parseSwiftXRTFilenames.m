function Info = parseSwiftXRTFilenames(Filenames)
    % Extracts ObsID, Instrument, Window, Mode from Swift-XRT filenames
    % Input  : - String array or cell array of file names.
    % Output : - A structure array with:
    %            .ObsID
    %            .Instrument
    %            .Window
    %            .Mode
    % Author : ChatGPT + Eran Ofek (2026 Jan) 
    % Example: Info=imUtil.swiftXRT.parseSwiftXRTFilenames('sw00067099001xpcw4po_cl.evt')

    if ischar(Filenames)
        Filenames = string(Filenames);
    end

    % Number of files
    NFiles = numel(Filenames);

    % Initialize output

    % Regular expression pattern
    % Matches: sw + ObsID + Instrument (xpc/xwt/xlr/...) + w# + mode (po/uf/ufre/st/sl/...)
    Pattern = 'sw(?<ObsID>\d+)(?<Instrument>[a-z]+)(?<Window>w\d+)(?<Mode>[a-z]+)_.*\.evt';

    Info = struct('ObsID',cell(NFiles,1), 'Instrument',cell(NFiles,1), 'Window',cell(NFiles,1), 'Mode',cell(NFiles,1));
    for K = 1:NFiles
        FileName = Filenames{K};

        Tokens = regexp(FileName, Pattern, 'names');

        if isempty(Tokens)
            warning('Filename "%s" does not match Swift-XRT pattern.', FileName);
        else
            Info(K) = Tokens;
        end
    end
end

