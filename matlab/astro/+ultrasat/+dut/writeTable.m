function writeTable(T, File, Header, ExtName)
    % Write a table as a FITS binary table, with header keywords in the table HDU itself
    %   The columns are written as double, with the table's VariableUnits.
    %   (FITS.writeTable1 'Header' would put the keywords into a separate
    %   image HDU after the table.) An existing file is replaced; the
    %   directory is created if missing.
    % Input  : - T: a table of numeric (or logical) columns.
    %          - File: the FITS file name.
    %          - Header: an N x 3 cell {key, value, comment}. Default is {}.
    %          - ExtName: the EXTNAME of the table. Default is 'TABLE'.
    % Author : D. Kovaleva (Oct 2026)
    % Example: ultrasat.dut.writeTable(table((1:3)', 'VariableNames', {'X'}), '/tmp/t.fits', {'A', 1, 'a key'});

    arguments
        T table
        File (1,:) char
        Header cell = {}
        ExtName (1,:) char = 'TABLE'
    end

    Dir = fileparts(File);
    if ~isempty(Dir) && ~isfolder(Dir)
        mkdir(Dir);
    end
    if isfile(File)
        delete(File);
    end
    Units = T.Properties.VariableUnits;
    if isempty(Units)
        Units = repmat({''}, 1, width(T));
    end
    M = double(table2array(T));
    FITS.writeTable1(AstroTable({M}, 'ColNames', T.Properties.VariableNames), File, 'ColUnits', Units, ...
                     'OverWrite', true, 'ExtName', ExtName);
    if ~isempty(Header)
        Fptr = matlab.io.fits.openFile(File, 'readwrite');
        Closer = onCleanup(@() matlab.io.fits.closeFile(Fptr));
        matlab.io.fits.movAbsHDU(Fptr, 2);
        for K = 1:size(Header, 1)
            matlab.io.fits.writeKey(Fptr, Header{K,1}, Header{K,2}, Header{K,3});
        end
    end
end
