function Result = unitTest()
    % unitTest for the io.blosc2 package (read1)
    %   Uses unitTest_image.b2nd: 37 x 53 float32, value(r,c) = ((r-1)*53 + (c-1))*0.25 - 100, rows 11..20 and
    %   columns 21..53 set to 0, with a FITS header (made with python-blosc2).
    % Example: io.blosc2.unitTest
    io.msgStyle(LogLevel.Test, '@start', 'io.blosc2 test started');

    File = fullfile(fileparts(mfilename('fullpath')), 'unitTest_image.b2nd');
    [C, R] = meshgrid(0:52, 0:36);
    Expected = single((R*53 + C)*0.25 - 100);
    Expected(11:20, 21:53) = 0;

    % file name, read whole (default) and through the library
    [Im, H] = io.blosc2.read1(File);
    assert(isa(Im,'single') && isequal(size(Im), [37 53]));
    assert(isequal(Im, Expected));
    Im2 = io.blosc2.read1(File, 'ReadWhole',false);
    assert(isequal(Im2, Expected));

    % bytes already in memory
    Fid = fopen(File, 'r'); B = fread(Fid, Inf, '*uint8'); fclose(Fid);
    assert(isequal(io.blosc2.read1(B), Expected));

    % header as a 3-column cell
    assert(size(H,2) == 3);
    Get = @(Key) H{strcmp(H(:,1),Key), 2};
    assert(Get('EXPTIME') == 20.5);
    assert(strcmp(Get('OBJECT'), 'O''Brien field'));
    assert(Get('ISREF') == true);
    assert(strcmp(Get('LONGSTR'), repmat('x',1,100)));
    assert(strcmp(H{strcmp(H(:,1),'EXPTIME'),3}, 'exposure time [s]'));

    % header as text
    [~, Ht] = io.blosc2.read1(File, 'HeaderType','char');
    assert(ischar(Ht) && mod(numel(Ht),80) == 0 && startsWith(Ht,'EXPTIME ='));

    io.msgStyle(LogLevel.Test, '@passed', 'io.blosc2 test passed');
    Result = true;
end
