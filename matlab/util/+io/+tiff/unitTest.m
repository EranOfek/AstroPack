function Result = unitTest()
    % unitTest for the io.tiff package (read1, readHeader1, ImageIO/AstroImage dispatch)
    % Example: io.tiff.unitTest
    io.msgStyle(LogLevel.Test, '@start', 'io.tiff test started');

    TmpDir = tempname;
    mkdir(TmpDir);
    Cleanup = onCleanup(@() rmdir(TmpDir, 's'));

    % single-page uint16 TIFF with a known pattern
    [X, Y] = meshgrid(1:40, 1:30);
    Im = uint16(1000*Y + X);
    File1 = fullfile(TmpDir, 'single.tif');
    imwrite(Im, File1, 'tif', 'Compression','none');

    % two-page TIFF
    File2 = fullfile(TmpDir, 'multi.tif');
    imwrite(Im,   File2, 'tif', 'Compression','none');
    imwrite(2*Im, File2, 'tif', 'Compression','none', 'WriteMode','append');

    % readHeader1
    H = io.tiff.readHeader1(File1);
    assert(size(H,2)==3);
    assert(H{strcmp(H(:,1),'NAXIS1'),2}==40 && H{strcmp(H(:,1),'NAXIS2'),2}==30);
    assert(H{strcmp(H(:,1),'BITPIX'),2}==16);
    assert(H{strcmp(H(:,1),'NPAGES'),2}==1);
    assert(strcmp(H{strcmp(H(:,1),'ORIGFILE'),2}, 'single.tif'));

    % read1: full image, native class, TIFF row order
    [Im1, H1] = io.tiff.read1(File1);
    assert(isa(Im1,'uint16') && isequal(Im1, Im));
    assert(H1{strcmp(H1(:,1),'FLIPUD'),2}==false);

    % read1: FlipUD
    ImF = io.tiff.read1(File1, 'FlipUD',true);
    assert(isequal(ImF, flipud(Im)));

    % read1: CCDSEC [Xmin Xmax Ymin Ymax], with and without FlipUD
    [ImS, HS] = io.tiff.read1(File1, 'CCDSEC',[3 7 2 5]);
    assert(isequal(ImS, Im(2:5, 3:7)));
    assert(HS{strcmp(HS(:,1),'NAXIS1'),2}==5 && HS{strcmp(HS(:,1),'NAXIS2'),2}==4);
    ImSF = io.tiff.read1(File1, 'CCDSEC',[3 7 2 5], 'FlipUD',true);
    assert(isequal(ImSF, ImF(2:5, 3:7)));

    % read1: pages
    Im2 = io.tiff.read1(File2, 'Page',2);
    assert(isequal(Im2, 2*Im));
    H2 = io.tiff.readHeader1(File2, 'Page',2);
    assert(H2{strcmp(H2(:,1),'NPAGES'),2}==2 && H2{strcmp(H2(:,1),'TIFFPAGE'),2}==2);
    try
        io.tiff.read1(File2, 'Page',3);
        error('io:tiff:unitTest', 'Page out of range did not raise an error');
    catch ME
        assert(strcmp(ME.identifier, 'io:tiff:read1:page'));
    end

    % ImageIO dispatch
    [D, HH] = ImageIO.read1(File1);
    assert(isequal(D, Im) && size(HH,2)==3 && any(strcmp(HH(:,1),'NAXIS1')));
    D = ImageIO.read1(File1, 'CCDSEC',[1 10 1 5]);
    assert(isequal(D, Im(1:5,1:10)));
    [~, HH] = ImageIO.read1(File1, 'ReadData',false);
    assert(any(strcmp(HH(:,1),'NAXIS1')));
    D = ImageIO.read1(File2, 'HDU',2);
    assert(isequal(D, 2*Im));

    % AstroImage
    AI = AstroImage(File1);
    assert(isequal(AI.Image, Im));
    assert(AI.HeaderData.getVal('NAXIS1')==40);
    AI = AstroImage(fullfile(TmpDir, '*.tif'));
    assert(numel(AI)==2);
    AI = AstroImage(File1, 'CCDSEC',[11 20 6 10]);
    assert(isequal(AI.Image, Im(6:10, 11:20)));

    % generic imread fallback is untouched
    FilePng = fullfile(TmpDir, 'a.png');
    imwrite(Im, FilePng);
    D = ImageIO.read1(FilePng);
    assert(isequal(D, Im));

    io.msgStyle(LogLevel.Test, '@passed', 'io.tiff test passed');
    Result = true;
end
