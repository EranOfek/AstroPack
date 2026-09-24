function [Image, Header] = read1(FileName, Args)
    % Read a single page of a TIFF file, optionally with a FITS-like header.
    %   The image is returned in its native class (e.g., uint16) and in the
    %   TIFF row order (first row = top of the frame), unless FlipUD is set.
    % Input  : - TIFF file name.
    %          * ...,key,val,...
    %            'Page' - Page (directory) number in a multi-page TIFF.
    %                   Default is 1.
    %            'CCDSEC' - [Xmin Xmax Ymin Ymax] section to read. Y refers
    %                   to the output orientation (see FlipUD). If empty,
    %                   read the entire image. Default is [].
    %            'FlipUD' - Flip the image vertically so that row 1 is the
    %                   bottom of the frame, as in FITS. Default is false.
    %            'ReadHeader' - Build the header (see io.tiff.readHeader1).
    %                   Default is true when the Header output is requested.
    % Output : - Image matrix.
    %          - A 3-column cell array header (see io.tiff.readHeader1),
    %            with FLIPUD and, if a section was read, CCDSEC added.
    % Author : Sasha Krassilchtchikov (Sep 2026)
    % Example: [Im, H] = io.tiff.read1('frame.tif');
    %          Im = io.tiff.read1('frame.tif', 'CCDSEC',[1 100 1 50]);
    %          Im = io.tiff.read1('frame.tif', 'FlipUD',true);

    arguments
        FileName
        Args.Page(1,1) double        = 1;
        Args.CCDSEC                  = [];
        Args.FlipUD(1,1) logical     = false;
        Args.ReadHeader(1,1) logical = true;
    end

    Info = imfinfo(FileName);
    if Args.Page > numel(Info)
        error('io:tiff:read1:page', 'Page %d requested but %s has %d pages', Args.Page, FileName, numel(Info));
    end

    if isempty(Args.CCDSEC) || all(isinf(Args.CCDSEC))
        Image = imread(FileName, 'Index',Args.Page, 'Info',Info);
    else
        Nrows = Info(Args.Page).Height;
        Rows  = Args.CCDSEC(3:4);
        if Args.FlipUD
            % convert bottom-up rows to TIFF (top-down) rows
            Rows = Nrows + 1 - fliplr(Rows);
        end
        Image = imread(FileName, 'Index',Args.Page, 'Info',Info, ...
                       'PixelRegion',{Rows, Args.CCDSEC(1:2)});
    end

    if Args.FlipUD
        Image = flipud(Image);
    end

    if nargout > 1 && Args.ReadHeader
        Header = io.tiff.readHeader1(FileName, 'Page',Args.Page, 'Info',Info);
        Header(end+1,:) = {'FLIPUD', Args.FlipUD, 'Image flipped vertically on read'};
        if ~(isempty(Args.CCDSEC) || all(isinf(Args.CCDSEC)))
            Header(end+1,:) = {'CCDSEC', sprintf('[%d:%d,%d:%d]', Args.CCDSEC), 'Section read from the file'};
            Header{strcmp(Header(:,1), 'NAXIS1'), 2} = size(Image, 2);
            Header{strcmp(Header(:,1), 'NAXIS2'), 2} = size(Image, 1);
        end
    else
        Header = cell(0,3);
    end
end
