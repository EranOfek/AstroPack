function Header = readHeader1(FileName, Args)
    % Read the metadata of a TIFF file into a FITS-like 3-column header cell.
    % Input  : - TIFF file name.
    %          * ...,key,val,...
    %            'Page' - Page (directory) number in a multi-page TIFF.
    %                   Default is 1.
    %            'Info' - imfinfo structure array of the file. If empty,
    %                   call imfinfo. Default is [].
    % Output : - A 3-column cell array {Key, Value, Comment} with FITS-style
    %            keys: NAXIS, NAXIS1, NAXIS2, BITPIX, NPAGES, TIFFPAGE,
    %            COMPRESS, PHOTOMET, SOFTWARE, ORIGFILE, FILEDATE, and,
    %            when the corresponding TIFF tags exist, DATE-OBS (from the
    %            DateTime tag, converted to ISO format) and IMGDESC.
    % Author : Sasha Krassilchtchikov (Sep 2026)
    % Example: H = io.tiff.readHeader1('frame.tif');
    %          H = io.tiff.readHeader1('stack.tif', 'Page',3);

    arguments
        FileName
        Args.Page(1,1) double = 1;
        Args.Info             = [];
    end

    if isempty(Args.Info)
        Info = imfinfo(FileName);
    else
        Info = Args.Info;
    end
    Npages = numel(Info);
    if Args.Page > Npages
        error('io:tiff:readHeader1:page', 'Page %d requested but %s has %d pages', Args.Page, FileName, Npages);
    end
    I = Info(Args.Page);

    % TIFF sample format: 1 unsigned int, 2 signed int, 3 float
    Bits = I.BitsPerSample(1);
    SampleFormat = 1;
    if isfield(I, 'SampleFormat') && ~isempty(I.SampleFormat)
        SampleFormat = tiffSampleFormatCode(I.SampleFormat);
    end
    switch SampleFormat
        case 3
            BitPix = -Bits;
        otherwise
            BitPix = Bits;
    end

    [~, Name, Ext] = fileparts(I.Filename);

    Header = {'NAXIS',    2,                      'Number of axes';
              'NAXIS1',   I.Width,                'Image width [pix]';
              'NAXIS2',   I.Height,               'Image height [pix]';
              'BITPIX',   BitPix,                 'Bits per sample (negative for float)';
              'NPAGES',   Npages,                 'Number of pages in the TIFF file';
              'TIFFPAGE', Args.Page,              'Page read from the TIFF file';
              'COMPRESS', I.Compression,          'TIFF compression';
              'PHOTOMET', I.PhotometricInterpretation, 'TIFF photometric interpretation';
              'ORIGFILE', [Name, Ext],            'Original file name';
              'FILEDATE', fileModTimeUTC(I.Filename), 'File modification time (UTC)'};

    if isfield(I, 'Software') && ~isempty(I.Software)
        Header(end+1,:) = {'SOFTWARE', strtrim(I.Software), 'TIFF Software tag'};
    end
    if isfield(I, 'DateTime') && ~isempty(I.DateTime)
        % TIFF DateTime tag format: 'YYYY:MM:DD HH:MM:SS'
        DateObs = regexprep(strtrim(I.DateTime), '^(\d{4}):(\d{2}):(\d{2}) ', '$1-$2-$3T');
        Header(end+1,:) = {'DATE-OBS', DateObs, 'From TIFF DateTime tag'};
    end
    if isfield(I, 'ImageDescription') && ~isempty(I.ImageDescription)
        Header(end+1,:) = {'IMGDESC', strtrim(I.ImageDescription), 'TIFF ImageDescription tag'};
    end
end

function Code = tiffSampleFormatCode(SF)
    % imfinfo may report SampleFormat as a number or as a string
    if isnumeric(SF)
        Code = SF(1);
    else
        switch lower(strtrim(SF))
            case {'ieee floating point', 'float'}
                Code = 3;
            case {'two''s complement signed integer', 'int'}
                Code = 2;
            otherwise
                Code = 1;
        end
    end
end

function Str = fileModTimeUTC(FileName)
    % File modification time as an ISO string in UTC
    D = dir(FileName);
    T = datetime(D(1).datenum, 'ConvertFrom','datenum', 'TimeZone','local');
    T.TimeZone = 'UTC';
    Str = char(datetime(T, 'Format','yyyy-MM-dd''T''HH:mm:ss'));
end
