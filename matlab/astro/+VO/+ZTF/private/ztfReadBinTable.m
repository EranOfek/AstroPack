function [Col, Head] = ztfReadBinTable(FileName)
    % Read a ZTF catalog FITS binary table into a struct of columns.
    % Description: fitsread keeps SExtractor's vector columns (MAG_APER and
    %              friends) as [Nrow x Naper] blocks, which is what the
    %              aperture expansion needs; FITS.readTable1 flattens them.
    %              The header is returned as ONE cell, primary followed by
    %              the table's own, because ZTF puts the photometric
    %              keywords in the primary header of a psfcat and in the
    %              binary-table header of a sexcat.
    % Input  : - FileName - path to a psfcat/sexcat FITS file.
    % Output : - Col  - struct, one field per column (named by its TTYPE),
    %                   each [Nrow x 1] or [Nrow x Nelement], class double.
    %          - Head - [N x 3] cell {Keyword, Value, Comment}, primary
    %                   header first, then the binary-table header.
    % Author : D. Kovaleva (Oct 2026)

    Info = fitsinfo(FileName);
    if ~isfield(Info, 'BinaryTable') || isempty(Info.BinaryTable)
        error('VO:ZTF:ztfReadBinTable:NoTable', '%s has no binary table', FileName);
    end

    Keys = Info.BinaryTable(1).Keywords;
    IsT  = strncmp(Keys(:,1), 'TTYPE', 5);
    Names = Keys(IsT, 2);
    Data  = fitsread(FileName, 'binarytable', 1);

    Col = struct();
    for Ic = 1:min(numel(Names), numel(Data))
        Name = matlab.lang.makeValidName(char(string(Names{Ic})));
        Col.(Name) = double(Data{Ic});
    end

    Head = Info.PrimaryData.Keywords;
    Head = [Head; Keys];
end
