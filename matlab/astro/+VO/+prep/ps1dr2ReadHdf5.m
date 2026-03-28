function Mat = ps1dr2ReadHdf5(FileName)
    % Read PS1DR2 HDF5 file produced by ps1dr2CsvToHdf5.
    % PostReadFun for use with buildHTMfromFiles.
    % Data is already transformed (RA/Dec in radians, -999 replaced,
    % delta columns computed), so this just reads and returns the matrix.
    %
    % Input  : - FileName (string), path to HDF5 file
    % Output : - Mat (Nrows x 56 double matrix)
    %
    % Author : Dana Kovaleva + Claude (2026 Mar)

    Mat = h5read(FileName, '/data');
end
