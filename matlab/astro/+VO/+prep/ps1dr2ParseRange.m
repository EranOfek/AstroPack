function [DecLo, DecHi, RALo, RAHi] = ps1dr2ParseRange(FilePath)
    % Read RA/Dec range from HDF5 attributes written by ps1dr2CsvToHdf5.
    % Returns ranges in degrees (as expected by buildHTMfromFiles).
    %
    % Input  : - FilePath (string), full path to HDF5 file
    % Output : - DecLo, DecHi, RALo, RAHi [degrees]
    %
    % Author : Dana Kovaleva + Claude (2026 Mar)

    RAD = 180 / pi;
    try
        DecLo = h5readatt(FilePath, '/data', 'MinDec') * RAD;
        DecHi = h5readatt(FilePath, '/data', 'MaxDec') * RAD;
        RALo  = h5readatt(FilePath, '/data', 'MinRA')  * RAD;
        RAHi  = h5readatt(FilePath, '/data', 'MaxRA')  * RAD;
    catch
        DecLo = NaN; DecHi = NaN; RALo = NaN; RAHi = NaN;
    end
end
