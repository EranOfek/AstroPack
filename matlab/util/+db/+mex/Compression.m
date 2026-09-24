% src/Compression.m
classdef Compression < int8
    % Compression  Native-protocol block compression methods.
    %
    %   Mirrors clickhouse-cpp's CompressionMethod enum
    %   (contrib/clickhouse-cpp/clickhouse/client.h). The underlying int8
    %   values MUST stay equal to the C++ enum so the value can be cast
    %   directly across the MEX boundary.
    %
    %   Use:
    %     opts.compression = Compression.LZ4;
    enumeration
        None (-1)
        LZ4  ( 1)
        ZSTD ( 2)
    end
end
