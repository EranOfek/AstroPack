function [BitPix,bzero] = dataType2bitpix(Class)
    % Convert class name to BITPIX keyword value number.
    % Input  : - A class (e.g., 'single').
    % Output : - A BITPIX value.
    %          - the recommended BZERO value for the header, for unsigned
    %            formats
    % Author : Eran Ofek (Jan 2022) (bzero added by Enrico Segre Feb 2023)
    % Eaxmple: BitPix = io.fits.dataType2bitpix('int16')
    
    switch lower(Class)
        case {'int8','uint8'}
            BitPix = 8;
        case {'int16','uint16'}
            BitPix = 16;
        case {'int32','uint32'}
            BitPix = 32;
        case {'int64','uin64'}
            BitPix = 64;
        case {'single'}
            BitPix = -32;
        case {'double'}
            BitPix = -64;
        otherwise
            error('Unknown Class option');
    end
    switch lower(Class)
        case 'int8'
            bzero=-128;
        case 'uint16'
            bzero=2^15;
        case 'uint32'
            bzero=2^31;
        otherwise
            bzero=0;
    end
end
            
            