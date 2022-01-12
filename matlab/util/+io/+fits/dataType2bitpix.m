function BitPix = dataType2bitpix(Class)
    % Convert class name to BITPIX keyword value number.
    % Input  : - A class (e.g., 'single').
    % Output : - A BITPIX value.
    % Author : Eran Ofek (Jan 2022)
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
end
            
            