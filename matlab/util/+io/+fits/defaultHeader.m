function HeaderCell=defaultHeader(DataClass, DataSize)
    % Generate a default (minimal) FITS header
    % Input  : - Data class type.
    %          - Image [I, J] size.
    % Output : - A 3 columns Header cell.
    % Author : Eran Ofek (Jan 2022)
    % Example: HeaderCell=io.fits.defaultHeader('single', [10 100])
    
    BitPix = io.fits.dataType2bitpix(DataClass);
        
    HeaderCell = {'SIMPLE',true,'';
                  'BITPIX',BitPix,'';
                  'NAXIS',numel(DataSize),'';
                  'NAXIS1',DataSize(2),'';
                  'NAXIS2',DataSize(1),'';
                  'EXTEND',false,'';
                  'END','',''};
end