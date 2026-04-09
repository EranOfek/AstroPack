% Read a catalog from a FITS, FITS.fz, FITS.gz or FITS.bz2 file
% Compiled with: mex read_catalog.cpp -lcfitsio (after sudo apt install libcfitsio-dev)
% Input  : - file name (*fits or *fits.fz)
%          - HDU number (NB: if the number is 0, the function will scan all the HDUs and read the first image found)   
%          - HDU header - if we wish to read the header from a different HDU (optional) 
% Output : - a struct array (catalog, need to be converted to a matlab table with DataTable = struct2table(DataStruct);)
%          - a header 
%          - the hdu number where the catalog was found (optional)
% Author : A.M. Krassilchtchikov (2026 Apr) 
% Example: FN = '/home/kra/LAST/LAST.01.01.01_20260324.164951.543_clear_1268_000_001_001_sci_coadd_Cat_1.fits';
%          read a catalog from the first HDU where there is one, and read header specifically from HDU = 3
%          [DataStruct, Header, hdu] = fits.mex.read_catalog(FN, 0, [], {}, 3); 
%          Data = struct2table(DataStruct);
