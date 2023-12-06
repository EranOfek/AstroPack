function fitswrite(Data,FileName,varargin)
% Write a FITS file with a single image in HDU=1
% Package: +imUtil.util.fits
% Input  : - Data
%          - File Name.
%          * Pairs of ...,key,val,... Possible keywords include:
%            'Header' - A 3 column cell array of [key, val, comment].
%                       Third column is optional.
%                       Alternatively, and headCl or HEAD class.
%                       Default is {}.
%            'Type' - Data class. Default is 'single'.
%            'HDU' - Default is 1 (FFU).
% imUtil.util.fits.fitswrite(rand(10,10),'aaa.fits')



InPar = inputParser;

addOptional(InPar,'Header',{});
addOptional(InPar,'Type','single');
addOptional(InPar,'HDU',1);  % non 1 will not work
parse(InPar,varargin{:});
InPar = InPar.Results;

Map = {'byte_img','uint8';...
       'short_img','int16';...
       'long_img','int32';...
       'longlong_img','int64';...
       'float_img','single';...
       'double_img','double'};

% convert data to type
Data = cast(Data,InPar.Type);

SizeXY = fliplr(size(Data));

if iscell(InPar.Header)
    Header = InPar.Header;
elseif isa(InPar.Header,'HEAD')
    Header = InPar.Header.Header;
elseif isa(InPar.Header,'headCl')
    Header = InPar.Header.Header;
else
    error('Unknown Header type');
end
if ~isempty(Header)
    Header = headCl.fixColCell(Header);  % add comment column
end

Nhead  = size(InPar.Header,1);  % number of keys in header

BitPix = Map{strcmp(InPar.Type,Map(:,2)),1};

% write image

if InPar.HDU==1
    fptr = matlab.io.fits.createFile(FileName);
    matlab.io.fits.createImg(fptr,InPar.Type,SizeXY);
else
    fptr = matlab.io.fits.openFile(FileName,'readwrite');

    %matlab.io.fits.movAbsHDU(fptr,InPar.HDU);
    matlab.io.fits.insertImg(fptr,BitPix,SizeXY);
end

for Ihead=1:1:Nhead
    %Header{Ihead,:}
    switch lower(Header{Ihead,1})
        case 'comment'
            matlab.io.fits.writeComment(fptr,Header{Ihead,2});
        case 'history'
            matlab.io.fits.writeHistory(fptr,Header{Ihead,2});
        case 'end'
            % do nothing
        otherwise
            if isnan(Header{Ihead,2})
                Header{Ihead,2} = 'NaN';
            end
            
            if isempty(Header{Ihead,3})
                matlab.io.fits.writeKey(fptr,Header{Ihead,1:2});
            else
                matlab.io.fits.writeKey(fptr,Header{Ihead,:});
            end
    end
end

matlab.io.fits.writeImg(fptr,Data);

matlab.io.fits.closeFile(fptr);


