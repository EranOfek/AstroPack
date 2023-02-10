function result=unittestWriteSimple()
% Unit test for FITS.writeSimpleFITS(), using in fact io.fits.writeSimpleFITS()
%
% this test might need to be written in the canonical form for unit tests,
%  which I don't know
% Author: Enrico Segre, Feb 2023

    % test image with vertical gradient and central white square
    im=kron(uint16(0:64:65535)',ones(1,1000,'uint16'));
    im(size(im,1)/2+(-20:20),size(im,2)/2+(-20:20))=uint16(65535);
    imagesc(im); colorbar
    
    imfile='/tmp/gradsquare.fits';
    
    % dummy header with a lot of entries
    nhead=150;
    header=cell(nhead,3);
    for k=1:nhead
        header(k,:)={sprintf('K%05d',k) ,rand(), 'random number'};
    end

    % write simple FITS with automatic casting and BZERO
    %  (Astropack branch fits-u16)
    FITS.writeSimpleFITS(im, imfile, 'Header',header);

    % reread with FITS.read1()
    imReadFITS=FITS.read1(imfile);

    % this should be 0 if all was ok
    result1=(numel(find(imReadFITS-im))==0);

    % reread with matlab.io.fits
    fptr = matlab.io.fits.openFile(imfile);
    imreadio = matlab.io.fits.readImg(fptr);
    matlab.io.fits.closeFile(fptr);

    % also this should be 0 if all was ok
    result2=(numel(find(imreadio-im))==0);

    % final result
    result=result1 & result2;