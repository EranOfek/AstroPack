function result=unittestWriteSimple()
% Unit test for FITS.writeSimpleFITS(), using in fact io.fits.writeSimpleFITS()
%
% this test might need to be written in the canonical form for unit tests,
%  which I don't know
% Author: Enrico Segre, Feb 2023

    % test image with vertical gradient and central white square
    % (double, 0-:-1)
    im0=kron((0:1023)'/1023,ones(1,1000));
    im0(size(im0,1)/2+(-20:20),size(im0,2)/2+(-20:20))=1;
    imagesc(im0); colorbar
    
    imfile='/tmp/gradsquare.fits';
    
    % dummy header with a lot of entries
    nhead=150;
    header=cell(nhead,3);
    for k=1:nhead
        header(k,:)={sprintf('K%05d',k) ,rand(), 'random number'};
    end

    imtype={'int8','uint8','int16','uint16','int32','uint32','single','double'};
    result1=false(1,numel(imtype));
    result2=result1;
    for i=1:numel(imtype)
        % cast im0 to the desired type and scale it
        switch imtype{i}
            case 'int8'
                bmax=255; bzero=128;
            case 'uint8'
                bmax=255; bzero=0;
            case 'int16'
                bmax=65535; bzero=32678;
            case 'uint16'
                bmax=65535; bzero=0;
            case 'int32'
                bmax=2^32-1; bzero=2^31;
            case 'uint32'
                bmax=2^32-1; bzero=0;
            case 'single'
                bmax=1; bzero=0;
            case 'double'
                bmax=1; bzero=0;
        end
        im=cast(im0*bmax-bzero,imtype{i});
       
        try
            fprintf('testing image type %s:\n',imtype{i})
            % write simple FITS with automatic casting and BZERO
            %  (Astropack branch fits-u16)
            FITS.writeSimpleFITS(im, imfile, 'Header',header);
            
            % reread with FITS.read1()
            imReadFITS=FITS.read1(imfile);
            
            % this should be 0 if all was ok
            result1(i)=(numel(find(imReadFITS-im))==0);
            
            % reread with matlab.io.fits
            fptr = matlab.io.fits.openFile(imfile);
            imreadio = matlab.io.fits.readImg(fptr);
            matlab.io.fits.closeFile(fptr);
            
            % also this should be 0 if all was ok
            result2(i)=(numel(find(imreadio-im))==0);
            if result1(i) && result2(i)
                fprintf('test successful\n')
            else
                fprintf('test failed\n')
            end
        catch
            fprintf('---image type %s cannot be written\n',imtype{i})
        end
    end

    % final result
    result=all(result1 & result2);