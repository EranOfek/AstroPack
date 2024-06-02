function Image = injectSources(Image, SrcPSF, XY, Args)
    % Inject odd-sized fluxed source images (PSFs) into whole pixel positions of an image  
    %     NB: first one needs to prepare fluxed and shifted source PSFs with imUtil.art.createSourceCube   
    % Input  : - an image matrix  
    %          - a prepared cube or cell array of fluxed source PSFs whose scaling fits to that of the Image  
    %          - a prepared list of whole pixel positions   
    %          * ...,key,val,... 
    %          'Size' - [X Y] a forced size of the resulting image [employed only if any(size(Image) < 2)]    
    %          'Subtract' - whether to add or subtract the source images 
    % Output : - an image with injected source PSFs  
    % Author : A.M. Krassilchtchikov (2024 May) 
    % Example: for i = 1:10; P(:,:,i) = imUtil.kernel2.gauss([4 4 0],[24 24]) + 1e-2*rand(24,24); end
    %          X1Y1 = 100.*rand(10,2); Flux = 100.*rand(10,1);
    %          [CubePSF, XY] = imUtil.art.createSourceCube(P, X1Y1, Flux, 'Oversample', 3, 'PositivePSF', true);
    %          Image = rand(100);
    %          ImageSrc = imUtil.art.injectSources(Image,CubePSF,XY);
    arguments
        Image
        SrcPSF
        XY
        Args.Size     = []; 
        Args.Subtract = false;
    end    
    % sanity checks and PSF size 
    if iscell(SrcPSF)             
        Nsrc   = size(SrcPSF);
        M      = cellfun(@size, SrcPSF, 'UniformOutput', false);
        if any(cellfun(@(x) any(mod(x, 2) == 0), M)) 
            error('Input PSFs must be odd-sized')
        end
        MaxM   = max(cellfun(@max, M));
        PSFRad = cellfun(@(x) (x(1)-1)/2,M);
    elseif isnumeric(SrcPSF)      
        [M,~,Nsrc] = size(SrcPSF); 
        if rem(M,2) == 0
            error('Input PSFs must be odd-sized')
        end
        MaxM       = M; 
        PSFRad     = (M-1)/2; 
    else
        error('Input PSF format is invalid');        
    end
        
    if abs(size(XY,1)-Nsrc) > 0  
        error('The length of the coordinate list and the PSF stack size do not match')
    end
            
    % output image size     
    if all(size(Image) > 1)       % match the size of the input image, if it exists
        ImSize = size(Image);
    elseif ~isempty(Args.ImSize)  % use the explicit sizes
        ImSize = Args.ImSize; 
    else                          % determine the sizes from the object coordinates and the maximal PSF size
        ImSize(1) = max(XY(:,1)) + (MaxM-1)/2;  
        ImSize(2) = max(XY(:,2)) + (MaxM-1)/2;        
    end        
        
    % construct a source image    
    SrcImage = repmat(0,ImSize);     
    
    for Isrc = 1:Nsrc
        % determine the region to be filled in the image
        if iscell(SrcPSF) % individual PSF size
            X1 = XY(Isrc,1)-PSFRad(Isrc); X2 = XY(Isrc,1)+PSFRad(Isrc);
            Y1 = XY(Isrc,2)-PSFRad(Isrc); Y2 = XY(Isrc,2)+PSFRad(Isrc);
            X11 = 1; Y11 = 1; X21 = 2*PSFRad(Isrc) + 1; Y21 = 2*PSFRad(Isrc) + 1;
        else        
            X1 = XY(Isrc,1)-PSFRad; X2 = XY(Isrc,1)+PSFRad;
            Y1 = XY(Isrc,2)-PSFRad; Y2 = XY(Isrc,2)+PSFRad;
            X11 = 1; Y11 = 1; X21 = 2*PSFRad + 1; Y21 = 2*PSFRad + 1;
        end
        % if the object is too close to a border, we need to cut the stamp
        if X1 < 1
            X11 = 2-X1; 
            X1  = 1;
        end
        if Y1 < 1
            Y11 = 2-Y1; 
            Y1  = 1;
        end
        if X2 > ImSize(1)
            X21 = X21-(X2-ImSize(1));
            X2 = ImSize(1);
        end
        if Y2 > ImSize(2)
            Y21 = Y21-(Y2-ImSize(2));
            Y2 = ImSize(2);
        end
        %
        if iscell(SrcPSF)
            SrcImage(X1:X2,Y1:Y2) = SrcImage(X1:X2,Y1:Y2) + SrcPSF{Isrc}(X11:X21,Y11:Y21);
%             tools.array.updateMatrixInplace(Image, SrcPSF{Isrc}, X1, Y1, X11, Y11, X21-X11+1, Y21-Y11+1);
        else
            SrcImage(X1:X2,Y1:Y2) = SrcImage(X1:X2,Y1:Y2) + SrcPSF(X11:X21,Y11:Y21,Isrc); 
             % this is quite slow, can to be replaced by a mex-function of Chen Tishler?
%              try
%                  tools.array.updateMatrixInplace(Image, SrcPSF(:,:,Isrc), X1, Y1, X11, Y11, X21-X11+1, Y21-Y11+1);
%              catch
%                  X1;
%              end
        end
    end
    
    % add or subract the source image from the sky image:
    if Args.Subtract
        Image = Image - SrcImage;
    else
        Image = Image + SrcImage;
    end
end
