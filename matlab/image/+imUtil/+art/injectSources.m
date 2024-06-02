function Image = injectSources(Image, SrcPSF, XY, Args)
    % Inject odd-sized fluxed source images (PSFs) into whole pixel positions of an image  
    %     NB: first one needs to prepare fluxed and shifted source PSFs with imUtil.art.createSourceCube   
    % Input  : - an image matrix  
    %          - a prepared cube or cell array of fluxed source PSFs whose scaling fits to that of the Image  
    %          - a prepared list of whole pixel positions   
    %          * ...,key,val,... 
    %          'Size' - [X Y] a forced size of the resulting image [employed only if any(size(Image) < 2)]    
    %          'Subtract' - whether to add or subtract the source images 
    %          'Oversample' - oversampling of the PSF stamps (1 value or a vector)
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
        Args.Size       = []; 
        Args.Subtract   = false;
        Args.Oversample = [];
    end    
    % if the PSF is not to scale, call the old directInjectSources function:
    if ~isempty(Args.Oversample)
        Flux = repmat(1.0,1,size(SrcPSF,3));   
        Cat = [XY(:,1) XY(:,2) Flux'];
        Image = directInjectSources(Image, Cat, Args.Oversample, SrcPSF);
        return
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

%%%
%%% internal functions
%%%

function Image = directInjectSources (Image0, Cat, Scaling, PSF)
    % Inject sources to catalog positions with PSFs scaled by the Scaling factor 
    % Package: imUtil.art
    % Description: Inject sources to catalog positions with PSFs scaled by the Scaling factor 
    % Input:   - Image0: a 2D array containing the initial image 
    %          - Cat: an 3-column table: X, Y, full band flux normalization
    %          - Scaling: a scaling factor, typically > 1
    %          - PSF: a 2+1 D array of source PSFs
    %          NB: the PSF stamp for all the sources is the same
    % Output : - Image: a 2D array containing the resulting image
    % Author : A. Krassilchtchikov et al. (Feb 2023)
    % Example: Image1 = imUtil.art.directInjectSources (Image0,Cat,Scaling,PSF)

    % image summation methods:     
    Method = 'Regular'; % 'Pad'     : summ full matrices
                         % 'Regular' : add the PSF stamp values in cycles
        
    % rescale the initial image to the PSF scale:    
    Im = imresize(Image0, Scaling, 'bilinear');
    SizeImX = size(Im,1);
    SizeImY = size(Im,2);
        
    % add the sources PSFs
    SizeX  = size(PSF,1);
    SizeY  = size(PSF,2);
    NumSrc = size(PSF,3);     
%     Src    = zeros( SizeX, SizeY );
    
    for Isrc = 1:1:NumSrc        
        % rescale the source coordinates        
        Xcenter = Scaling * Cat(Isrc,1);
        Ycenter = Scaling * Cat(Isrc,2);
        
        % define the stamp borders in the rescaled image        
        Xleft   = max( floor( Xcenter - SizeX/2. ), 1);
        Yleft   = max( floor( Ycenter - SizeY/2. ), 1);
        Xright  = min( Xleft + SizeX, SizeImX);
        Yright  = min( Yleft + SizeY, SizeImY);
        SzX     = Xright-Xleft;
        SzY     = Yright-Yleft;
        
        switch lower(Method)            
            case 'pad'                
                % pad the stamp with zeros upto the full image size and add the images
        
                PadXL   = max(Xleft-1, 0);
                PadXR   = max(SizeImX-Xright+1, 0);
                PadYL   = max(Yleft-1, 0);
                PadYR   = max(SizeImY-Yright+1, 0);  

                Src = PSF(:,:,Isrc) .* Cat(Isrc,3);

                Src = padarray(Src,[PadXL 0],'pre'); 
                Src = padarray(Src,[PadXR 0],'post'); 
                Src = padarray(Src,[0 PadYL],'pre'); 
                Src = padarray(Src,[0 PadYR],'post');

                Im = Im + Src .* Scaling^2;  
                % NB! "imresize" scales the sum of the counts as Scale^2, so we need to scale the added signal
            case 'regular'           
                for iX = 1:1:SzX
                    for iY = 1:1:SzY
                        Im( Xleft+iX-1, Yleft+iY-1 ) = Im( Xleft+iX-1, Yleft+iY-1) + ...
                            PSF(iX, iY, Isrc) .* Cat(Isrc,3) .* Scaling^2; 
                        % NB! "imresize" scales the sum of the counts as Scale^2, so we need to scale the added signal
                    end
                end
            otherwise            
                fprintf('Summation method not defined!\n');            
        end                
    end
    % scale down to the original pixel size:    
    Image = imresize(Im, 1./Scaling, 'bilinear');
end
