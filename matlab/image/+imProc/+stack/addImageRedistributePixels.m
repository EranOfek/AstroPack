function Image = addImageRedistributePixels(Image, Image1, X, Y, Args)
    % Add Image1 to Image according to the pixel redistribution defined by the coordinate arrays X, Y with the signal distributed amoung 4 neighbouring pixels
    % Package: imProc.stack
    % Description: Add Image1 to Image0 according to the pixel redistribution defined by the coordinate arrays X, Y 
    % with the signal distributed amoung 4 neighbouring pixels so that the total "flux" is conserved
    % Input:   - Image        : an existing image to be augmented (larger than Nx x Ny)
    %          - Image1       : the augmenting image (or a cutout thereof) of size Nx x Ny
    %          - X, Y         : 2D arrays of pixel coordinate redistribution (remapping), 
    %                           each of size Nx x Ny or smaller (if only part of Image1 is requested)
    %          * ...,key,val,...   
    %          'Nx'      : Nx rows of Image1 are remapped
    %          'Ny'      : Ny columns of Image1 are remapped
    %          'XL'      : first XL rows of Image1 are not remapped
    %          'YL'      : first YL columns of Image1 are not remapped
    %          'Method'  : 'redistribute' the signal between 4 neighbouring pixels of Image (default) 
    %                    : 'direct' put all the signal into the nearest pixel (some pixels of the Image will be empty)
    %          
    % Output : - Image (a 2D array): = Image + Image1[XL:XL+NX-1,YL:YL+NY-1] (redistributed)
    %            
    % Tested : Matlab R2020b
    % Author : A. Krassilchtchikov et al. (May 2023)
    % Example: Image = imProc.stack.addImageRedistributePixels(Image0, Image1, X, Y, 'Nx', NX, 'Ny', NY, 'XL', XL, 'YL', YL);

    arguments
        
        Image
        Image1
        X
        Y
        Args.Nx     = 0;  % Nx rows of Image1 are requested (if 0, use the full size of the image) 
        Args.Ny     = 0;  % Ny columns of Image1 are requested (if 0, use the full size of the image)  
        Args.XL     = 0;  % first XL rows of Image1 are not remapped
        Args.YL     = 0;  % first YL columns of Image1 are not remapped
        Args.Method = 'redistribute' % redistribute the signal (default) or put all the signal into the nearest pixel (some pixels of the Image will be empty)
        
    end
    
    % if Nx or Ny are not input explicitly, use the full size of Image1
    
    if Args.Nx == 0
        Args.Nx = size(Image1,1);
    end
    if Args.Ny == 0
        Args.Ny = size(Image1,2);
    end
    
    % check if there exists redistribution data for all the requested area of Image1 
    
    if size(X,1) < Args.Nx || size(Y,1) < Args.Nx || size(X,2) < Args.Ny || size(Y,2) < Args.Ny
        error('The size of a pixel redistribution array in addImageRedistributePixels is insufficient, exiting..');
    end
     
    %
    
    switch lower(Args.Method)
        
        case 'redistribute'  % redistribute the pixel signal upon 4 neighbouring pixels
    
            X0 = floor(X);   Y0 = floor(Y);    
            X1 = X-X0;       Y1 = Y-Y0;      X1Y1 = X1 .* Y1;       

            for iX = 1:1:Args.Nx 

                imX = iX + Args.XL; 

                for iY = 1:1:Args.Ny 

                    imY = iY + Args.YL; 

                    if numel(Image1) == 1 
                        S = Image1;  % if the new image is a constant (e.g., exposure time), we may still need such a redistribution matching that of the flux
                    else
                        S = Image1 (imX, imY); 
                    end

                    X11 = X0(iX,iY); Y11 = Y0(iX,iY);

                    Image ( X11  , Y11 )   = Image ( X11  , Y11 )   + S * ( 1 - X1(iX,iY) ) * (1 - Y1(iX,iY) );
                    Image ( X11+1, Y11+1 ) = Image ( X11+1, Y11+1 ) + S * X1Y1(iX,iY); 
                    Image ( X11+1, Y11 )   = Image ( X11+1, Y11 )   + S * X1(iX,iY) * (1 - Y1(iX,iY) );
                    Image ( X11  , Y11+1 ) = Image ( X11  , Y11+1 ) + S * ( 1 - X1(iX,iY) ) * Y1(iX,iY); 

                end
            end
            
        case 'direct' % put all the signal into the nearest pixel (some pixels of the Image will be empty)
            
            X0 = round(X); Y0 = round(Y);        

            for iX = 1:1:Args.Nx      
                
                imX = iX + Args.XL;
                    
                for iY = 1:1:Args.Ny                      
                    
                    imY = iY + Args.YL;    
                        
                    if numel(Image1) == 1 
                        S = Image1;  % if the new image is a constant (e.g., exposure time), we may still need such a redistribution matching that of the flux
                    else
                        S = Image1 (imX, imY); 
                    end
                       
                    Image ( X0(iX,iY), Y0(iX,iY) ) = Image ( X0(iX,iY), Y0(iX,iY) ) + S;  % add counts from the subimage to the mosaic
                       
                end
            end
            
        case 'interp' % using imresize, extremely slow.. 54774 seconds !
            
            Scale = 4; % resizing scale
            
            X0 = floor(X); Y0 = floor(Y);
            
            X1 = ceil( (2 * Scale - 1) .* (X-X0) ); Y1 = ceil( (2 * Scale - 1 ) .* (Y-Y0) );  
            
            for iX = 1:1:Args.Nx
                
                imX = iX + Args.XL;
                
                for iY = 1:1:Args.Ny
                    
                    imY = iY + Args.YL; 
                       
                    if numel(Image1) == 1 
                        S = Image1;  % if the new image is a constant (e.g., exposure time), we may still need such a redistribution matching that of the flux
                    else
                        S = Image1 (imX, imY); 
                    end
                    
                    X11 = X0(iX,iY); Y11 = Y0(iX,iY);
                    
                    Stamp = Image( X11:X11+1 , Y11:Y11+1 );
                    
                    StampScaled = imresize(Stamp, Scale, 'bilinear');
                    
                    StampScaled ( X1(iX,iY), Y1(iX,iY) ) = StampScaled ( X1(iX,iY), Y1(iX,iY) ) + S * Scale^2. ;
                    
                    Stamp = imresize(StampScaled, 1./Scale, 'bilinear');
                    
                    Image( X11:X11+1 , Y11:Y11+1 ) = Stamp;
                    
                end
                
            end
            
        otherwise
            
            error('Incorrect flux redistribution method, exiting..'); 
            
    end
    
    
end