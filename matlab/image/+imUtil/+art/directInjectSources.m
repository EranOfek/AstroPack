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
    %            
    % Tested : Matlab R2020b
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
