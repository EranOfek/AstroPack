function S = injectSources(Image,Cat,PSFin,Args)
    % Inject artificial sources into an image.
    %   The function user provided PSF and catalogs an build the sources image.
    % 
    % Input  :  - An image (matrix) to inject the source. In case of two
    %             components vector (i.e., [1200,1000]), the function will
    %             inject sources to an empty image with size set by Image. 
    %             In case of scalar, the function will inject into an equal
    %             size empty image.
    %        :  - A three column vector contains X, Y and flux, for each
    %             source.
    %        :  - PSF stamp. The codes work with symmetric PSF (i.e., odd
    %        stamp size)

    arguments

        Image;
        Cat;
        PSF;
        Args.RecenterPSF = false;
    
    end

    if numel(Image)==2
        S = zeros(Image);
    elseif numel(Image)==1 % In case of scalar
        S = zeros([Image,Image]);
    end

    [SizeImageX,SizeImageY] = size(S);

    X = Cat(:,1);
    Y = Cat(:,2);
    flux = Cat(:,3);
    DX = mod(X,1);
    DY = mod(Y,1);
    Xround=  floor(X);
    Yround=  floor(Y);
    Nsrc = numel(flux);

switch size(PSFin,3) 

case Nsrc % for each source an individual PSF is provided

    for i = 1:Nsrc

        PSF(:,:) = PSFin(:,:,i);

        [SizePSFX,SizePSFY] = size(PSF);
        Xcenter = ceil(SizePSFX./2);
        Ycenter = ceil(SizePSFX./2);

        if Args.RecenterPSF
            momt = imUtil.image.moment2(PSF,SizePSFX,SizePSFY);
            DX = DX - (Xcenter - momt.X);
            DY = DY - (Ycenter - momt.Y);
        end

        Xind_vec = 1:1:SizePSFX;
        Yind_vec = 1:1:SizePSFY;
        VecX= Xind_vec-Xcenter;
        VecY= Yind_vec-Ycenter;
        [matx,maty]= meshgrid(VecX,VecY);
        PSF_shiftted = imUtil.trans.shift_fft(PSF,DX,DY);

        Xind = matx+ Xround(i);
        Yind = maty+ Yround(i);

        flag = ~(Xind <=0 | Yind<=0 |Xind>SizeImageX| Yind>SizeImageY);
        ind = sub2ind([SizeImageX,SizeImageY],Yind(flag),Xind(flag));

        ind(isnan(ind))=[];

        psf_t = squeeze(PSF_shiftted(:,:,i));
        S(ind) = S(ind) +  psf_t(flag(:)).*flux(i);

    end

case 1    % one PSF for all the sources

    PSF(:,:) = PSFin(:,:,1);
    
    [SizePSFX,SizePSFY] = size(PSF);
    Xcenter = ceil(SizePSFX./2);
    Ycenter = ceil(SizePSFX./2);

    if Args.RecenterPSF
        momt =imUtil.image.moment2(PSF,SizePSFX,SizePSFY);
        DX = DX - (Xcenter - momt.X);
        DY = DY - (Ycenter - momt.Y);
        
    end

    Xind_vec = 1:1:SizePSFX;
    Yind_vec = 1:1:SizePSFY;
    VecX= Xind_vec-Xcenter;
    VecY= Yind_vec-Ycenter;
    [matx,maty]= meshgrid(VecX,VecY);
    PSF_shiftted = imUtil.trans.shift_fft(PSF,DX,DY);
    
    for i = 1:Nsrc
        
        
        Xind = matx+ Xround(i);
        Yind = maty+ Yround(i);
        
        flag = ~(Xind <=0 | Yind<=0 |Xind>SizeImageX| Yind>SizeImageY);
        ind = sub2ind([SizeImageX,SizeImageY],Yind(flag),Xind(flag));
        
        ind(isnan(ind))=[];

        psf_t = squeeze(PSF_shiftted(:,:,i));
        S(ind) = S(ind) +  psf_t(flag(:)).*flux(i); 
    
    end

end
    
end




