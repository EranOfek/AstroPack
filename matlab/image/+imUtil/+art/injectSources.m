function S = injectSources(Image,Cat,PSFin,Args)
    % Inject artificial sources into an image.
    %   The function build the sources image with user provided PSF and catalogs.
    %   The function does not treat nans. Make sure that non of the inputs 
    %    contain nans.
    %       
    % Input  :  - An image (matrix) to inject the source. In case of two
    %             components vector (i.e., [1200,1000]), the function will
    %             inject sources to an empty image with size set by Image. 
    %             For a scalar, the function will inject into an equal
    %             sized empty image. In case of a matrix with more than two
    %             elements, the function will add the injected sources to
    %             the input image.
    %           - A three column vector containing X, Y and flux, for each
    %             source. 
    %           - PSF stamps. Either a single PSF stamp (2D) or PSF stamp per source
    %             in a 3D matrix, where the size of third dimension equal the nubmer of sources.
    %             The codes work with symmetric PSF (i.e., odd stamp size)
    %
    %           * ...,key,val,...
    %             'RecenterPSF' - a logical indicating if to set the center
    %                   of the PSF by the first moment instead of the central pixel. 
    %                   Default is false.
    %
    % Output : - An image with the injected sources (matrix).
    % Author : Noam Segev (Jan 2023)
    %          refactored by Enrico Segre (Jul 2023)
    % Example: PSF = imUtil.kernel2.gauss; 
    %          Cat = [rand(10,2)*100,rand(10,1)*1e5];
    %          S = imUtil.art.injectSources(100,Cat,PSF)
    
    arguments
        Image;
        Cat;
        PSFin;
        Args.RecenterPSF = false;
    end

    if numel(Image)==2     % Assume the output is imagesize
        S = zeros(Image);
    elseif numel(Image)==1 % In case of scalar
        S = zeros([Image,Image]);
    else
        S = Image;
    end

    [SizeImageY,SizeImageX] = size(S);

    X = Cat(:,1);
    Y = Cat(:,2);
    flux = Cat(:,3);
    DX = mod(X,1);
    DY = mod(Y,1);
    Xround=  floor(X);
    Yround=  floor(Y);
    Nsrc = numel(flux);
    Npsf = size(PSFin,3);
    if Npsf~=1 && Npsf ~= Nsrc
        error("injectSources must get either a single PSF or as many PSFs as sources")
    end

    for i = 1:Nsrc

        if Npsf>1 || i==1
            PSF(:,:) = PSFin(:,:,i);
            [SizePSFX,SizePSFY] = size(PSF);
            Xcenter = ceil(SizePSFX./2);
            Ycenter = ceil(SizePSFY./2);

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
            PSF_shifted = imUtil.trans.shift_fft(PSF,DX,DY);
        end

        Xind = matx+ Xround(i);
        Yind = maty+ Yround(i);

        flag = ~(Xind<=0 | Yind<=0 | Xind>SizeImageX | Yind>SizeImageY);
        %ind = sub2ind([SizeImageY,SizeImageX],Yind(flag),Xind(flag));
        Ind = imUtil.image.sub2ind_fast([SizeImageY,SizeImageX],Yind(flag),Xind(flag));

        % ind(isnan(ind))=[]; % ? for what pathological case ind may be NaN?

        psf_t = squeeze(PSF_shifted(:,:,i));
        S(Ind) = S(Ind) +  psf_t(flag(:)).*flux(i);

    end

end


