function [Image, ResultingImage] = injectStamps(XY, PSF, Flux, Args)
    % Construct an image from object stamps + background 
    %     Optionally subtract the constructed image with another one or add to it
    % Input  : - an array of XY positions of the PSF's central pixels
    %          - a cube of PSF stamps (M x M x N objects) or a cell array of PSF stamps (for the case the PSF size varies)
    %            NB: currently the algorithm works for odd M only
    %          - an array of source fluxes
    %          * ...,key,val,...     
    %          'SizeX' - image size (if not empty, overrides the size defined by InputImage or by the set of XY)
    %          'SizeY' - image size (if not empty, overrides the size defined by InputImage or by the set of XY)
    %          'Back' - a background level
    %          'PositivePSF' - whether to suppress the PSF edges so that they smoothly come to zero
    %          'PSFfun' - the smoothing kernel function 
    %          'PSFfunpars' - the smoothing kernel parameters
    %          'AddNoise' - whether to add Poisson noise
    %          'InputImage' - an input image to be added or subracted 
    %          'Subtract' - whether to subtract the constructed image from
    %                       the InputImage
    % Output : - an artificial image of objects + (optional) background and noise 
    %          - a sum or difference of the InputImage and the artificial image (optional)
    % Author : A.M. Krassilchtchikov (2024 Apr) 
    % Example: Im = imUtil.art.injectStamps(XY,PSF,F,'Back',200.);

    arguments
        XY
        PSF
        Flux
        
        Args.SizeX             = [];   
        Args.SizeY             = [];
        Args.Back              = [];
        Args.PositivePSF logical = false;
        Args.PSFfun            = @imUtil.kernel2.cosbell;
        Args.PSFfunpars        = [4 6];   % should be different for different telescopes! 
        Args.AddNoise logical  = true;
                
        Args.InputImage        = [];
        Args.Subtract logical  = false;
    end

    ResultingImage = [];
    
    % input format conversion, sanity checks and image size   
    if ~iscell(PSF) % if PSF is a cube, make it a cell array
        [M,~,N] = size(PSF);
        PSF = squeeze(mat2cell(PSF, M, M, ones(1, N)));
        MaxM = M;
    else
        N = size(PSF);
        M = cellfun(@size, PSF, 'UniformOutput', false);
        MaxM = max(cellfun(@max, M));
    end
 
    Nim  = numel(Flux);  % number of object fluxes
    Ncoo = size(XY,1);   % number of object coordinates
    SizeInpIm  = size(Args.InputImage);
    SizeBackIm = size(Args.Back); 
    
    if abs(Nim-N) > 0 || abs(Ncoo-Nim) > 0 || abs(Ncoo-N) > 0 
        error('Input data dimensions do not match')
    end
    
    if SizeInpIm(1) > 1      % match the size of the input image, if it exists
        SizeX = SizeInpIm(1); SizeY = SizeInpIm(2);
    elseif SizeBackIm(1) > 1 % match the size of the background image, if it exists
        SizeX = SizeBackIm(1); SizeY = SizeBackIm(2);
    elseif ~isempty(Args.SizeX) && ~isempty(Args.SizeY) % use the explicit sizes
        SizeX = Args.SizeX; SizeY = Args.SizeY;
    else                     % determine the sizes from the object coordinates and the maximal PSF size
        SizeX = max(XY(:,1)) + (MaxM-1)/2;  
        SizeY = max(XY(:,2)) + (MaxM-1)/2;        
    end        
    
    Image = zeros(SizeX, SizeY);
    
    % eliminate negative PSF edges (may be not needed when a realistic bkg is provided?)  
    if Args.PositivePSF
        for Isrc = 1:Nim
            if iscell(M)
                PSFfunpars = ceil( Args.PSFfunpars .* M{Isrc}(1) / 15); % empiric, should somehow depend on M
            else
                PSFfunpars = ceil( Args.PSFfunpars .* M / 15); 
            end
            SupressedEdges = Args.PSFfun( PSFfunpars, size(PSF{Isrc}) ) .* PSF{Isrc};
            PSF{Isrc} = SupressedEdges ./ sum(SupressedEdges,'all');    % normalize 
        end
    end
            
    % construct a new artificial image      
    for Isrc = 1:Nim
        if iscell(M) % determine the PSF size
            PSFRad = (M{Isrc}(1)-1)/2;
        else
            PSFRad = (M-1)/2;
        end
        % determine the region to be filled in the image
        X1 = XY(Isrc,1)-PSFRad; X2 = XY(Isrc,1)+PSFRad;
        Y1 = XY(Isrc,2)-PSFRad; Y2 = XY(Isrc,2)+PSFRad;
        X11 = 1; Y11 = 1; X21 = 2*PSFRad + 1; Y21 = 2*PSFRad + 1; 
        % if the object is too close to a border, we need to cut the stamp
        if X1 < 1
            X11 = 2-X1; 
            X1  = 1;
        end
        if Y1 < 1
            Y11 = 2-Y1; 
            Y1  = 1;
        end
        if X2 > SizeX
            X21 = X21-(X2-SizeX);
            X2 = SizeX;
        end
        if Y2 > SizeY
            Y21 = Y21-(Y2-SizeY);
            Y2 = SizeY;
        end
        %
        Image(X1:X2,Y1:Y2) = Image(X1:X2,Y1:Y2) + Flux(Isrc) .* PSF{Isrc}(X11:X21,Y11:Y21);                
    end
       
    % add background
    if ~isempty(Args.Back)
        if numel(Args.Back) < 2 || ( (SizeBackIm(1) == SizeX) &&  (SizeBackIm(2) == SizeY) )
            Image = Image + Args.Back;
        else
            error('The background image size does not match that of the source image'); 
        end
    end
    
    % add noise
    if Args.AddNoise
        normrnd( Image, sqrt(Image), SizeX, SizeY ); 
    end
    
    % if requested, subtract the new image from the InputImage or add to it
    if ~isempty(Args.InputImage)
        if Args.Subtract
            ResultingImage = Args.InputImage - Image;
        else
            ResultingImage = Args.InputImage + Image;
        end
    end    
end
