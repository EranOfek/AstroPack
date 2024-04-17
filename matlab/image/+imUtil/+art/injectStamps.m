function [Image, ResultingImage] = injectStamps(XY, PSF, Flux, Args)
    % Construct an image from object stamps + background 
    %     
    % Input  : - an array of XY positions of the PSF's central pixels
    %          - a cube of PSF stamps (M x M x N objects) or a cell array of PSF stamps (for the case the PSF size varies)
    %            NB: currently the algorithm works for odd M only
    %          - an array of source fluxes
    %          - 
    %          - 
    %          * ...,key,val,...     
    %          'SizeX' - image size (if not empty, overrides the size defined by InputImage or by the set of XY)
    %          'SizeY' - image size (if not empty, overrides the size defined by InputImage or by the set of XY)
    %          'Back' - a background level
    %          'AddNoise' - whether to add Poisson noise
    %          'InputImage' - an input image to be added or subracted 
    %          'Subtract' - whether to subtract the constructed image from
    %                       the InputImage
    % Output : - an artificial image of objects + background  
    %          - a sum or difference of the InputImage and the artificial image (optional)
    % Author : A.M. Krassilchtchikov (2024 Apr) 
    % Example: Im = imUtil.art.injectStamps(XY,PSF,F);

    arguments
        XY
        PSF
        Flux
        
        Args.SizeX             = [];   
        Args.SizeY             = [];
        Args.Back              = [];
        Args.PositivePSF logical = true;
        Args.PSFfun            = @imUtil.kernel2.cosbell;
        Args.CleanPSFpars      = [4 6];   % should be different for different telescopes! 
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
 
    Nim = numel(Flux);
    Ncoo = size(XY,1);    
    SizeInpIm = size(Args.InputImage);
    
    if abs(Nim-N) > 0 || abs(Ncoo-Nim) > 0 || abs(Ncoo-N) > 0
        error('Input data dimensions do not match')
    end
    
    InpSizeX = max(XY(:,1))-min(XY(:,1))+1 + MaxM+1; 
    InpSizeY = max(XY(:,2))-min(XY(:,2))+1 + MaxM+1;
    
    if isempty(Args.SizeX) && isempty(Args.SizeY)
        SizeX = max(SizeInpIm(1),InpSizeX);
        SizeY = max(SizeInpIm(2),InpSizeY);
    else
        SizeX = Args.SizeX;
        SizeY = Args.SizeY;
    end
    
    Image = zeros(SizeX, SizeY);
    
    % eliminate negative PSF edges (may be not needed with realistic bkg?)  
    if Args.PositivePSF
        for Isrc = 1:Nim
            if iscell(M)
                CleanPSFpars = ceil( Args.CleanPSFpars .* M{Isrc}(1) / 15); % empiric, should somehow depend on M?
            else
                CleanPSFpars = ceil( Args.CleanPSFpars .* M / 15); 
            end
            SupressedEdges = Args.PSFfun( CleanPSFpars, size(PSF{Isrc}) ) .* PSF{Isrc};
            PSF{Isrc} = SupressedEdges ./ sum(SupressedEdges,'all');
        end
    end
            
    % construct a new artificial image      
    for Isrc = 1:Nim
        if iscell(M)
            PSFRad = (M{Isrc}(1)-1)/2;
        else
            PSFRad = (M-1)/2;
        end
        RangeX = XY(Isrc,1)-PSFRad:XY(Isrc,1)+PSFRad;
        RangeY = XY(Isrc,2)-PSFRad:XY(Isrc,2)+PSFRad;
        if RangeX(1) > 0 && RangeY(1) > 0
            Image(RangeX,RangeY) = Image(RangeX,RangeY) + Flux(Isrc) .* PSF{Isrc};
        else
            % need to cut the range and the PSF to make images at the
            % borders 
        end
    end
    
    % add background
    if ~isempty(Args.Back)
        if numel(Args.Back) < 2 || size(Args.Back) == size(Image)
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
