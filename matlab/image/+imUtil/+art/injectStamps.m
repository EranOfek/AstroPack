function [Image, ResultingImage] = injectStamps(XY, PSF, Flux, Args)
    % Construct an image from object stamps + background 
    %     Optionally subtract the constructed image with another one or add to it
    %     !!! NB: currently the algorithm works for odd M only
    % Input  : - an array of XY positions of the PSF's central pixels
    %          - a cube of PSF stamps (M x M x N objects), a cell array of PSF stamps (for the case the PSF size varies), 
    %            or juts one PSF stamp (M x M) 
    %          - an array of source fluxes or just 1 flux for all the stamps
    %          * ...,key,val,...     
    %          'ImSize'      - image size (if not empty, overrides the size defined by the set of XY)
    %          'Back'        - a background image of a single-valued background level
    %          'PositivePSF' - whether to suppress the PSF edges so that they smoothly come to zero
    %          'FunEdge' - the smoothing kernel function 
    %          'FunEdgePars' - the smoothing kernel parameters
    %          'AddNoise'    - whether to add Poisson noise 
    %          'InputImage'  - an input image to be added or subracted 
    %          'Subtract'    - whether to subtract the constructed image from the InputImage or add to it
    % Output : - an artificial image of objects + (optional) background and noise 
    %          - a sum or difference of the InputImage and the artificial image (optional)
    % Author : A.M. Krassilchtchikov (2024 Apr) 
    % Example: Im = imUtil.art.injectStamps(XY,PSF,F,'Back',200.);

    arguments
        XY                     = floor(1700.*rand(1000)); % a dummy distribution resemling a LAST subimage
        PSF                    = imUtil.kernel2.gauss;    
        Flux                   = 1;
        
        Args.ImSize            = [];   
        Args.Back              = [];
        Args.PositivePSF logical = false;
        Args.FunEdge           = @imUtil.kernel2.cosbell;
        Args.FunEdgePars       = [4 6];   % should be different for different telescopes! 
        Args.AddNoise logical  = false;
        Args.InputImage        = [];
        Args.Subtract logical  = false;
    end

    ResultingImage = [];
           
    SizeInpIm  = size(Args.InputImage);
    SizeBackIm = size(Args.Back); 
    
    Nim = size(XY,1);          % number of input PSF stamps 
    Nfl = numel(Flux);         % number of input object fluxes (can be 1)
 
    % sanity checks, input conversion
    if iscell(PSF)             % if the PSF size is individual, we'd need a cell array
        Npsf   = size(PSF);
        M      = cellfun(@size, PSF, 'UniformOutput', false);
        MaxM   = max(cellfun(@max, M));
        PSFRad = cellfun(@x, (x(1)-1)/2,M);
    elseif isnumeric(PSF)      % if the PSF size is the same, we can use an array of matrices
        [M,~,Npsf] = size(PSF); 
        MaxM       = M; 
        PSFRad     = (M-1)/2;  % assuming M is odd
    else
        error('Input PSF format is invalid');        
    end
        
    if Npsf > 1 && abs(Nim-Npsf) > 0  
        error('Input data dimensions do not match')
    end
    
    if Nfl == 1  % make a vector of fluxes if only 1 value is given
        Flux = Flux.*ones(1,Nim);
    end
    
    if Npsf == 1 % make an array of PSFs if only one stamp is given
        PSF = repmat(PSF, [1, 1, Nim]);
    end
    
    % output image size     
    if SizeInpIm(1) > 1          % match the size of the input image, if it exists
        ImSize = SizeInpIm;
    elseif SizeBackIm(1) > 1     % match the size of the background image, if it exists
        ImSize = SizeBackIm;
    elseif ~isempty(Args.ImSize) % use the explicit sizes
        ImSize = Args.ImSize; 
    else                     % determine the sizes from the object coordinates and the maximal PSF size
        ImSize(1) = max(XY(:,1)) + (MaxM-1)/2;  
        ImSize(2) = max(XY(:,2)) + (MaxM-1)/2;        
    end        
    
    Image = repmat(0,ImSize);  %#ok<*REPMAT>
    
    % eliminate negative PSF edges (may be not needed when a realistic bkg is provided?)  
    if Args.PositivePSF
        for Isrc = 1:Nim
            if iscell(PSF)
                EdgeFunPars = ceil( Args.FunEdgePars .* M{Isrc}(1) / 15);    % empiric, should somehow depend on M
                SupressedEdges = Args.FunEdge( EdgeFunPars, size(PSF{Isrc}) ) .* PSF{Isrc};
                PSF{Isrc} = SupressedEdges ./ sum(SupressedEdges,'all');     % normalize
            else
                EdgeFunPars = ceil( Args.FunEdgePars .* M / 15);             % empiric, should somehow depend on M
                SupressedEdges = Args.FunEdge( EdgeFunPars, [M M] ) .* PSF(:,:,Isrc);
                PSF(:,:,Isrc) = SupressedEdges ./ sum(SupressedEdges,'all'); % normalize
            end            
        end
    end
            
    % construct a new artificial image      
    for Isrc = 1:Nim
        % determine the region to be filled in the image
        if iscell(PSF) % individual PSF size
            X1 = XY(Isrc,1)-PSFRad{Isrc}; X2 = XY(Isrc,1)+PSFRad{Isrc};
            Y1 = XY(Isrc,2)-PSFRad{Isrc}; Y2 = XY(Isrc,2)+PSFRad{Isrc};
            X11 = 1; Y11 = 1; X21 = 2*PSFRad{Isrc} + 1; Y21 = 2*PSFRad{Isrc} + 1;
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
        if iscell(PSF)
            Image(X1:X2,Y1:Y2) = Image(X1:X2,Y1:Y2) + Flux(Isrc) .* PSF{Isrc}(X11:X21,Y11:Y21);
        else
            Image(X1:X2,Y1:Y2) = Image(X1:X2,Y1:Y2) + Flux(Isrc) .* PSF(X11:X21,Y11:Y21,Isrc); 
        end
    end
       
    % add background
    if ~isempty(Args.Back)
        if numel(Args.Back) < 2 || ( (SizeBackIm(1) == ImSize(1)) &&  (SizeBackIm(2) == ImSize(2)) )
            Image = Image + Args.Back;
        else
            error('The background image size does not match that of the source image'); 
        end
    end
    
    % add noise
    if Args.AddNoise
        normrnd( Image, sqrt(Image), ImSize(1), ImSize(2) ); 
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
