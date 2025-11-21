function [SubtractedImage, SourceImage] = subtractSources(Image, PSF, Args)
    % Subtract a list of sources with a PSF from an image
    %     Given an image, a PSF (or cube of PSFs), and X, Y coordinatess and flux
    %     of sources, subtract the sources from the image.
    %       This function is used by the multi-iteration PSF-fitting code.
    % Input  : - An image (2D matrix).
    %          - A PSF stamp, or PSF cube (PSF index in the third dim.)
    %          * ...,key,val,... 
    %            'X' - X coordinate of sources, or alternatively roundex X
    %                   coordinates. In the latter, the DX argument must be
    %                   not a zero scalar.
    %                   This argument must be provided.
    %                   Default is [].
    %            'Y' - Like 'X', but for the Y coordinate.
    %                   Default is [].
    %            'Flux' - Like 'X', but for the Flux.
    %                   Default is [].
    %            'DX' - If not zero, then this is DX to add to X in order
    %                   to get the sources coordinates.
    %                   Default is 0.
    %            'DY' - Like 'DX', but for the Y coordinates.
    %                   Default is 0.
    %            'ShiftMethod' - PSF shift method.
    %                   Default is 'fft'.
    %            'SupressPSF' - A function handle that will be used to
    %                   supress the PSF edges. If empty, then skip.
    %                   Default is @imUtil.kernel2.cosbell
    %            'SupressPSFArgs' - Arguments to pass to the SupressPSF
    %                   function. Default is [5 8].
    %            'NormPSF' - A logical indicating if to normalize the PSF
    %                   to 1. Default is true.
    %            See additional arguments for ds9 region files in the code.
    %            
    % Output : - The original image after subtraction of the sources.
    %          - The image of the subtracted sources (i.e., model) with
    %            zero background.
    % Author : Alexander K. + Eran Ofek (2025 Oct) 
    % Example: [SubIm, SrcIm] = imUtil.sources.subtractSources(Image)

    arguments
        Image
        PSF                    = [];
        Args.IsShiftedPSF      = false;
        Args.X                 = [];   % or AstroCatalog column (all from header)
        Args.Y                 = [];   % "
        Args.Flux              = [];   % "
        Args.DX                = 0;
        Args.DY                = 0;
        

        Args.ShiftMethod       = 'fft';

        Args.SupressPSF        = @imUtil.kernel2.cosbell;  % if empty, skip
        Args.SupressPSFArgs    = [5 8];
        Args.NormPSF           = true;

        Args.RegionFile        = []; %If empty, do not write ds9 region file
        Args.RegionColor       = 'green';
        Args.RegionMarker      = 'o';
        Args.RegionSize        = 5;
        Args.RegionWidth       = 1;
    end

    % This commented block will be useful if we want to replicate this
    % function in imProc.sources.subtractSources:
    %
    % if ~isnumeric(Args.X) && ~isnumeric(Args.Y) && ~isnumeric(Args.Flux)
    %     % read X,Y,Flux coordainates from catalog. Ignore DX/DY
    %     XYF = AI(Iobj).CatData.getCol({Args.X, Args.Y, Args.Flux});
    %     RoundX = round(XYF(:,1));
    %     RoundY = round(XYF(:,2));
    %     DX     = XYF(:,1) - RoundX;
    %     DY     = XYF(:,2) - RoundY;
    %     Flux   = XYF(:,3);
    % else

    if ~Args.IsShiftedPSF
        % User supplied numeric DX,DY,Flux,X,Y 
        if isscalr(Args.DX) && Args.DX==0 && isscalar(Args.DY) && Args.DY==0
            % X and Y contains non-rounded coordinates
            % Shift PSF to such the stamp center will be in the rounded
            % position
            RoundX = round(X);
            RoundY = round(Y);
            DX     = Args.X - RoundX;
            DY     = Args.Y - RoundY;
        else
            % Assume X and Y contains rounded coordinates
            % and DX, DY shifts relative to these rounded coordinates
            RoundX = Args.X;
            RoundY = Args.Y;
            DX     = Args.DX;
            DY     = Args.DY;
        end
        Flux = Args.Flux;
    
    
        % shift PSF cube to rounded coordinates
        switch Args.ShiftMethod
            case 'fft'
                ShiftedPSF = imUtil.trans.shift_fft(PSF, DX, DY);
    
            case 'interp'
                % call imUtil.trans.shift_interp
                % NEED TO CREATE THIS FUNCTION BASED ON YOUR CODED in
                % mextractor
            otherwise
                error('Unknown ShiftMethod option');
        end
    end
    % supress PSF edges & Normalize
    if ~isempty(Args.SupressPSF)
        ShiftedPSF = imUtil.psf.suppressEdges(ShiftedPSF, 'Fun',Args.SupressPSF, 'FunPars', Args.SupressPSFArgs, 'Norm', Args.NormPSF);
    end



    % subtract the newly found and measured sources:
    % 1. construct a source image
    % 2. subtract the source image from the current image
    [CubePSF, XY]                = imUtil.art.createSourceCube(ShiftedPSF, [RoundY RoundX], Flux, ...
                                                                        'Recenter', false, 'PositivePSF',true);
    SizeImage = size(AI(Iobj).ImageData.Image);
    % Can we avoid the permute? may be expensive?
    SourceImage             = imUtil.art.addSources(repmat(0,SizeImage), permute(CubePSF,[2,1,3]),XY,...
                                                                        'Oversample',[], 'Subtract',false);           
    SubtractedImage         = Image - SourceImage;  
    
    
    
    % NOT GOOD
    % use instead: but only in the imProc version:
    %imProc.mask.replaceMaskedPixVal(AI,  'Saturated', NaN);
            
    % optionaly set pixels with Mask > 0 to the background values (in practice this does not influence the result?)
    %if Args.RemoveMasked
    %    Ind = AI.Mask > 0;  % removing everything with Mask>0 is problematic!                
    %    Subtracted(Ind) = AI.Back(Ind);
    %end
    % optionaly set pixels with reconstructed source PSFs to the background values 
    % NOT CLEAR?
    %    
    %if Args.RemovePSFCore
    %    Ind = SourceImage(:,:,Iiter) > 0;
    %    Subtracted(Ind) = AI.Back(Ind); % need to be tested and improved to operate only on a 3x3 (5x5?) pixel core
    %end   

    
                        
    % write region files with extracted objects 
    if ~isempty(Args.RegionFile)
        % DS9_new - is it better than ds9??
        ds9.regionWrite([RoudnX+DX, RoundY+DY],...
                        'FileName',Args.RegionFile,...
                        'Color',Args.RegionColor,...
                        'Marker',Args.RegionMarker,...
                        'Size',Args.RegionSize,...
                        'Width',Args.RegionWidth,...
                        'Precision','%.2f',...
                        'PrintIndividualProp',0);
    end            

end
