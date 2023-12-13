function [InterpImage,SpatPos] = linearizeTrace(Image, Trace, Args)
    % Given an image containing a curved trace, and the trace function, generate a linearized version of the image
    % in which the trace is a vertical or horizontal.
    %     The linearization is done using the interp1 or interp2 functions.
    %     When using the interp1, in each wavelength, the interpolation is
    %     done in the spatial dimension.
    % Input  : - A 2D matrix containing an image.
    %            By default the wavelength dimension is 1 (See 'DimWave'
    %            argument').
    %          - A vector of the spatial position of the trace as a
    %            function of the wavelength position.
    %          * ...,key,val,... 
    %            'DimWave' - Wavelength axis dimension. Default is 1.
    %            'InterpDim' - Number of dimensions over which to
    %                   interpolate: 1|2.
    %                   If 1, then the interpolation is done on the spatial
    %                   dimension, independetly in each wavelength.
    %                   If 2, then the interpolation is done simoultanoulsy
    %                   on both axes.
    %                   Default is 2.
    %            'HalfWidth' - Half width of the spatial dimension of the output
    %                   image. If empty then use floor(0.5.*Nspat - 2),
    %                   where Nspat is the numbre pf pixels in the spatial
    %                   dimension. Default is [].
    %            'Oversampling' - 1/Oversampling of the spatial dimension of the
    %                   original image.
    %                   If 1 will keep the spatial pix size as is.
    %                   If 0.5 will have oversampling by factor of 2...
    %                   Default is 1.
    %            'InterpMethod' - Interpolation method (see interp1/2 for
    %                   options). Default is 'makima'.
    %            'BackToOrigDim' - A logical indicating if to return the
    %                   wavelength dim of the output matrix  to be like the input
    %                   matrix. Default is true.
    % Output : - A 2D matrix in which the trace is an horizontal or
    %            vertical line.
    %          - The spatial position of the linearized trace.
    % Author : Eran Ofek (2023 Dec) 
    % Example: [InterpImage,SpatPos] = imUtil.spec.trace.linearizeTrace(Image, Trace)

    arguments
        Image
        Trace
        Args.DimWave           = 1;
        Args.InterpDim         = 2;  % 1|2
        Args.HalfWidth         = []; % f empty, then use floor of 1/2 the 2D spectra width - 2
        Args.Oversampling      = 1;
        Args.InterpMethod      = 'makima';
        Args.BackToOrigDim logical = true;
    end

    % make the wavelength dim as X-axis as this will expedite the
    % interpolation
    if Args.DimWave==1
        Image = Image.';
    end
    Trace = Trace(:).'; % convert trace to a raw vector
    
    [Nspat, Nwave] = size(Image);
    if Nwave~=numel(Trace)
        error('Number of trace points must corresponds to number of wavelength dimension');
    end
    
    if isempty(Args.HalfWidth)
        Args.HalfWidth = floor(0.5.*Nspat - 2);
    end
    
    Xorig = (1:1:Nspat).';
    % Xnew is a matrix generated from a colum vector and a raw vector:
    Xspat = (-Args.HalfWidth:Args.Oversampling:Args.HalfWidth).';
    SpatPos = Args.HalfWidth + 1;
    Nnew  = numel(Xspat);
    
    Xnew  = Trace + (-Args.HalfWidth:Args.Oversampling:Args.HalfWidth).';
    
    
    % convert all to the same type
    ImageClass = class(Image);
    Xorig      = cast(Xorig, ImageClass);
    Xnew       = cast(Xnew, ImageClass);
    Trace      = cast(Trace, ImageClass);
    
    switch Args.InterpDim
        case 1
            % 1 dimension interpolation along the spatial axis
            InterpImage = zeros(Nnew, Nwave);
            for Iwave=1:1:Nwave
                InterpImage(:,Iwave) = interp1(Xorig, Image(:,Iwave), Xspat+Trace(Iwave), Args.InterpMethod);
            end
            % must do loop
            %InterpImage = interp1(Xorig, Image, Xnew, Args.InterpMethod);
        case 2
            % 2 dim interpolation
            Xwave = (1:1:Nwave);
            Xwave = cast(Xwave, ImageClass);
            InterpImage = interp2(Xwave, Xorig, Image, repmat(Xwave, Nnew,1), Xnew, Args.InterpMethod);
        otherwise
            error('InterpDim must be 1 | 2');
    end
            
    % back to orginal dimensions
    if Args.BackToOrigDim
        if Args.DimWave==1
            InterpImage = InterpImage.';
        end
    end
    
    
end
