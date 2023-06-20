function [Result, Surface]=fitSurface(Image, Args)
    % Fit a 2-D surafce to an image in a matrix
    %   Linearly fit a 2-D surface of some polynomial shape to a matrix.
    % Input  : - A 2-D matrix.
    %          * ...,key,val,...
    %            'Fun' - A cell array of functions that will be used to
    %                   define the design matrix, or a char array
    %                   indicating some predefined functionals.
    %                   Default is 'poly1' which will generate:
    %                   {@(x,y)ones(size(x)), @(x,y)x, @(x,y)y, @(x,y)x.*y};
    %            'StepXY' - Step size in X and Y in which the 2D image will
    %                   be sampled, and only the sampled pixels will be fitted.
    %                   This is mainly used for speeding up the function.
    %                   Default is 5.
    %            'Norm' - A logical indicating if to normalize the X and Y
    %                   values prior to the fit.
    %                   Default is true.
    %            'Niter' - Number of sigma clipping iterations.
    %                   Default is 3;
    %            'SigmaClip' - Sigma clipping, in units of the rms.
    %                   Default is 5.
    % Author : Eran Ofek (Jun 2023)
    % Example: [MatX, MatY] = meshgrid( (1:1:1000), (1:1:1000) );
    %          Z = 2+MatX +MatY + MatX.*MatY;
    %          [R,S]=imUtil.background.fitSurface(Z);
    
    arguments
        Image
        Args.Fun            = 'poly1';
        Args.StepXY         = 5;
        Args.Norm logical   = true;
        Args.Niter          = 3;
        Args.SigmaClip      = 5;
        
    end
    
    if iscell(Args.Fun)
        Fun = Args.Fun;
    else
        switch lower(Args.Fun)
            case 'poly1'
                Fun = {@(x,y)ones(size(x)), @(x,y)x, @(x,y)y, @(x,y)x.*y};
            case 'cheby1_2'
                Fun = {@(x,y)ones(size(x)), @(x,y)x, @(x,y)y, @(x,y)2.*x.^2-1, @(x,y)2.*y.^2-1, @(x,y)x.*y};
            otherwise
                error('Unnown Fun name option');
        end
    end
    
    % select pixels in image
    SizeIm = size(Image);
    IndI = (1:Args.StepXY:SizeIm(1));
    IndJ = (1:Args.StepXY:SizeIm(2));
    
    % using this is faster than:
    % SelectedPix = Image(IndI, IndJ);
    Ind = tools.array.sub2ind_fast(SizeIm, IndI(:),IndJ(:).');
    SelectedPix = Image(Ind);
    SelectedPix = SelectedPix(:);
    
    [MatX, MatY] = meshgrid(IndJ, IndI);
    MatX = MatX(:);
    MatY = MatY(:);
    
    if Args.Norm
        MidX = SizeIm(2).*0.5;
        MidY = SizeIm(1).*0.5;
        RangeX = SizeIm(2);
        RangeY = SizeIm(1);
        MatX = (MatX - MidX)./(0.5.*RangeX);
        MatY = (MatY - MidY)./(0.5.*RangeY);
    else
        MidX   = 0;
        MidY   = 0;
        RangeX = 1;
        RangeY = 1;
    end
    Npix = numel(MatX);
    
    % The design matrix
    Nf = numel(Fun);
    H  = zeros(Npix, Nf);
    for If=1:1:Nf
        H(:,If) = Fun{If}(MatX, MatY);
    end
    
    Result.Flag = true(Npix,1);
    for Iiter=1:1:Args.Niter
         
        Par          = H(Result.Flag,:)\SelectedPix(Result.Flag);
        Result.Resid = SelectedPix - H*Par;
        Result.RMS   = std(Result.Resid(Result.Flag));
        
        if Iiter~=Args.Niter
            % not final iteration
            Flag = abs(Result.Resid./Result.RMS)<=Args.SigmaClip;
        end
  
    end 
    
    Result.Fun  = Args.Fun;
    Result.Par  = Par;
    Result.MidX = MidX;
    Result.MidY = MidY;
    Result.RangeX = RangeX;
    Result.RangeY = RangeY;

    if nargout>1
        Surface = H*Par;
        
        IndI = (1:1:SizeIm(1));
        IndJ = (1:1:SizeIm(2));

        % using this is faster than:
        % SelectedPix = Image(IndI, IndJ);
       
        [MatX, MatY] = meshgrid(IndJ, IndI);
        MatX = MatX(:);
        MatY = MatY(:);
        
        if Args.Norm
            MidX = SizeIm(2).*0.5;
            MidY = SizeIm(1).*0.5;
            RangeX = SizeIm(2);
            RangeY = SizeIm(1);
            MatX = (MatX - MidX)./(0.5.*RangeX);
            MatY = (MatY - MidY)./(0.5.*RangeY);
        else
            MidX   = 0;
            MidY   = 0;
            RangeX = 1;
            RangeY = 1;
        end
        Npix = numel(MatX);

        H  = zeros(Npix, Nf);
        for If=1:1:Nf
            H(:,If) = Fun{If}(MatX, MatY);
        end
        Surface = H*Par;
        
        Surface = reshape(Surface, numel(IndI), numel(IndJ));
    end
end
