function Result = sparse2full(SparseImage, VecX, VecY, SizeIJ, Args)
    % Resize/extend/magnify an image sampled at some specific very sparse points into a full image.
    % Input  : - A sparse image
    %          - X points in which the sparse image is sampled from the
    %            full image.
    %          - Y points in which the sparse image is sampled from the
    %            full image.
    %          - [I,J] size of the full image.
    %          * ...,key,val,...
    %            'Method' - One of the following methods:
    %                   'interp' - use interp2
    %            'ExtrapFill' - Extrpolate to image edges before
    %                   interpolation. Default is true.
    %            'InterpMethod' - Interpolation method.
    %                   Default is 'linear'.
    %            'Smooth' - Perform smoothing after filling.
    %                   Default is false.
    %            'GaussSigma' - Sigma of gaussian smoothing kernel.
    %                   Default is 10.
    %            'FiltSizeFactor' - Size of filter in units of GaussSigma.
    %                   Default is 6.
    % Output : - Filled image.
    % Author : Eran Ofek (Jun 2021)
    % Example: Result = imUtil.image.sparse2full(rand(2,4), [11 21 31 41], [11 21], [30 50])
    %          R=rand(12,12);
    %          Result = imUtil.image.sparse2full(R, (128:128:1600),(128:128:1600), [1600 1600]);
    
    arguments
        SparseImage
        VecX
        VecY
        SizeIJ
        Args.Method char               = 'interp';   %
        % interp Method parameters
        Args.ExtrapFill(1,1) logical   = true;
        Args.InterpMethod              = 'linear';
        
        % post interpolation
        Args.Smooth(1,1) logical       = false;
        Args.GaussSigma                = 10; % if empty do nothing
        Args.FiltSizeFactor            = 6;
    end
   
    switch lower(Args.Method)
        case 'interp'
            % use linear interpolation with extrapolation
            
            if Args.ExtrapFill
                SparseImage = [SparseImage(:,1), SparseImage, SparseImage(:,end)];
                SparseImage = [SparseImage(1,:); SparseImage; SparseImage(end,:)];
                VecX        = [1; VecX(:); SizeIJ(2)];
                VecY        = [1; VecY(:); SizeIJ(1)];
            end
            
            % no need to define full matrices
            %[MatX,MatY] = meshgrid((1:1:SizeIJ(2)), (1:1:SizeIJ(1)));
            %Result = interp2(VecX, VecY, SparseImage, MatX, MatY, Args.InterpMethod);
            
            % This is faster
            VecXX = (1:1:SizeIJ(2));
            VecYY = (1:1:SizeIJ(1)).';
            Result = interp2(VecX, VecY, SparseImage, VecXX, VecYY, Args.InterpMethod);
            
        otherwise
            error('Unknown Method option');
    end
    
    
    % post interpolation / smothing
    if Args.Smooth
        KernelSize = (Args.GaussSigma.*Args.FiltSizeFactor + 1).*ones(1,2);
        KernelSize = min([SizeIJ; KernelSize]);
        Kernel     =  imUtil.kernel2.gauss(Args.GaussSigma, fliplr(KernelSize));
        Result     = imUtil.filter.conv2_fast(Result, Kernel);
    end
    
end