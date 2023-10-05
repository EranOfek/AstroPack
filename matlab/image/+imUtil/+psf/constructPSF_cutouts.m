function [Mean, Var, Nim, FlagSelected] = constructPSF_cutouts(Image, XY, Args)
    % Given a background-subtracted image and PSF star positions, construct a mean PSF stamp from cutouts
    % Input  : - A 2-D image, or a cube of cutouts around sources.
    %            If a cube then the image index must be in the 3rd
    %            dimesniosn.
    %          - A two column matrix of [X, Y] sources positions.
    %            If first input is a cube, then these are the positions in
    %            the cube. If empty, then set position to (stamp size -1)/2
    %            Default is [].
    %          * ...,key,val,...
    %            'M1' - Optional structure containing the result of the
    %                   moment2 function. If empty, then will recalculate
    %                   the 1st moment. Default is [].
    %            'Norm' - Vector of normalizations per cutrouts.
    %                   These are the flux normalization one has to
    %                   multiply each cutout, before summation.
    %                   If empty, then attempt to find the flux
    %                   normalization (see 'FluxRadius').
    %                   Default is [].
    %            'FluxRadius' - If 'Norm' is empty, then this is the radius
    %                   around the shifted (centered PSF) cube in which to
    %                   measure the stars flux, and this flux will be used
    %                   as the flux normalization.
    %                   Default is 4.
    %                   See additional info: https://github.com/EranOfek/AstroPack/issues/259
    %            'Back' - A vector of background to subtract from each
    %                   source stamp. If empty, don't subtract background.
    %                   Default is [].
    %            'SubAnnulusBack' - Logical indicating if to subtract
    %                   annulus background. This operation will be done
    %                   after the Back subtraction.
    %                   Default is false.
    %            'backgroundCubeArgs' - A cell array of arguments to pass
    %                   to imUtil.background.backgroundCube
    %                   Default is {}.
    %            'SumMethod' - One of the following summation of PSFs:
    %                    'sigclip' - Use imUtil.image.mean_sigclip.
    %                    'mean' - Mean of PSFs.
    %                    'median' - Median of PSFs.
    %                    Default is 'sigclip'.
    %            'mean_sigclipArgs' - A cell array of arguments to pass to  
    %                   imUtil.image.mean_sigclip if SumMethod='sigclip'.
    %                   Default is {}.
    %            'PostNormBySum' - A logical indicating if post
    %                   normalization is by sum (true), or by peak (false).
    %            'PostNorm' - Post normalization value. If [], do not
    %                   perform post normalization. Default is 1.
    %
    %            'MedianCubeSumRange' - The code calculates the sum of each
    %                   stamp. If the median of sum of stamps is not within the
    %                   range specified by this argument, than the function will
    %                   fail.
    %                   Default is [0.95 1.05].
    %            'CubeSumRange' - Stamps which their flux-nrmalized sum is not
    %                   in this range are excluded from the summation.
    %                   This may be useful in order to remove stamps which
    %                   contains more than one source.
    %                   Default is [0.8 1.2].
    %            'SmoothWings' - A logical indicating if to smooth PSF wings
    %                   using imUtil.psf.psf_zeroConvergeArgs.
    %                   Default is true.
    %            'psf_zeroConvergeArgs' - A cell array of additional
    %                   arguments to pass to imUtil.psf.psf_zeroConvergeArgs.
    %                   Default is {}.
    %
    %            'mexCutout' - use imUtil.cut.mex.mex_cutout.m (true) or
    %                   imUtil.cut.find_within_radius_mat (false).
    %                   Default is true.
    %            'Circle' - If true, then will set all points outside the radius to NaN.
    %                   Default is false.
    %            'MomRadius' - radius for 2nd moment calculations.
    %                   Default is 8.
    %            'Annulus' - [Inner, Outer] annulus raddi for background.
    %                   Default is [10 12].
    %            'ShiftMethod' - Options are:
    %                   'lanczos' - Lanczos interpolation.
    %                   'fft' - Sinc interpolation.
    %            'A' - Order of Lanczos interpolation.
    %                   Default is 2.
    %            'IsCircFilt' - Is circular filtering.
    %                   Relevant for shift_lanczos.
    %                   Default is true.
    %            'PadVal' - Padding value for interpolation.
    %                   Default is 0.
    % Output : - Mean cutout (PSF).
    %          - Variance of all cutouts.
    % Author : Eran Ofek (Dec 2021)
    % Example: Cube = imUtil.kernel2.gauss(1.5.*ones(100,1));
    %          Cube = poissrnd(Cube.*1e5) + randn(size(Cube));
    %          [Mean, Var, Nim] = imUtil.psf.constructPSF_cutouts(Cube, [], 'Norm',1)
    
    arguments
        Image                             % 2D image or cube of cutouts
        XY                         = [];  % XY positions of sources in image
        
        Args.M1                    = []; % override the first moment calculation
        Args.Norm                  = [];  % vector of normalization per cutout
        Args.FluxRadius            = 4; % if norm is not given.
        Args.Back                  = [];  % Back to subtract. If [] don't subtract.
        Args.SubAnnulusBack logical= false;  % subtract annulus background
        Args.backgroundCubeArgs cell= {};
        Args.SumMethod             = 'sigclip';
        Args.mean_sigclipArgs cell = {};
        Args.PostNormBySum logical = true;
        Args.PostNorm              = 1;
        
        Args.MedianCubeSumRange    = [0.9 1.1];
        Args.CubeSumRange          = [0.8 1.2];
        Args.SmoothWings logical   = true;
        Args.psf_zeroConvergeArgs  = {};
        
        Args.mexCutout logical     = true;
        Args.Circle logical        = false;
        Args.ReCenter logical      = true;    % call moment2
        Args.MomRadius             = 8;
        Args.Annulus               = [10 12];        
        
        Args.ShiftMethod           = 'fft';   % 'lanczos' | 'fft'
        Args.A                     = 2;
        Args.IsCircFilt logical    = true;
        Args.PadVal                = 0;
    end
            
%     if isempty(Args.Norm)
%         size(XY)
%         Args.Norm
%         error('Norm argument must be provided');
%     end
    
    MaxRadius = Args.MomRadius;
    
    if isempty(XY)
        if ndims(Image)==3
            % Image is a cube
            [SizeY, SizeX, Nim] = size(Image);
            
            X = ones(Nim,1).*(SizeX.*0.5 + 0.5);
            Y = ones(Nim,1).*(SizeY.*0.5 + 0.5);
        else
            error('if input is an image, XY must be provided');
        end
    else
        X = XY(:,1);
        Y = XY(:,2);
    end
    
    if ndims(Image)==2
        [Cube, RoundX, RoundY, X, Y] = imUtil.cut.image2cutouts(Image, X, Y, MaxRadius, 'mexCutout',Args.mexCutout, 'Circle',Args.Circle);
    else
        Cube = Image;
        RoudnX = round(X);
        RoundY = round(Y);
    end

    Dim = 3;
    Nim = size(Cube,3);
    
    if ~isempty(Args.Back)
        Cube = Cube - reshape(Args.Back(:), 1, 1, Nim);
    end
    
    % add subtract annulus background
    if Args.SubAnnulusBack
        [Back] = imUtil.sources.backgroundCube(Cube, Args.backgroundCubeArgs{:});
        Cube   = Cube - reshape(Back(:), 1, 1, Nim);
    end
        
    SizeCube = size(Cube);
    Ncube    = SizeCube(3);
    Xcen     = SizeCube(2).*0.5 + 0.5;
    Ycen     = SizeCube(1).*0.5 + 0.5;

    
    if Args.ReCenter
        
        %M1 = imUtil.image.moment2(Cube, X, Y, 'MomRadius',Args.MomRadius);
        if isempty(Args.M1)
            M1 = imUtil.image.moment2(Cube, Xcen, Ycen, 'MomRadius',Args.MomRadius, 'Annulus',Args.Annulus);
        else
            M1 = Args.M1;
        end
        X  = M1.X;
        Y  = M1.Y;
        
        ShiftXY  = [Xcen - X, Ycen - Y];
    else
        
        ShiftXY  = [RoundX - X, RoundY - Y];
    end
    
    
    switch lower(Args.ShiftMethod)
        case 'lanczos'
            [ShiftedCube] = imUtil.trans.shift_lanczos(Cube, ShiftXY, Args.A, Args.IsCircFilt, Args.PadVal);
        case 'fft'            
            [ShiftedCube] = imUtil.trans.shift_fft(Cube, ShiftXY(:,1), ShiftXY(:,2));
        otherwise
            error('Unknown ShiftMethod option');
    end
    
    % cutouts normalization
    if numel(Args.Norm)==1
        Args.Norm = repmat(Args.Norm,1,Ncube);
    end
    
    if isempty(Args.Norm)
        % find the flux normalization
        Xvec = (1:1:SizeCube(2)) - Xcen;
        Yvec = (1:1:SizeCube(1)) - Ycen;
        MatR2 = Xvec.^2 + Yvec(:).^2;
        
        Flag  = MatR2<Args.FluxRadius.^2;
        Args.Norm = 1./squeeze(sum(Cube.*Flag, [1 2]));
        
    end
    
    Args.Norm = reshape(Args.Norm, 1,1, Ncube);  % put Norm in 3rd dim
    ShiftedCube = ShiftedCube.*Args.Norm;
    
        
    % remove sources with non unity flux (maybe neighboors?)
    CubeSum = squeeze(sum(ShiftedCube,[1 2]));
    
    % remove sources with MedianCubeSumRange different than ~1
    FlagSelected = CubeSum>Args.CubeSumRange(1) & CubeSum<Args.CubeSumRange(2);
    ShiftedCube  = ShiftedCube(:,:,FlagSelected);
    Nim = sum(FlagSelected);
    
    % verify that median(CubeSum) is around 1
    % otherwise there is excess flux somewhere
    MedCubeSum = median(CubeSum);
    if Nim==0 
        % || MedCubeSum<Args.MedianCubeSumRange(1) || MedCubeSum>Args.MedianCubeSumRange(2)
        % warning('Median of the flux normalized cube sum is not 1');
        warning('No candidate PSF stars found');
        FlagSelected = [];
        Mean         = [];
        Var          = [];
        FlagGood     = [];
        GoodCounter  = 0;
        Nim          = 0;
    else
        % remove sources with MedianCubeSumRange different than ~1
        %FlagSelected = CubeSum>Args.CubeSumRange(1) & CubeSum<Args.CubeSumRange(2);
        %ShiftedCube  = ShiftedCube(:,:,FlagSelected);
        
        % cutout summation
        switch lower(Args.SumMethod)
            case 'sigclip'
                [Mean,Var,FlagGood,GoodCounter] = imUtil.image.mean_sigclip(ShiftedCube, Dim, Args.mean_sigclipArgs{:});
            case 'mean'
                Mean = mean(ShiftedCube, Dim, 'omitnan');
                Var  = var(ShiftedCube,1, Dim, 'omitnan');
            case 'median'
                Mean = median(ShiftedCube, Dim, 'omitnan');
                Var  = var(ShiftedCube,1, Dim, 'omitnan');
            otherwise
                error('Unknown SumMethod option');
        end
    
        % smooth wings...
        if Args.SmoothWings
            Mean = imUtil.psf.psf_zeroConverge(Mean, Args.psf_zeroConvergeArgs{:});
        end

        if ~isempty(Args.PostNorm)
            if Args.PostNormBySum
                Norm = 1./(Args.PostNorm.*sum(Mean, 'all'));
            else
                % norm by peak
                Norm = 1./(Args.PostNorm.*max(Mean,[],'all'));
            end

            Mean = Mean .* Norm;
            Var  = Var  .* Norm;
        end
    end
end