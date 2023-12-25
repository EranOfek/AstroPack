function [X1,Y1,MinChi2,Flux0,Dof,H, Result]=psfChi2_RangeIter(Cube, Std, PSF, Args)
    % Fit a PSF for multiple sources in several pertubed positions, and fit
    %       a parabolic surface to find the local minima in the chi^2 surface.
    %       This function should be used iteratively to identify the best-fit
    %       PSF position.
    %   Given a PSF and stamps of stars:
    %   1. Shift the PSF according to DX,DY provided by the user.
    %   2. Calculate the \chi^2 for the DX, DY positions as well as several
    %   other positions (relative to DX,DY) given by GridPointX, GridPointY
    %   and RadiusRange (RadiusRange is just a scaling parameter).
    %   3. Fit a chi^2 surface and find its minimum.
    %   4. If minimum is outside some radius, set it to be within this
    %   radius.
    %   5. Return the estimated best fit position that minimuized the
    %   \chi^2.
    %   The \chi^2 per position is calculated using:
    %   imUtil.psf.psfChi2
    %
    % Input  : - A cube of stamps around sources, in which the stamps index
    %            is in the 3rd dimension.
    %          - A cube, matrix or scalar, of std (error) in stamps.
    %          - A matrix of PSF whose size is like the stamps' size in the
    %            first two dimensions.
    %          * ...,key,val,...
    %            'DX' - A vector whose length is like the number of stamps
    %                   containing the X shift to apply to the PSF prior to the
    %                   fit.
    %                   If empty, then do not shift PSF.
    %                   Default is 0.
    %            'DY' - Like 'DX', but for the Y shift.
    %                   Default is 0.
    %            'MinFlux' - A vector of minimal flux per source. If the
    %                   measured flux of a source is below this value, then the
    %                   measured flux will be replaced with the minimum flux.
    %                   This ensures that the chi^2 will be panelized if
    %                   the flux is below the minum flux.
    %                   If empty, then do nothing.
    %                   Default is [].
    %            'WeightedPSF' - sum(PSF.^2, [1 2])
    %                   If empty, then will recalculate.
    %                   Default is [].
    %            'FitRadius2' - Radius.^2 around the PSF center of pixels
    %                   to use in the fit.
    %                   If empty, then use the entire stamp.
    %                   Default is [].
    %            'VecXrel' - A vector of relative X positions in the stamp.
    %                   If empty, then use: (1:1:Nx) - Xcenter.
    %                   Default is [].
    %            'VecYrel' - Like 'VecXrel', but for the Y positions.
    %                   Default is [].
    %            'SumArgs' - A cell array of arguments to pass to the sum
    %                   function. Default is {'omitnan'}.
    %
    %            'RadiusRange' - A factor that will multiply GridPointsX/Y.
    %                   This is useful as you can keep 'GridPointsX/Y' to
    %                   be constant and just reduce this parameter from
    %                   iteration to iteration.
    %                   Default is 1.
    %            'MaxStep_RadiusRangeUnits' - If the minima of the
    %                   parabolic \chi^2 surface is larger than this value
    %                   (in units of GridPointsX/Y) then will set the
    %                   best minima to be on the maximum radius.
    %                   Default is 1.
    %            'GridPointsX' - Grid of X points around the central point in
    %                   which to calculate the \chi^2.
    %                   Default is cosd((0:60:359))
    %            'GridPointsY' - Like 'GridPointsX', but for the Y
    %                   position.
    %                   Default is sind((0:60:359))
    %            'H' - The design matrix of the parabolic surface.
    %                   If empty, then use:
    %                   H = [ones(Npoint,1), Args.GridPointsX(:), Args.GridPointsY(:), Args.GridPointsX(:).^2, Args.GridPointsY(:).^2];
    %                   Default is [].
    % Output : - X position of the \chi^2 parabolic surface minimum.
    %          - Y position of the \chi^2 parabolic surface minimum.
    %          - \chi^2 value evaluate at the minimum parabolic \chi^2
    %            surface.
    %          - Vector of flux fitted for all sources at the centreal
    %            (DX,DY) position.
    %          - Vector of Dof for each source.
    %          - The design matrix. Can be used as the input for the next
    %            iteration.
    %          - A structure containing the following fields:
    %            .MatChi2 - Matrix of chi^2. The rows corresponds to sources, and
    %                   columns to different positions relative to the initial
    %                   position.
    %            .GridPointsX - Vector of X points around central position
    %                   in which the chi^2 values were calculated for.
    %            .GridPointsY - Like GridPointsX, but for the Y
    %                   coordinates.
    % Author : Eran Ofek (Jun 2023)
    % Example: Cube=imUtil.kernel2.gauss(2.*ones(10,1),[15 15]); Std=randn(15,15,10);
    %          Cube = Cube.*100 + Std;
    %          PSF = imUtil.kernel2.gauss(2,[15 15]);
    %          [X1,Y1, MinChi2,Flux0,Dof,H, Res]=imUtil.psf.psfChi2_RangeIter(Cube, 1, PSF);
    
    
    arguments
        Cube
        Std
        PSF
        Args.DX              = 0;
        Args.DY              = 0;
        Args.MinFlux         = [];
        Args.WeightedPSF     = [];
        Args.FitRadius2      = [];
        Args.VecXrel         = [];
        Args.VecYrel         = [];
        Args.SumArgs cell    = {'omitnan'};
        
        Args.RadiusRange     = 1;
        Args.MaxStep_RadiusRangeUnits = 1;
        Args.GridPointsX     = cosd((0:60:359));
        Args.GridPointsY     = sind((0:60:359));
        Args.H               = [];
    end
    
    
    if isempty(Args.WeightedPSF)
        Args.WeightedPSF = sum(PSF.^2, [1 2]); % for flux estimation
    end
    
    if isempty(Args.VecXrel)
        % assume both VecXrel and VecYrel are empty:
    
        [Ny, Nx, Nim] = size(Cube);
        Xcenter = (Nx+1).*0.5;
        Ycenter = (Ny+1).*0.5;
        Dof     = Nx.*Ny - 3;
    
        Args.VecXrel = (1:1:Nx) - Xcenter;
        Args.VecYrel = (1:1:Ny) - Ycenter;
    end
    
    if numel(Args.DX)==1 && Args.DX==0
        DX = 0;
        DY = 0;
    else
        DX = Args.DX;
        DY = Args.DY;
    end
    
    Args.GridPointsX = [0, Args.GridPointsX(:).'];
    Args.GridPointsY = [0, Args.GridPointsY(:).'];
    
    GridPointsX = Args.GridPointsX.*Args.RadiusRange;
    GridPointsY = Args.GridPointsY.*Args.RadiusRange;
    
    Npoint = numel(Args.GridPointsX);
    
    % chi2 for central point
    [Chi2, Flux0, Dof] = imUtil.psf.psfChi2(Cube, Std, PSF,...
                                           'DX',DX,...
                                           'DY',DY,...
                                           'MinFlux',Args.MinFlux,...
                                           'WeightedPSF',Args.WeightedPSF,...
                                           'FitRadius2',Args.FitRadius2,...
                                           'VecXrel',Args.VecXrel,...
                                           'VecYrel',Args.VecYrel,...
                                           'SumArgs',Args.SumArgs);
                                                               
    Nsrc = numel(Chi2);
    MatChi2 = zeros(Npoint, Nsrc);
    MatChi2(1,:) = Chi2(:).';
    for Igrid=2:1:Npoint
        % chi2 for grid points around the central point
        [Chi2, ~] = imUtil.psf.psfChi2(Cube, Std, PSF,...
                                           'DX',DX + GridPointsX(Igrid),...
                                           'DY',DY + GridPointsY(Igrid),...
                                           'MinFlux',Args.MinFlux,...
                                           'WeightedPSF',Args.WeightedPSF,...
                                           'FitRadius2',Args.FitRadius2,...
                                           'VecXrel',Args.VecXrel,...
                                           'VecYrel',Args.VecYrel,...
                                           'SumArgs',Args.SumArgs);
        MatChi2(Igrid,:) = Chi2(:).';
    end                                                                     
  
    % construct the design matrix for the fit
    if isempty(Args.H)
        % Note that X and Y in H are normlized to 1
        H = [ones(Npoint,1), Args.GridPointsX(:), Args.GridPointsY(:), Args.GridPointsX(:).^2, Args.GridPointsY(:).^2];
    else
        H = Args.H;
    end
    
    % fit chi^2 surface for all sources
    % Z = a + b*(X-X0)^2 + c*(Y-Y0)^2
    % Z = [a +b*x0^2 + c*y0^2] + X*[-2b x0] + Y*[-2c y0] +X^2 b + Y^2 c
        
%     Par = H\MatChi2; 

    % fprintf('MatChi2 condition number: %d',cond(MatChi2));  % deb  

    % The MatChi2 matrix tends to have high condition numbers, and that leads to
    % instabilities, so it is better to use some regularization
    % instead of a simple solution (Tikhonov regularization is applied here): 
%     
    LambdaT = 1e-6;
    Par = (H' * H + LambdaT * eye(size(H, 2))) \ (H' * MatChi2);

    Par(Par==0) = 1e-16;
    
    % Find the parameters 
    %
    P_c   = Par(5,:);
    P_b   = Par(4,:);
    Ymin  = Par(3,:)./(-2.*P_c);
    Xmin  = Par(2,:)./(-2.*P_b);
    MinChi2 = [Par(1,:) - P_b.*Xmin.^2 - P_c.*Ymin.^2].';
    
    % X,Y for next iteration
    Rmin = sqrt(Xmin.^2 + Ymin.^2);
    Xmin = Xmin.*min(Rmin, Args.MaxStep_RadiusRangeUnits);
    Ymin = Ymin.*min(Rmin, Args.MaxStep_RadiusRangeUnits);
    
    X1    = DX(:) + Xmin(:).*Args.RadiusRange;
    Y1    = DY(:) + Ymin(:).*Args.RadiusRange;
    
    Flux0 = squeeze(Flux0);
    
    Result.MatChi2     = MatChi2.';
    Result.GridPointsX = GridPointsX;
    Result.GridPointsY = GridPointsY;
end