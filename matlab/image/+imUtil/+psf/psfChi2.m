function [Chi2, WeightedFlux, Dof, ShiftedPSF] = psfChi2(Cube, Std, PSF, Args)
    % Given a PSF and cube of sources stamps, fit flux and calculate the \chi^2
    %   Optionally shift the PSF by DX,DY
    % Input  : - A cube of stamps around sources, in which the stamps index
    %            is in the 3rd dimension.
    %          - A cube, matrix or scalar, of std (error) in stamps.
    %          - A matrix of PSF which size is like the stamps size in the
    %            first two dimensions.
    %          * ...,key,val,...
    %            'DX' - A vector which length is like the number of stamps
    %                   containing the X shift to apply to the PSF prior to the
    %                   fit.
    %                   If empty, then do not shift PSF.
    %                   Default is [].
    %            'DY' - Like 'DX', but for the Y shift.
    %                   Default is [].
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
    % Output : - Vector of \chi^2 per stamp.
    %          - Vector flux measured for each stamp.
    %          - Degrees of freedom (number of used pixel - 3).
    %          - Cube of shifted PSFs.
    % Author : Eran Ofek (Jun 2023)
    % Example: Cube=randn(15,15,1000); Std=randn(15,15,1000);
    %          PSF = imUtil.kernel2.gauss(2,[15 15]);
    %          [Chi2,Flux,Dof]=imUtil.psf.psfChi2(Cube, Std, PSF);
    
    arguments
        Cube
        Std
        PSF
        Args.DX              = []
        Args.DY              = [];
        Args.MinFlux         = [];
        Args.WeightedPSF     = [];
        Args.FitRadius2      = [];
        Args.VecXrel         = [];
        Args.VecYrel         = [];
        Args.SumArgs cell    = {'omitnan'};
        %Args.FluxMethod   = 'wsumall';
    end
    
    if isempty(Args.WeightedPSF)
        Args.WeightedPSF = sum(PSF.^2, [1 2]); % for flux estimation
    end
    
    if isempty(Args.VecXrel)
        % assume bith VecXrel and VecYrel are empty:
    
        [Ny, Nx, ~] = size(Cube);
        Xcenter = (Nx+1).*0.5;
        Ycenter = (Ny+1).*0.5;
        Dof     = Nx.*Ny - 3;
    
        Args.VecXrel = (1:1:Nx) - Xcenter;
        Args.VecYrel = (1:1:Ny) - Ycenter;
    end
    
    if isempty(Args.DX)
        % assume both DX and DY are empty
        Args.DX = 0;
        Args.DY = 0;
        ShiftedPSF = PSF;
    else
        ShiftedPSF = imUtil.trans.shift_fft(PSF, Args.DX, Args.DY);
    end
        
    WeightedFlux = sum(Cube.*ShiftedPSF, [1 2], Args.SumArgs{:})./Args.WeightedPSF;
    if ~isempty(Args.MinFlux)
        WeightedFlux = max(WeightedFlux, Args.MinFlux);
    end
    
    Resid = Cube - WeightedFlux.*ShiftedPSF;
    
    % FFU: search / remove outliers

    if isempty(Args.FitRadius2)
        % use the entire stamp
        %ResidStd2 = (Resid./Std).^2;
        
        Chi2  = sum( (Resid./Std).^2, [1 2], Args.SumArgs{:}); %, 'omitnan');
        
        Dof      = [];
    else
        MatX     = permute(Args.VecXrel - Args.DX(:),[3 2 1]);
        MatY     = permute(Args.VecYrel - Args.DY(:),[2 3 1]);
        MatR2    = MatX.^2 + MatY.^2;
        Flag     = MatR2<Args.FitRadius2;
        %ResidStd2= (Flag.*Resid./Std).^2;
        
        Chi2  = sum( (Flag.*Resid./Std).^2, [1 2], Args.SumArgs{:}); %, 'omitnan');
        
        Dof      = squeeze(sum(Flag,[1 2]) - 3);
    end
    
    %Chi2  = sum( ResidStd2, [1 2], Args.SumArgs{:}); %, 'omitnan');
    %Chi2  = sum( (Resid./Std).^2, [1 2], 'omitnan');
    Chi2  = squeeze(Chi2);
    
end

    
        