function PSF=suppressEdges(PSF, Args)
    % Multiply the PSF by edge suppressing function (e.g., cosbell).
    %   Useful in order to verify that the PSF is zero padded and
    %   approach zero smoothly.
    %   See also: imUtil.psf.psf_zeroConverge
    % Input  : - A PSF matrix or cube (image index in 3rd dim).
    %          * ...,key,val,...
    %            'Fun' - A 2-D function that will multiply the PSF.
    %                   The function is of the form F(Pars, SizeXY). Its
    %                   output is normalized to unit peak before use, so it
    %                   acts as a [0,1] taper (imUtil.kernel2.* functions
    %                   return sum-normalized kernels by default).
    %                   Default is @imUtil.kernel2.cosbell
    %            'FunPars' - Parameters passed as the first argument to Fun.
    %                   A scalar W is the taper width in pixels measured
    %                   inward from the stamp outer radius R, i.e. Fun is
    %                   called with [R-W, R]. R is the largest on-axis radius
    %                   present in the stamp: R = min(SizeXY - ceil(SizeXY/2))
    %                   (=(N-1)/2 for odd N, N/2 for even N, min over the two
    %                   axes for a non-square stamp), consistent with the
    %                   center used by imUtil.kernel2.*.
    %                   For the default cosbell, W=2 gives 1 at R-2, 0.5 at
    %                   R-1 and 0 at R, i.e. a smooth taper over the two
    %                   outermost pixels that reaches zero at the edge.
    %                   A two-element vector is used as is ([inner, outer]
    %                   radii of the cosbell), independent of the stamp size.
    %                   Default is 2.
    %            'Norm' - A logical indicating if to normalize the
    %                   sum of the PSF to 1.
    %                   Default is true.
    % Output : - A PSF stamp.
    % Author : Eran Ofek (Jul 2023)
    % Example: imUtil.psf.suppressEdges(rand(25,25))
    %          imUtil.psf.suppressEdges(rand(25,25), 'FunPars',[5 8])

    arguments
        PSF
        Args.Fun                     = @imUtil.kernel2.cosbell;
        Args.FunPars                 = 2;
        Args.Norm                    = true;
    end

    Size   = size(PSF);
    SizeXY = [Size(2) Size(1)];
    FunPars = imUtil.psf.suppressEdgesPars(Args.FunPars, SizeXY);

    % peak-normalize: a [0,1] taper, not a sum-normalized kernel, so the
    % PSF flux scale is preserved also when Norm is false
    Fun  = Args.Fun(FunPars, SizeXY);
    Fun  = Fun./max(Fun, [], 'all');
    PSF = PSF .* Fun;
    if Args.Norm
        PSF = PSF./sum(PSF, [1 2]);
    end
end
