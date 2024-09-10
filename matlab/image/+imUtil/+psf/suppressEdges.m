function PSF=suppressEdges(PSF, Args)
    % Multiply the PSF by edge suppressing function (e.g., cosbell).
    %   Useful in order to verify that the PSF is zero padded and
    %   approach zero smoothly.
    %   See also: imUtil.psf.psf_zeroConverge
    % Input  : - A PSF matrix.
    %          * ...,key,val,...
    %            'Fun' - A 2-D function that will multiply the PSF.
    %                   The function is of the form F(Pars, SizeXY)
    %                   Default is @imUtil.kernel2.cosbell
    %            'FunPars' - Vector of parameters that will be
    %                   passed as the first argument to the Fun.
    %                   Default is 5 7
    %            'Norm' - A logical indicating if to normalize the
    %                   sum of the PSF to 1.
    %                   Default is true.
    % Output : - A PSF stamp.
    % Author : Eran Ofek (Jul 2023)
    % Example: imUtil.psf.suppressEdges(rand(25,25))

    arguments
        PSF
        Args.Fun                     = @imUtil.kernel2.cosbell;
        Args.FunPars                 = [5 7];
        Args.Norm logical            = true;
    end

    Size = size(PSF);
    Fun  = Args.Fun(Args.FunPars, [Size(2) Size(1)]);
    PSF = PSF .* Fun;
    if Args.Norm
        PSF = PSF./sum(PSF, [1 2]);
    end
end