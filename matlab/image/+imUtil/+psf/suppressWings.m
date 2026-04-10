function [PSF, InnerRad] = suppressWings(PSF, Args)
    % Suppress PSF wings by a cosbell with inner radius set dynamically from radial profile.
    % Input  : - PSF stamp.
    %          * ...,key,val,... 
    %            'Fun' - A 2-D function that will multiply the PSF.
    %                   The function is of the form F(Pars, SizeXY)
    %                   Default is @imUtil.kernel2.cosbell
    %            'Threshold' - If [], them will use inner and outer radii
    %                   in FunPars. If given, then a radial profile will be
    %                   calculated and the inner radius will be set by the
    %                   crossing this threshold (in units of PSF peak).
    %                   Default is 1e-4.
    %            'FunPars' - A scalar or two element vector.
    %                   Either [InnerRad, OuterRad], or width.
    %                   Default is 3.
    %            'Norm' - A logical indicating if to normalize the
    %                   sum of the PSF to 1.
    %                   Default is true.
    % Output : - A PSF with suprressed wings.
    %          - Chosen inner radius.
    % Author : Eran Ofek (2026 Apr) 
    % Example: [PSF, InnerRad] = imUtil.psf.suppressWings(PSF, Args)

    arguments
        PSF
        Args.Fun                     = @imUtil.kernel2.cosbell;
        Args.Thrsehold                = 1e-4;
        Args.FunPars                 = 3; % or # from edge
        Args.Norm                    = true;
    end

    Size = size(PSF);
    HalfSize = (min(Size) - 1).*0.5; % assume odd-size PSF!

    if isempty(Args.Threshold)
        % use cosbell with pre define parameters
        if isscalar(Args.FunPars)
            
            Args.FunPars = [HalfSize-Args.FunPars, Args.FunPars];
        end
    else
        % set InnerRadius based on PSF radial profile < Threshold
        [Radius, Mean] = imUtil.psf.mex.radialProfile_mex(PSF);
        InnerRad = ceil(tools.interp.interp1crossVal(Radius, Mean./max(Mean), Args.Thrsehold, false));
        if numel(Args.FunPars)>1
            % Set FunPars to scalar:
            Args.FunPars = Args.FunPars(2) - Args.FunPars(1);
        end
        Args.FunPars = [InnerRad, min(HalfSize, InnerRad+Args.FunPars)];
    end

    InnerRad = Args.FunPars(1);
    Fun  = Args.Fun(Args.FunPars, [Size(2) Size(1)], [], false, PSF);
    PSF = PSF .* Fun;
    if Args.Norm
        PSF = PSF./sum(PSF, [1 2]);
    end
end
