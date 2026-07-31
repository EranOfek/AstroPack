function [PSF,InnerRadius] = wingsFix(PSF, Args)
    % Fix PSF wings using various methods
    % Input  : - PSF stamp
    %          * ...,key,val,...
    %            'WingsMethod' - 'analytic'|'cosbell'|'empirical'.
    %                   Default is 'analytic' (using:
    %                   imUtil.psf.addWings2PSF)
    %            'SuppressThreshold' - Threshold passed to suppressWings.
    %                   Default is 1e-2.
    %            'WingsPowerLaw' - Power law of wings extension.
    %                   Default is 2.
    %            'SuppressFun' - Window function used by suppressWings to
    %                   taper the master PSF. Default is @imUtil.kernel2.cosbell.
    %            'SuppressFunPars' - Parameters for SuppressFun (e.g. the
    %                   number of pixels from the edge). Default is 3.
    %            'ProfileRadius' - Radii of a precomputed empirical wing
    %                   profile (see imUtil.psf.buildEmpiricalWing), used
    %                   only when WingsMethod='empirical'. Default is [].
    %            'ProfileValue' - Values of the precomputed empirical wing
    %                   profile, paired with ProfileRadius. Default is [].
    %            'ProfileSuccess' - Whether the caller (imUtil.psf.buildPSF)
    %                   was able to build a usable empirical profile (e.g.
    %                   enough bright/near-saturated stars were available).
    %                   When WingsMethod='empirical' and this is false, the
    %                   'cosbell' taper is used instead for this image.
    %                   Default is false.
    % Output : - PSF with fixed wings.
    % Author : Eran Ofek (2026 Jun)
    % Example: [PSF,InnerRadius] = imUtil.psf.wingsFix(PSF);

    arguments
        PSF
        Args.WingsMethod                 = 'analytic';
        Args.SuppressThreshold           = 1e-2;
        Args.WingsPowerLaw               = 2.0;
        Args.SuppressFun                 = @imUtil.kernel2.cosbell;
        Args.SuppressFunPars             = 3; % or # from edge
        Args.ExtendedSize                = [];
        Args.ProfileRadius               = [];
        Args.ProfileValue                = [];
        Args.ProfileSuccess              = false;
    end


    switch Args.WingsMethod
        case 'analytic'
            InnerRadius = imUtil.psf.radiusAtFraction(PSF, Args.SuppressThreshold);
            OuterRadius  = min(InnerRadius + 3, (size(PSF,1)-1).*0.5);
            PSF = imUtil.psf.addWings2PSF(PSF, Args.WingsPowerLaw, InnerRadius, OuterRadius);
        case  'cosbell'
            [PSF, InnerRadius] = imUtil.psf.suppressWings(PSF, 'Fun',Args.SuppressFun,...
                                                            'Threshold',Args.SuppressThreshold,...
                                                            'FunPars',Args.SuppressFunPars,...
                                                            'Norm',true,...
                                                            'ExtendedSize',Args.ExtendedSize,...
                                                            'Alpha',Args.WingsPowerLaw);
        case 'empirical'
            InnerRadius = imUtil.psf.radiusAtFraction(PSF, Args.SuppressThreshold);
            if Args.ProfileSuccess && ~isempty(Args.ProfileRadius) && ~isempty(Args.ProfileValue)
                OuterRadius = min(InnerRadius + 3, (size(PSF,1)-1).*0.5);
                PSF = imUtil.psf.addEmpiricalWings2PSF(PSF, Args.ProfileRadius, Args.ProfileValue, ...
                                                         'R1',InnerRadius, 'R2',OuterRadius);
            else
                % Not enough bright/near-saturated stars to calibrate an
                % empirical wing for this image -- fall back to cosbell.
                [PSF, InnerRadius] = imUtil.psf.suppressWings(PSF, 'Fun',Args.SuppressFun,...
                                                                'Threshold',Args.SuppressThreshold,...
                                                                'FunPars',Args.SuppressFunPars,...
                                                                'Norm',true,...
                                                                'ExtendedSize',Args.ExtendedSize,...
                                                                'Alpha',Args.WingsPowerLaw);
            end
        otherwise
            error('Unknown WingsMethod option');
    end


end
