function [FWHM,Nstars,Info] = fwhm_fromMoments(Image, Args)
    % Estimate the FWHM of stars in image by finding stars and measure their 2nd moment.
    %   This function is useful when the system is near focus.
    %   For out of focus images use: imUtil.psf.fwhm_fromACF
    %   See also: imUtil.psf.fwhm_fromBank
    % Input  : - An image in 2D matrix format.
    %          * ...,key,val,... 
    %            'CCDSEC' - CCDSEC [Xmin Xmax Ymin Ymax] of region in which to
    %                   measure FWHM. If empty use entire image. Default is [].
    %            'HalfSize' - Image half size. If 'CCDSEC' is empty, and this
    %                   argument is provided, then run this program on centeral
    %                   image with this half size. Default is [].
    %            'MinSN' - Minimum S/N to use. Default is 30.
    %            'PsfFun' - A function handle to generate PSF or a cube of
    %                   PSFs.
    %                   Default is @imUtil.kernel2.gauss.
    %            'PsfFunPar' - A cell array of parameters to pass to the PsfFun
    %                   function.
    %                   Default is {[0.1;1.5;3]} (i.e., will generate a cuve of
    %                   templates with Gaussian PSF with sigmas of 0.1, 1.5 and
    %                   3 pixels).
    %
    %            'MomRadius' - Radius around position in which to calculate the
    %                       moments. Recomended ~1.7 FWHM. Default is 12.
    %                       Used only by Method='moment2'.
    %            'Method' - Second-moment estimator:
    %                   'moment2' - imUtil.image.moment2 (obsolete, fixed
    %                           Gaussian weight; MinorSig/MajorSig are
    %                           variances [pix^2], FWHM ~0.6 of the PSF FWHM).
    %                   'moments' - imUtil.sources.moments (unweighted
    %                           2nd moment within MaxRadiusM2; MinorSig/MajorSig
    %                           are sigmas [pix]). See issue #1083.
    %                   Default is 'moment2'.
    %            'MaxRadiusM2' - Radius [pix] of the 2nd-moment sum.
    %                   Used only by Method='moments'. Default is 6.
    %            'StampHalfSize' - Half size [pix] of the stamps cut around
    %                   each star. Used only by Method='moments'. Default is 12.
    %            'Annulus' - [Rin Rout] radii [pix] of the local background
    %                   annulus. Used only by Method='moments'.
    %                   Default is [10 12].
    % Output : - Estimated median FWHM [pix].
    %          - Number of stars used.
    %          - Structure with the following info:
    %            .X1 - 1st X moment of all used stars.
    %            .Y1 - 1st Y moment of all used stars.
    %            .X2 - X^2 moment of all used stars.
    %            .Y2 - Y^2 moment of all used stars.
    %            .XY - Y*Y moment of all used stars.
    %            .MinorSig - Median gaussian sigma of PSF minor axis [pix].
    %                   For Method='moment2' this is a variance [pix^2].
    %            .MajorSig - Median gaussian sigma of PSF major axis [pix].
    %                   For Method='moment2' this is a variance [pix^2].
    %            .Angle - PSF major axis angle [rad].
    % Author : Eran Ofek (2024 Nov) 
    % Example: F=imUtil.psf.fwhm_fromMoments(AI.Image);
    %          F=imUtil.psf.fwhm_fromMoments(AI.Image, 'HalfSize',1000, 'Method','moments');

   


    arguments
        Image
        Args.CCDSEC       = [];
        Args.HalfSize     = [];

        Args.MinSN        = 30;
        Args.PsfFun function_handle        = @imUtil.kernel2.gauss;
        Args.PsfFunPar cell                = {[0.1;1.5;4]};
        Args.MomRadius    = 12;

        Args.Method       = 'moment2';  % 'moment2'|'moments'
        Args.MaxRadiusM2  = 6;
        Args.StampHalfSize = 12;
        Args.Annulus      = [10 12];
    end

    if isempty(Image)
        % early return
        FWHM=NaN;
        Nstars=0;
        Info.X1 = NaN;
        Info.Y1 = NaN;
        Info.X2 = NaN;
        Info.Y2 = NaN;
        Info.XY = NaN;
        Info.MinorSig = NaN;
        Info.MajorSig = NaN;
        Info.Angle    = NaN;
        return
    end

    Image = single(Image);

    if ~isempty(Args.CCDSEC)
        Image = Image(Args.CCDSEC(1,3):Args.CCDSEC(1,4), Args.CCDSEC(1,1):Args.CCDSEC(1,2));

    else
        if ~isempty(Args.HalfSize)
            SizeIm   = size(Image);
            CenterIm = floor(SizeIm.*0.5);
            Args.CCDSEC = [CenterIm(2)-Args.HalfSize, CenterIm(2)+Args.HalfSize, CenterIm(1)-Args.HalfSize, CenterIm(1)+Args.HalfSize];    
            Image = Image(Args.CCDSEC(1,3):Args.CCDSEC(1,4), Args.CCDSEC(1,1):Args.CCDSEC(1,2));
        end
    end

    % subtract background
    Back = median(Image,'all','omitnan');
    Var  = tools.math.stat.rstd(Image).^2;

    Result = imUtil.sources.findSources(Image, 'Threshold',Args.MinSN, 'PsfFun',Args.PsfFun, 'PsfFunPar',Args.PsfFunPar, 'BackIm',Back, 'VarIm',Var);

    FlagStars = Result.SN(:,2)>Result.SN(:,1) | Result.SN(:,3)>Result.SN(:,1);

    switch lower(Args.Method)
        case 'moment2'
            [M1,M2] = imUtil.image.moment2(Image, Result.XPEAK(FlagStars), Result.YPEAK(FlagStars), 'MomRadius',Args.MomRadius);
        case 'moments'
            SN = max(Result.SN(FlagStars,:), [], 2);
            [M1,M2] = imUtil.sources.moments(Image - Back, 'X',Result.XPEAK(FlagStars), 'Y',Result.YPEAK(FlagStars), 'SN',SN, ...
                                             'HalfSize',Args.StampHalfSize, 'Annulus',Args.Annulus, 'MaxRadiusM2',Args.MaxRadiusM2);
        otherwise
            error('Unknown Method option: %s', Args.Method);
    end

    SQ  = sqrt((M2.X2 - M2.Y2).^2 + 4.*M2.XY.^2);
    XY2 = M2.X2 + M2.Y2;

    Lambda1 = median(0.5.*(XY2 + SQ));
    Lambda2 = median(0.5.*(XY2 - SQ));

    if strcmpi(Args.Method, 'moments')
        % eigenvalues are variances: convert to sigmas
        Lambda1(~(Lambda1>0)) = NaN;
        Lambda2(~(Lambda2>0)) = NaN;
        Lambda1 = sqrt(Lambda1);
        Lambda2 = sqrt(Lambda2);
    end

    Info.X1 = M1.X;
    Info.Y1 = M1.Y;
    Info.X2 = M2.X2;
    Info.Y2 = M2.Y2;
    Info.XY = M2.XY;

    Nstars = sum(FlagStars);
    Info.MinorSig = min(Lambda1, Lambda2);
    Info.MajorSig = max(Lambda1, Lambda2);
    Info.Angle    = 0.5.*atan2(2.*median(M2.XY), (median(M2.X2) - median(M2.Y2)));
    FWHM     = sqrt(2*log(2)) * (Info.MinorSig + Info.MajorSig);

end
