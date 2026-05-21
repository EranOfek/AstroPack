function [FWHM, FWHE] = fwhmOfStamp(Cube, Args)
    % Given a cube of image stamps containing PSFs, directly calculate the FWHM and FWHE of PSFs.
    %       Note that for a small FWHM, these estimators are biased.
    %       For a Gaussian PSF, the mean of the two is a resnoable
    %       estimator.
    % Input  : - A cube of PSF assumed to be background subtracted.
    %          * ...,key,val,... 
    %            'ReCenter' - Recenter the PSF using 1st moment.
    %                   Default is false.
    %            'Step' - radial step size. Default is 1.
    %            'UseMaxRadius' - For FWHM estimate, normalize PSF maximum from the value of
    %                   the 1st radial bin (true), or max of stamp (false).
    %                   Default is true.
    % Output : - FWHM estimator.
    %          - FWHE estimator.
    % Author : Eran Ofek (2026 May) 
    % Example: [FWHM,FWHE]=imUtil.psf.fwhmOfStamp(G);
    %
    %{
            s=1+rand(100,1);
            G=imUtil.kernel2.gauss(s,[25 25]);
            [FWHM,FWHE]=imUtil.psf.fwhmOfStamp(G);
            plot(s.*2.35,FWHM,'o');
            hold on
            plot(s.*2.35,FWHE,'o') 
            plot([2 5],[2 5],'-')  
    %}


    arguments
        Cube
        Args.ReCenter           = false;
        Args.Step               = 1;
        Args.UseMaxRadius       = true;
    end

    [SizeY, SizeX, Npsf] = size(Cube);
    Xc  = (SizeX+1).*0.5;
    Yc  = (SizeY+1).*0.5;
    MaxRad = floor((min(SizeX, SizeY)-1).*0.5);
        
    if Args.ReCenter
        M1 = imUtil.sources.moments(Cube, 'Cut2D',false);
        Xc = M1.X;
        Yc = M1.Y;            
    end

    [Radius,Mean,Sum] = imUtil.psf.mex.radialProfile_mex(Cube, Xc, Yc, MaxRad, Args.Step);
    CumSum = cumsum(Sum, 1, 'omitnan');
    if Args.UseMaxRadius
        Max = Mean(1,:);
    else
        Max    = max(Cube,[], [1 2], 'omitnan');
        Max    = reshape(Max,1,[]);
    end
    Mean   = Mean./Max;
    CumSum = CumSum./CumSum(end,:);

    FWHM = zeros(Npsf,1);
    FWHE = zeros(Npsf,1);

    
    for Ipsf=1:1:Npsf
        FWHM(Ipsf)=2.*tools.interp.interp1crossVal(Radius, Mean(:,Ipsf), 0.5, false);
        FWHE(Ipsf)=2.*tools.interp.interp1crossVal(Radius, CumSum(:,Ipsf), 0.5, true);
    end


end
