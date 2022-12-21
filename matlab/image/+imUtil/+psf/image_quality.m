function [Result, Fit]=image_quality(Image, Args)
    % Measure the image quality as a function of position in image.
    % Input  : - An image in matrix format
    %          * ...,key,val,...
    %            'BlockSize' - Block size of sub images division.
    %                       Default is [1024 1024].
    %            'MinSN' - Minimum S/N to use. Default is 50.
    %            'SigmaVec'   - Vector of the Gaussian bank sigmas.
    %                           This should not include a sharp object (such a
    %                           sharp object is added by the code).
    %                           Default is logspace(0,2,5).
    %            'MinStars'   - Minimum numbre of stars needed to estimate
    %                           FWHM. Default is 5.
    %            'PixScale'   - Pixel scale ["/pix]. Default is 1.
    %            'Method'     - Method: 
    %                           'bisec' - Bi-sector search
    %                           'MaxNdet' - Choose the filter with the max
    %                                   number of detections.
    %                           'MaxNdetInterp' - Same as 'MaxNdet', but with
    %                                   interpolation over number of detections.
    %                           Default is 'bisec'.
    %            'MaxIter'    - Numbre of iterations for the 'bisec' method.
    %                           Default is 5.
    %            'Plot'       - PLot image quality map. Default is false.
    % Output : - A structure with the following fields:
    %            .FWHM - Matrix of FWHM per position.
    %            .Nstars - Matrix of number of used stars per position.
    %            .ListEdges - Edges per position.
    %            .X - Matrix of X centers per position.
    %            .Y - Matrix of Y centers per position.
    %          - Paraboloid surface fit.
    %            The coordinate system is relative to the image center.
    %            ExtramXc, ExtramYc provides the extremum position relative
    %            to the image center.
    % Author : Eran Ofek (Jan 2022)
    % Example: [Result,Fit]=imUtil.psf.image_quality(Image)


    arguments
        Image
        Args.BlockSize      = [1024 1024];
        Args.SigmaVec       = logspace(-0.2,0.8,5);
        Args.MinSN          = 50;
        Args.MinStars       = 5;
        Args.PixScale       = 1;
        Args.Method         = 'bisec';
        Args.MaxIter        = 5;
        
        Args.Clean logical  = true;
        Args.HalfRangeGood  = 1.5;
        Args.Plot logical   = false;
    end

    Image = single(Image);

    [Sub,ListEdge,ListCenter,~,~,Nxy] = imUtil.cut.partition_subimage(Image,[],'SubSizeXY',Args.BlockSize, 'Output','struct');
    Nsub = numel(Sub);
    FWHM   = nan(size(Sub));
    Nstars = nan(size(Sub));
    for Isub=1:1:Nsub
        [FWHM(Isub), Nstars(Isub)] = imUtil.psf.fwhm_fromBank(Sub(Isub).Im, 'SigmaVec',Args.SigmaVec,...
                                                                     'MinSN',Args.MinSN,...
                                                                     'MinStars',Args.MinStars,...
                                                                     'PixScale',Args.PixScale,...
                                                                     'Method',Args.Method,...
                                                                     'MaxIter',Args.MaxIter);
    end
    Result.FWHM       = reshape(FWHM, Nxy(2), Nxy(1));
    Result.Nstars     = reshape(Nstars, Nxy(2), Nxy(1));
    Result.ListEdge   = ListEdge;
    Result.X          = reshape(ListCenter(:,1), Nxy(2), Nxy(1));
    Result.Y          = reshape(ListCenter(:,2), Nxy(2), Nxy(1));
    
    X = Result.X(:) - mean(Result.X(1,:));
    Y = Result.Y(:) - mean(Result.Y(:,1));
    
    if nargout>1
        % fit
        N = numel(FWHM);
        H = [ones(N,1), X, Y, X.*Y, X.^2, Y.^2];
        if Args.Clean
            MedFWHM = median(FWHM);
            Flag = FWHM>(MedFWHM-Args.HalfRangeGood) & FWHM<(MedFWHM+Args.HalfRangeGood);
        else
            Flag = true(N,1);
        end
        Fit.Par   = H(Flag,:)\FWHM(Flag);
        Fit.Flag  = Flag;
        Fit.Resid = FWHM - H*Fit.Par;
        
        Fit.ExtramXc = (Fit.Par(3).*Fit.Par(4) - 2.*Fit.Par(2).*Fit.Par(6))./(-Fit.Par(4).^2 + 4.*Fit.Par(5).*Fit.Par(6));
        Fit.ExtramYc = (Fit.Par(2).*Fit.Par(4) - 2.*Fit.Par(3).*Fit.Par(5))./(-Fit.Par(4).^2 + 4.*Fit.Par(5).*Fit.Par(6));
        
    end
    
    if Args.Plot
        surface(Result.X(1,:), Result.Y(:,1),Result.FWHM.*1.25);
        H=colorbar;
        H.Label.String='FWHM [arcsec]';
        H.Label.Interpreter='latex';
        H=xlabel('X [pix]');           
        H.FontSize=18;
        H.Interpreter='latex';
        H=ylabel('Y [pix]');
        H.FontSize=18;
        H.Interpreter='latex';
        H = gcf;
        H.PaperPosition=[1.25 3.28 6 6];
        
    end
        
end