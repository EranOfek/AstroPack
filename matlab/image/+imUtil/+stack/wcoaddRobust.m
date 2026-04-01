function [Coadd, CoaddVar, Ncoadd]=wcoaddRobust(Image, Back, Args)
    % weighted and robust coaddition of images
    %   The function performs weighted coaddition where the weights are
    %   (F/Var), and F is the flux zeri point and Var is the background
    %   variance.
    %   In addition it performs robust outliers removal in two steps.
    %   First, an optional single min/max rejection, followed by an
    %   optional sigma clipping.
    %   For the Sigma clipping Std calculation, 3 methods are available.
    %   See also (fast mex): imUtil.stack.mex.wcoaddRobust_mex
    %   Or use the 'UseMex' option. 
    % Input  : - Image cube, where the image index is in the 3rd dim.
    %          - Background. If empty, assumes that the images are
    %            background subtracted. Otherwise, can be a scalar, vector
    %            (with length like the number of images), or cube.
    %          * ...,key,val,...
    %            'Var' - Scalar, vector, or cube of variance.
    %                    Default is 1.
    %            'F'   - Flux matching factor, per image. Each image is
    %                    multiplied by F in order to bring it to a common
    %                    zero point.
    %                    Default is [].
    %            'ZP'  - Zero point for calculating F.
    %                    Default is 25.
    %            'ZP0' - Arbitrary common ZP offset. Default is 25.
    %            --- Robust parameters ---
    %            'RemoveMinMax' - Remove min and max values in each pixel
    %                    prior to coaddition.
    %                    Default is true.
    %            'Niter' - Number of sigma clipping iterations.
    %                    For Niter=0 no sigma clipping is done.
    %                    Default is 1.
    %            'SigmaClip' - [Low High] sigma clipping thresholds.
    %                    Units are standard deviations.
    %                    Default is [3 3].
    %            'StdMethod' - Method to estimate the scatter for sigma
    %                    clipping:
    %                    1 = std around weighted mean.
    %                    2 = scaled mean absolute deviation around weighted
    %                        mean.
    %                    3 = scaled weighted median absolute deviation
    %                        around weighted median.
    %                    Default is 2.
    %            'UseMex' - A logical indicating if to use the mex version
    %                   of this code in: imUtil.stack.mex.wcoaddRobust_mex
    %                   Default is true.
    % Output  : - Weighted coadd image on the common photometric scale.
    %             The flux scale of the coadd image is always 1 (in units of
    %             'F'). If ZP is used then the ZP is ZP0.
    %           - The coadd variance image (or scalar).
    %           - A matrix (single) of the number of used images per pixel.
    %             This is available only when using 'UseMex'=true,
    %             otherwise, will return [].
    % Author : Eran Ofek (Mar 2026)
    % Example: [C, Cvar,Ncoadd] = imUtil.stack.wcoaddRobust(Im, B, 'Var',V, 'F',F_k, 'ZP',ZP,'ZP0',ZP0,'RemoveMinMax',RemoveMinMax,'Niter',Niter,'SigmaClip',SigmaClip, 'StdMethod',StdMethod);


    arguments
        Image
        Back      = [];
        Args.Var  = 1;
        Args.F    = [];
        Args.ZP   = 25;
        Args.ZP0  = 25;
        Args.RemoveMinMax = true;
        Args.Niter = 1;
        Args.SigmaClip = [3 3];
        Args.StdMethod = 2;
        Args.UseMex    = true;
    end

    if Args.UseMex
        if nargout==3
            [Coadd, CoaddVar, Ncoadd] = imUtil.stack.mex.wcoaddRobust_mex(Image, Back, Args.Var, Args.F, Args.ZP, Args.ZP0, Args.RemoveMinMax, Args.Niter, Args.SigmaClip, Args.StdMethod);
        elseif nargout==2
            [Coadd, CoaddVar] = imUtil.stack.mex.wcoaddRobust_mex(Image, Back, Args.Var, Args.F, Args.ZP, Args.ZP0, Args.RemoveMinMax, Args.Niter, Args.SigmaClip, Args.StdMethod);
        else
            [Coadd] = imUtil.stack.mex.wcoaddRobust_mex(Image, Back, Args.Var, Args.F, Args.ZP, Args.ZP0, Args.RemoveMinMax, Args.Niter, Args.SigmaClip, Args.StdMethod);
        end
    else
        % matlab version
        [SizeY, SizeX, Nim] = size(Image);
    
        %----------------------
        % background subtraction
        %----------------------
        if isempty(Back)
            ImageBS = Image;
        else
            if isscalar(Back)
                Back = repmat(Back, 1, 1, Nim);
            else
                if ismatrix(Back)
                    % back is assumed to be a vector
                    Back = reshape(Back, 1, 1, Nim);
                end
            end
    
            ImageBS = Image - Back;
        end
    
        %----------------------
        % flux scaling factor
        %----------------------
        if isempty(Args.F)
            Args.F = 10.^(0.4.*(Args.ZP0 - Args.ZP));
        end
    
        if isscalar(Args.F)
            Args.F = repmat(Args.F, 1, 1, Nim);
        else
            if ismatrix(Args.F)
                Args.F = reshape(Args.F, 1, 1, Nim);
            end
        end
    
        % variance
        if isscalar(Args.Var)
            Args.Var = repmat(Args.Var, 1, 1, Nim);
        else
            if ismatrix(Args.Var)
                Args.Var = reshape(Args.Var, 1, 1, Nim);
            end
        end
    
        % If ImageScaled = F .* ImageBS
        % then VarScaled = F.^2 .* Var
        % and optimal weights are W = 1./VarScaled = 1./(F.^2 .* Var)
        ImageScaled = Args.F .* ImageBS;
        W           = 1./(Args.F.^2 .* Args.Var);
    
        % Min/Max rejection in the common photometric system
        if Args.RemoveMinMax
            MinIm = min(ImageScaled, [], 3, 'omitnan');
            Mask  = (ImageScaled == MinIm);
            ImageScaled(Mask) = NaN;
    
            MaxIm = max(ImageScaled, [], 3, 'omitnan');
            Mask  = (ImageScaled == MaxIm);
            ImageScaled(Mask) = NaN;
        end
    
        %----------------------
        % sigma clipping
        %----------------------
        SigmaLow  = -abs(Args.SigmaClip(1));
        SigmaHigh =  abs(Args.SigmaClip(2));
    
        for Iiter = 1:Args.Niter
            Valid = isfinite(ImageScaled) & isfinite(W);
    
            WEff     = W .* Valid;
            ImageEff = ImageScaled;
            ImageEff(~Valid) = 0;
    
            SumW  = sum(WEff, 3);
            MeanI = sum(WEff .* ImageEff, 3) ./ SumW;
            MeanI(SumW==0) = NaN;
    
            switch Args.StdMethod
                case 1
                    % standard deviation around weighted mean
                    CenterI = MeanI;
                    StdI    = std(ImageScaled, 0, 3, 'omitnan');
    
                case 2
                    % scaled mean absolute deviation around weighted mean
                    CenterI = MeanI;
                    StdI    = 1.253 .* mean(abs(ImageScaled - CenterI), 3, 'omitnan');
    
                case 3
                    % scaled weighted median absolute deviation around weighted median
                    CenterI = nan(SizeY, SizeX, 'like', ImageScaled);
                    StdI    = nan(SizeY, SizeX, 'like', ImageScaled);
    
                    for Iy = 1:SizeY
                        for Ix = 1:SizeX
                            Xi = reshape(ImageScaled(Iy, Ix, :), [], 1);
                            Wi = reshape(WEff(Iy, Ix, :), [], 1);
    
                            Good = isfinite(Xi) & isfinite(Wi) & Wi>0;
                            Xi   = Xi(Good);
                            Wi   = Wi(Good);
    
                            if ~isempty(Xi)
                                MedVal = localWeightedMedian(Xi, Wi);
                                CenterI(Iy, Ix) = MedVal;
                                StdI(Iy, Ix) = 1.4826 .* localWeightedMedian(abs(Xi - MedVal), Wi);
                            end
                        end
                    end
    
                otherwise
                    error('Args.StdMethod must be 1, 2, or 3');
            end
    
            Z = (ImageScaled - CenterI)./StdI;
            ClipMask = Z<SigmaLow | Z>SigmaHigh;
    
            ImageScaled(ClipMask) = NaN;
        end
    
        % Use the same valid mask in numerator and denominator
        Valid = isfinite(ImageScaled) & isfinite(W);
    
        WEff     = W .* Valid;
        ImageEff = ImageScaled;
        ImageEff(~Valid) = 0;
    
        SumW  = sum(WEff, 3);
        Coadd = sum(WEff .* ImageEff, 3) ./ SumW;
        Coadd(SumW==0) = NaN;
    
        if nargout>1
            % variance of weighted mean on common scale
            CoaddVar = 1./SumW;
            CoaddVar(SumW==0) = NaN;
        end
    end
    Ncoadd = [];
end


function M = localWeightedMedian(X, W)
    % weighted median of a vector X with non-negative weights W

    [Xs, SI] = sort(X);
    Ws = W(SI);

    CumW = cumsum(Ws);
    HalfW = 0.5 .* sum(Ws);

    I = find(CumW >= HalfW, 1, 'first');
    M = Xs(I);
end