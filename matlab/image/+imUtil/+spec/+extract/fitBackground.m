function [Back, Image] = fitBackground(Image, Args)
    % Fit background to a 2D spectral image
    %     Fit background using several methods, including global
    %     background, mean/median per wavelength, and slope fitting with
    %     sigma clipping iterations.
    % Input  : - A 2-D image matrix.
    %          * ...,key,val,... 
    %            'DimWave' - The dimension of the wavelength direction.
    %                   Default is 2.
    %            'Flag' - An optional matrix of logicals indicating (with
    %                   true) pixels to ignore in the background estimation.
    %                   If empty, then use all pixels.
    %                   Default is [].
    %            'TracePos' - The position of the trace (spectrum) in the
    %                   spatial direction.
    %                   If empty, then set it to (Npsat-1)/2, where Nspat
    %                   is the number of spatial pixels.
    %                   Default is [].
    %            'Annulus' - Annulus (like in 2D photometry) is the
    %                   [inner, outer] pixel position above and below the
    %                   'TracePos' position from which to extract the
    %                   pixels that will be used for the background
    %                   estimation.
    %                   Default is [10 15].
    %            'Method' - One of the following background fitting methods:
    %                   'global' - Global background in the annulus region. 
    %                   'wave' - Background is estimated per wavelength.
    %                   'poly' - Fit a polynomial per each wavelength
    %                           (with sigma clipping).
    %                           Note that if 'FitOrders' is set to [0],
    %                           then this is like 'wave', but with the
    %                           sigma clipping option.
    %                   Default is 'wave'.
    %            'Fun' - If Method=global|wave, then this is a the function
    %                   handle that used to calculate the background.
    %                   The function have the form Back=Fun(Data, Dim, FunArgs{:})
    %                   Default is @median.
    %            'FunArgs' - A cell array of additional arguments to pass
    %                   to Fun. Default is {'omitnan'}.
    %            'FitOrders' - In case Method=poly, then this is a vector
    %                   of polynomial orders to fit.
    %                   Default is [0 1].
    %            'SigmaClip' - In case Method=poly, then this is the
    %                   [lower, upper] sigma clipping in units of the rstd.
    %                   Default is [3 3].
    %            'Niter' - Number of sigma clipping iterations.
    %                   1 for no sigma clipping.
    %                   Default is 2.
    % Output : - The background scalar, vector or image.
    %          - If two output arguments are requested, then this is the
    %            background subtracted image.
    % Author : Eran Ofek (2023 Dec) 
    % Example: Image = 100+randn(50,1000);
    %          [Back] = imUtil.spec.extract.fitBackground(Image)
    %          [Back] = imUtil.spec.extract.fitBackground(Image, 'Method','global')
    %          [Back] = imUtil.spec.extract.fitBackground(Image, 'Method','slope')
    %          %Test with NaN
    %          Image(11,10) = NaN; Image(12,12) = NaN;
    %          [Back] = imUtil.spec.extract.fitBackground(Image, 'Method','slope')

    arguments
        Image
        Args.DimWave                 = 2;
        Args.Flag                    = [];     % an optional flag image (ignote flagged pixels)
        Args.TracePos                = [];   % or scalar
        Args.Annulus                 = [10 15];
        Args.Method                  = 'wave';     % 'poly'|'wave'|'global';  
        Args.Fun                     = @median;
        Args.FunArgs                 = {'omitnan'};  % F(X, Dim, other pars)
        Args.FitOrders               = [0 1];  % for first order poly
        Args.SigmaClip               = [3 3];
        Args.Niter                   = 2;
    end

    % Convert image such that the wave-dir is in the 2nd dim (x-axis)
    if Args.DimWave==1
        Image = Image.';
    end
        
    % number of pixels in each direction
    [Nspat, Nwave] = size(Image);
    
    if isempty(Args.TracePos)
        Args.TracePos = (Nspat - 1).*0.5;
    end
    
    AnnulusRangeLow  = (round(Args.TracePos - Args.Annulus(2)):1:round(Args.TracePos - Args.Annulus(1)));
    AnnulusRangeHigh = (round(Args.TracePos + Args.Annulus(1)):1:round(Args.TracePos + Args.Annulus(2)));
    
    
    % Flag image
    if isempty(Args.Flag)
        % do nothing
    else
        Image(Args.Flag) = NaN;
    end
    
    BackRegionVector = [AnnulusRangeLow(:); AnnulusRangeHigh(:)];
    OnlyBack = Image(BackRegionVector,:);
    
    switch lower(Args.Method)
        case 'global'
            Back     = Args.Fun(OnlyBack, 'all', Args.FunArgs{:}); % scalar
            
        case 'wave'            
            Back     = Args.Fun(OnlyBack, 1, Args.FunArgs{:}); % vector
            
        case 'poly'
            % for each wavelength, fit a slope in the spatila dir
            % spatial dir vector
            SpatVector    = (1:1:Nspat).';
            SpatVectorFit = SpatVector(BackRegionVector);
            
            % design matrix
            H = (BackRegionVector-Args.TracePos).^(Args.FitOrders(:).');
            Hfull = (SpatVector-Args.TracePos).^(Args.FitOrders(:).');
            
            for Iiter=1:1:Args.Niter
                
                % can not fit all simoltanosly as some may contains NaNs
                Par = zeros(numel(Args.FitOrders), Nwave);
                for Iwave=1:1:Nwave
                    Fnn = ~isnan(OnlyBack(:,Iwave));
                    Par(:,Iwave)  = H(Fnn,:)\OnlyBack(Fnn,Iwave);
                end
                

                % sigma clipping
                if Iiter<Args.Niter
                    Std = tools.math.stat.rstd(OnlyBack,1);
                    Med = median(OnlyBack,1,'omitnan');
                    Z   = (OnlyBack - Med)./Std;
                    FlagBad = Z > Args.SigmaClip(2) | Z < -abs(Args.SigmaClip(1));
                    OnlyBack(FlagBad) = NaN;
                end
            end
            
            Back = Hfull*Par;
            
        otherwise
            error('Unknown Method option');
    end
            
    % optional back subtraction
    if nargout>1
        Image = Image - Back;
    end
     
    % return to original shape
    if Args.DimWave==1
        Image = Image.';
    end
    
end
