function Cov = covMatrix_nonEvenlySpaced_PS(Time, Err, CovModel, Args)
    % Calculate the cov matrix of a non-evenly time series with a power-law power spectrum
    % Input  : - A vector of time.
    %          - A vector (or scalar) of error (Err.^2 will be added to the
    %            diagonal of the cov matrix).
    %            Default is 1.
    %          - Covariance model [Amp -Slope] for a power law power
    %            spectrom of the form: Amp * Omega.^(-Slope)
    %            Default s [1 3].
    %          * ...,key,val,...
    %            'Freq' - A vector of frequencies in which to integrate the
    %                   power spectrum for the covariance.
    %                   If empty, then choose: 
    %                       HighFreq  = Args.HighFreqOver./min(diff(Time));
    %                       Args.Freq = (1./RangeT:1./(2.*RangeT):HighFreq);
    %            'HighFreqOver' - Oversampling in the high frequency limit.
    %                   Default is 5.
    %            'NoLoop' - A logical indicating if to use a no-lop
    %                   calculation. Somewhat faster, but requires more
    %                   RAM. Default is false.
    % Output : - The covariance matrix.
    % Reference: Springer & Ofek (2021) MNRAS 506, 864-876
    % Author : Eran Ofek (Nov 2025)
    % Example: Cov=covMatrix_nonEvenlySpaced_PS(T,1)

    arguments
        Time
        Err               = 1;
        CovModel          = [1 3];
        Args.Freq         = [];
        Args.HighFreqOver = 5;
        Args.NoLoop       = false;
    end


    if isempty(Args.Freq)
        RangeT    = range(Time);
        HighFreq  = Args.HighFreqOver./min(diff(Time));
        Args.Freq = (1./RangeT:1./(2.*RangeT):HighFreq);
    end
    Omega = 2.*pi.*Args.Freq(:);

    Nt = numel(Time);
    if isscalar(Err)
        Err = repmat(Err,Nt,1);
    end

    if Args.NoLoop
        ErrMat = diag(Err.^2);
        ErrMat = ErrMat(:).';
    
        AllDeltaT = Time(:) - Time(:).';
        AllDeltaT = AllDeltaT(:).';
        Cov = sum( (CovModel(1).*Omega.^-CovModel(2)) .* cos(Omega.*AllDeltaT), 1) + ErrMat;
        Cov = reshape(Cov, [Nt Nt]);
    else

        Cov = zeros(Nt, Nt);
        for It_i=1:1:Nt
            for It_j=It_i:1:Nt
                % Integrate the cov term i-j
                Integrand = (CovModel(1).*Omega.^-CovModel(2)) .* cos(Omega.*(Time(It_i)-Time(It_j)));
                Integral  = sum(Integrand);
                if It_i==It_j
                    Cov(It_i, It_j) = Integral + Err(It_i).^2;
                else
                    Cov(It_i, It_j) = Integral;
                    Cov(It_j, It_i) = Integral;
                end
            end
        end
    end

    
end
