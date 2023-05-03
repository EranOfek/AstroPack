function Result = fitPolyHyp(T, Mag, Args)
    % Hypothesis testing between fitting polynomials of various degrees to
    % a matrix of light curves (with unknown errors).
    % Input  : - A vector of times.
    %          - A matrix of light curves [Epochs X Sources]
    %          * ...,key,vals,...
    %            'PolyDeg' - A cell array in wich each element contains all
    %                   the degrees of the polynomial to fit.
    %                   E.g., [0:1:2], is a full 2nd deg polynomial.
    %                   The first cell corresponds to the null hypothesis.
    %                   The Delta\chi2^2 is calculated relative to the null
    %                   hypothesis. In addition, the error normalization is
    %                   calculated such that the chi^2/dof of the null
    %                   hypothesis will be 1 (with uniform errors).
    %                   Default is {[0], [0:1:1], [0:1:2], [0:1:3], [0:1:4], [0:1:5]}.
    %            'SubtractMeanT' - A logical indicating if to subtract the
    %                   mean of the time vectors from all the times.
    %                   Default is true.
    %            'NormT' - A logical indicating if to normalize the times
    %                   to unity (i.e., max of abs of times will be 1.
    %                   Default is true.
    %            'CalcProb' - Add a field to the output structure with the
    %                   probability to reject the null hypothesis given the
    %                   \Delta\chi^2. This may double the run time.
    %                   Default is false.
    % Output : - A structure array with parameters of the fit for each
    %            tested polynomial (number of elements is like the number
    %            of elements in PolyDeg).
    %            .PolyDeg - Polynomial degrees in the fit.
    %            .Npar - Number of free parameters in the fit.
    %            .Par - The best fitted parameter for each LC. [Npar X Nsrc]
    %            .Chi2 - chi^2 per source.
    %            .Ndof - Number of degrees of freedom.
    %            .ResidStd - Vector of std of residuals for each source.
    %            .DeltaChi2 - A vector of \Delta\chi^2 per source.
    %            .DeltaNdof - The difference in degrees of freedom between
    %                   this hypotesis and the null hypothesis.
    %            .ProbChi2 - (return only if ProbChi2=true). - The
    %                   probability to reject the null hypothesis.
    % Author : Eran Ofek (Sep 2021)
    % Example: T   = (1:1:20)./1440;
    %          Mag = randn(20,500);
    %          Res = timeSeries.fit.fitPolyHyp(T, Mag);
    
    arguments
        T
        Mag
        Args.PolyDeg cell                = {[0], [0:1:1], [0:1:2], [0:1:3], [0:1:4], [0:1:5]};
        Args.SubtractMeanT(1,1) logical  = true;
        Args.NormT(1,1) logical          = true;
        Args.CalcProb(1,1) logical       = false;
    end
    
    if Args.SubtractMeanT
        T = T - mean(T);
    end
    
    if Args.NormT
        T = T./max(abs(T));
    end
    
   
    NpolyDeg       = numel(Args.PolyDeg);
    [Nepoch, Nsrc] = size(Mag);
    H              = zeros(Nepoch, NpolyDeg);
    for IpolyDeg=1:1:NpolyDeg
        PolyDeg = Args.PolyDeg{IpolyDeg};
        H = T(:).^(PolyDeg(:).');  % design matrix for polynomial
        % LSQ for all LCs simultanously
        Par      = H\Mag;
        Resid    = Mag - H*Par;
        ResidStd = std(Resid, [], 1);
        Ndof     = Nepoch - numel(PolyDeg);
        
        if IpolyDeg==1
            % estimate the error normalization factor
            % this is done only for the "null" hypothesis defined as the
            % first cell in PolyDeg
            Chi2norm = sqrt(sum(Resid.^2, 1)./Ndof);
        end
        
        Chi2 = sum((Resid./Chi2norm).^2);
        
        Result(IpolyDeg).PolyDeg   = PolyDeg;
        Result(IpolyDeg).Npar      = numel(PolyDeg);
        Result(IpolyDeg).Par       = Par;
        Result(IpolyDeg).Chi2      = Chi2;
        Result(IpolyDeg).Ndof      = Ndof;
        Result(IpolyDeg).ResidStd  = ResidStd;
        Result(IpolyDeg).DeltaChi2 = Result(1).Chi2 - Result(IpolyDeg).Chi2;
        Result(IpolyDeg).DeltaNdof = Result(IpolyDeg).Npar - Result(1).Npar;
        % probability to reject the null hypothesis
        if Args.CalcProb
            Result(IpolyDeg).ProbChi2  = chi2cdf(Result(IpolyDeg).DeltaChi2, Result(IpolyDeg).DeltaNdof);
        end
    end
    
end