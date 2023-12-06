function [Result]=linearLeastSq(H, Y, Cov, Args)
    % Formal linear least squars with known covariance, but with formal
    %   errors (no rescaling).
    % Input  : - A design matrix. E.g., [f_1(x_i),f_2(x_i),...]
    %          - A column vector of observations (Y).
    %            Alternatively, this can be a matrix. In this case a solution
    %            will be returned for each column of the matrix.
    %          - A covariance matrix, or a vector of variances, or a single
    %            variance for all measurments.
    %          * ...,key,val,...
    %            'Inv' - Matrix inversion method. Options are @inv|@pinv.
    %                   Default is @inv.
    % Output : - A structure array (one per Y column) with the following
    %            fields:
    %            .Par     - Best fitted parameters
    %            .OutCov  - Covariance matrix of the fitted parameters.
    %                   Note that this matrix does not depands on the data
    %                   points Y. It depands only on X and the input Cov.
    %            .ParErr - Formal errors on the fitted paramaeters.
    %                   As obtained for the sqrt of the diagonal of the
    %                   output covariance matrix.
    %            .Resid - Vector of residuals.
    %            .RMS - std of the vector of residuals.
    %            .Chi2 - Chi^2.
    %            .Dof - Degrees of freedom.
    % Author : Eran Ofek (Oct 2023)
    % Example: X = (-10:1:10)'; Y = 1+0.1.*X + randn(numel(X),1e4).*0.1;
    %          H = [ones(size(X)), X];
    %          [Result]=tools.math.fit.linearLeastSq(H, Y, 0.1.^2)


    arguments
        H
        Y
        Cov
        Args.Inv = @inv;
    end

    [N,Npar] = size(H);
    if min(size(Cov))==1
        if numel(Cov)==1
            Cov = diag(Cov.*ones(N,1));
        else
            % covariance is a vector of variances
            Cov = diag(Cov);
        end
    end

    InvCov = Args.Inv(Cov);

    Ncol = size(Y,2);
    Result = struct('Par',cell(Ncol,1), 'OutCov',cell(Ncol,1), 'ParErr',cell(Ncol,1), 'Resid',cell(Ncol,1), 'RMS',cell(Ncol,1), 'Chi2',cell(Ncol,1), 'Dof',cell(Ncol,1));
    for Icol=1:1:Ncol
        Di      = Y(:,Icol).*InvCov*H;
        OutCov  = Args.Inv(H.' * InvCov * H);
        
        Result(Icol).Par    = sum(OutCov*Di.',2);
        Result(Icol).OutCov = OutCov;
        Result(Icol).ParErr = sqrt(diag(OutCov));
        Result(Icol).Resid  = Y(:,Icol) - H*Result(Icol).Par;
        Result(Icol).RMS    = std(Result(Icol).Resid);
        Result(Icol).Chi2   = sum((Result(Icol).Resid.^2./diag(Cov)));
        Result(Icol).Dof    = N - Npar;
    end

end
