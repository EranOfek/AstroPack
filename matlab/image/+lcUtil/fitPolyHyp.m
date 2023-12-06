function [Result] = fitPolyHyp(Obj, Args)
    % Hypothesis testing between fitting polynomials of various degrees to
    %   a matrix of light curves in a MatchedSources object (with unknown errors).
    %   Like timeSeries.fit.fitPolyHyp, but for a MatchedSources class.
    % Input  : - A MatchedSources object.
    %          * ...,key,vals,...
    %            'MagFieldNames' - A cell array of dictionary field names
    %                   for the Magnitude matrix in the MatchedSources
    %                   object.
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
    % Example: MS = MatchedSources;
    %          MS.addMatrix(rand(100,200),'FLUX')
    %          MS.addMatrix({rand(100,200), rand(100,200), rand(100,200)},{'MAG','X','Y'})
    %          Result = lcUtil.fitPolyHyp(MS);
    
    arguments
        Obj(1,1) MatchedSources
        
        Args.MagFieldNames               = AstroCatalog.DefNamesMag;
        Args.PolyDeg cell                = {[0], [0:1:1], [0:1:2], [0:1:3], [0:1:4], [0:1:5]};
        Args.SubtractMeanT(1,1) logical  = true;
        Args.NormT(1,1) logical          = true;
        Args.CalcProb(1,1) logical       = false;
    end
    
    % get field name from dictionary
    [FieldName] = getFieldNameDic(Obj, Args.MagFieldNames);
    
    % get Mag matrix
    Mag = getMatrix(Obj, FieldName);
    
    % poly hypothesis testing
    Result = timeSeries.fit.fitPolyHyp(Obj.JD, Mag);
    
end