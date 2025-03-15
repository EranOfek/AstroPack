function [Result, ColNames] = reportPolyHyp(Obj, Args)
    % Return DeltaChi2 between 0 and N polynomials for each source in MatchedSources object.
    % Input  : - A single element MatchedSources object.
    %          * ...,key,val,... 
    %            'FieldMag' - Default is 'MAG_APER_3'.
    %            'PolyDeg' - A cell array in wich each element contains all
    %                   the degrees of the polynomial to fit.
    %                   E.g., [0:1:2], is a full 2nd deg polynomial.
    %                   The first cell corresponds to the null hypothesis.
    %                   The Delta\chi2^2 is calculated relative to the null
    %                   hypothesis. In addition, the error normalization is
    %                   calculated such that the chi^2/dof of the null
    %                   hypothesis will be 1 (with uniform errors).
    %                   Default is {[0], [0:1:1], [0:1:2], [0:1:3], [0:1:4], [0:1:5]}.
    %            'OutType' -  Output type:
    %                   'matrix' - Matrix output.
    %                   'table' - table output.
    %                   Default is 'table'.
    % Output : - A matrix of table of report with 3 columns per each tested
    %            polynomials. These are the DeltaChi2, DeltaNdof, and std
    %            of residuals after the best fit polynomial subtraction.
    % Author : Eran Ofek (2025 Mar) 
    % Example: R=lcUtil.reportPolyHyp(MS)

    arguments
        Obj
        Args.FieldMag                    = AstroCatalog.DefNamesMag;
        Args.PolyDeg cell                = {[0], [0:1:1], [0:1:2], [0:1:3], [0:1:4], [0:1:5]};
        Args.OutType                     = 'table';
    end

    Ncol = 3;
    
    ResPH = lcUtil.fitPolyHyp(Obj, 'MagFieldNames',Args.FieldMag, 'PolyDeg',Args.PolyDeg);

    
    Npd   = numel(Args.PolyDeg);
    Result = zeros(Obj.Nsrc, Npd.*Ncol);
    ColNames = cell(1, Npd.*Ncol);
    for Ipd=1:1:Npd
        ColI = (Ipd-1).*Ncol;
        MaxPoly = max(Args.PolyDeg{Ipd});
        ColNames{ColI+1} = sprintf('Poly%d_DeltaChi2',MaxPoly);
        ColNames{ColI+2} = sprintf('Poly%d_DeltaNdof',MaxPoly);
        ColNames{ColI+3} = sprintf('Poly%d_ResidStd',MaxPoly);
        Result(:, ColI + 1) = ResPH(Ipd).DeltaChi2(:);
        Result(:, ColI + 2) = ResPH(Ipd).DeltaNdof.*ones(Obj.Nsrc,1);
        Result(:, ColI + 3) = ResPH(Ipd).ResidStd(:);
    end
    
    switch lower(Args.OutType)
        case 'table'
            Result = array2table(Result);
            Result.Properties.VariableNames = ColNames;
        otherwise
            % do nothing
    end
    
end
