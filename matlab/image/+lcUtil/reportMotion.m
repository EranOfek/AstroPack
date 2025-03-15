function [Result, ColNames] = reportMotion(Obj, Args)
    % Return proper motion fit for each source in MatchedSources object.
    % Input  : - A single element MatchedSources object.
    %          * ...,key,val,... 
    %            'fitMotionArgs' - A cell array of additional arguments to pass
    %                   to lcUtil.fitMotion.
    %                   Default is {}.
    %            'OutType' - Output type:
    %                   'matrix' - Matrix output.
    %                   'table' - table output.
    %                   Default is 'table'.
    % Output : - A matrix or table of proper motion information for each
    %            source.
    % Author : Eran Ofek (2025 Mar) 
    % Example: RM=lcUtil.reportMotion(MS);

    arguments
        Obj(1,1)
        Args.fitMotionArgs     = {};
        Args.OutType           = 'table';
    end

    ResM = lcUtil.fitMotion(Obj, Args.fitMotionArgs{:});
    Nsrc = Obj.Nsrc;
    
    ColNames = {'PM_JD', 'PM_DeltaChi2', 'PM_Prob', 'PM_Ngood', 'PM_RA0', 'PM_Dec0', 'PM_MuRA', 'PM_MuDec'};
    Result = [ResM.MeanTime.*ones(Nsrc,1), ResM.DeltaChi2(:), ResM.Prob(:), ResM.Ngood(:), ResM.RA0(:), ResM.Dec0(:), ResM.MuRA(:), ResM.MuDec(:)];
    
    
    switch lower(Args.OutType)
        case 'table'
            Result = array2table(Result);
            Result.Properties.VariableNames = ColNames;
        otherwise
            % do nothing
    end
    
    
end

