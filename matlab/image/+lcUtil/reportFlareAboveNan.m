function [Result] = reportFlareAboveNan(Obj, Args)
    % Search for flare events above NaN (non detection) background.
    %   The search is done on the S/N field.
    % Input  : - A MatchedSOurces object.
    %          * ...,key,val,... 
    %            'FieldSN' - Default is 'SN_3'.
    %            'MinSN' - Detection threshold. Default is 8.
    %            'MinNnondet' - Min. number of NaNs in light curves.
    %                   Default is 5.
    %            'OutType' - Output type:
    %                   'matrix' - Matrix output.
    %                   'table' - table output.
    %                   Default is 'table'.
    % Output : - A structure array with element per MatchedSources object,
    %            with the follwoing fields:
    %            .Flag - A logical indicating if flare is detected.
    %            .MaxSN - Maximal S/N in the runmean filter.
    %            If OutType is table then this is a two columns table.
    % Author : Eran Ofek (2025 Mar) 
    % Example: R=lcUtil.reportFlareAboveNan(M);

    arguments
        Obj(1,1)
        Args.FieldSN           = 'SN_3';
        Args.MinSN             = 8;
        Args.MinNnondet        = 5;
        Args.OutType           = 'table';
        Args.ReportType        = 0;
    end

    Iref = 1;
    Iobj = 1;

    
    switch Args.ReportType
        case 0
            
            MatrixSN = Obj(Iobj).Data.(Args.FieldSN);

            [Nnan, Nedges, FirstAndLastIsNaN] = timeSeries.filter.nanSequence(MatrixSN);
            % mean S/N
            MeanSN = mean(MatrixSN, 1, 'omitnan');
            % integral S/N
            IntegralSN  = sqrt(sum(MatrixSN.^2, 1, 'omitnan'));

            % not NaN
            N_Not_Nan = sum(~isnan(MatrixSN), 1);

            Result = array2table([Nnan(:), N_Not_Nan(:), Nedges(:), FirstAndLastIsNaN(:), MeanSN(:), IntegralSN(:)]);
            Result.Properties.VariableNames = {'N_Nan', 'N_Not_Nan', 'N_Nan_edges', 'FirstAndLastIsNaN', 'MeanSN', 'IntegralSN'};

        case 1
            % old report
            Nobj = numel(Obj);
            Tmp = struct('Flag',cell(Nobj,1), 'MaxSN',cell(Nobj,1));
            for Iobj=1:1:Nobj
                MatrixSN = Obj(Iobj).Data.(Args.FieldSN);
                % renomalizr the SN
                RatioSN  = median(MatrixSN./MatrixSN(Iref,:), 2, 'omitnan');
                MatrixSN = MatrixSN./RatioSN;
        
                MatrixSN(isnan(MatrixSN)) = Args.MinSN;
        
                Res = timeSeries.filter.runMeanFilter(MatrixSN, 'StdFun','one', 'PolyFit',0, 'WinSize',2);
        
                Tmp(Iobj).Flag = sum(Res.Z>Args.MinSN, 1)>1 & sum(MatrixSN<=Args.MinSN, 1)>=Args.MinNnondet;
        
                Tmp(Iobj).MaxSN = max(Res.Z, [], 1);
            end
            
            switch lower(Args.OutType)
                case 'table'
                    if Nobj>1
                        error('table output is possible only for single element object');
                    end
                    Iobj = 1;
                    Result = table(Tmp(Iobj).Flag(:), Tmp(Iobj).MaxSN(:));
                    Result.Properties.VariableNames = {'FlareNanFlag', 'FlareNanSN'};
                    
                otherwise
                    % do nothing
                    Result = Tmp;
            end
        otherwise
            error('Unknown ReportType option');
    end

end
