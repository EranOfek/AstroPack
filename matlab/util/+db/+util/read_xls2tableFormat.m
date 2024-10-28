function [Result] = read_xls2tableFormat(FileName, Args)
    % One line description
    %     Optional detailed description
    % Input  : - 
    %          - 
    %          * ...,key,val,... 
    % Output : - 
    % Author : Eran Ofek (2024 Oct) 
    % Example: 

    arguments
        FileName                = 'Design-Database-Pipeline-ClickHouse.xlsx';
        Args.Sheet              = 'Images';
        Args.VariableNamesRange = 'A3';

        Args.TableName          = 'visit_images';
        Args.TableColName       = 'column_name';
        Args.HeadColName        = 'header_col_name';
        Args.TypeColName        = 'Type';
        Args.FunColName         = 'Fun';


    end

    TT = readtable(FileName,'Sheet',Args.Sheet,'VariableNamesRange',Args.VariableNamesRange);
    
    FlagUse = TT.(Args.TableName);
    FlagUse(isnan(FlagUse)) = false;
    FlagUse = logical(FlagUse);
    FlagUse = FlagUse & ~tools.cell.isempty_cell(TT.(Args.HeadColName)).';

    TT      = TT(FlagUse,:);

    [UnCol,Iunique] = unique(TT.(Args.HeadColName), 'stable');
    Nun = numel(Iunique);
    Result = struct('ColName',cell(Nun,1), 'ColNameOut',cell(Nun,1), 'ColFun',cell(Nun,1));
    for Iun=1:1:Nun
        IU = Iunique(Iun);
        Imulti = find(strcmp(UnCol(Iun), TT.(Args.HeadColName)));

        Result(Iun).ColName    = UnCol{Iun};
        Result(Iun).ColNameOut = string(TT.(Args.TableColName)(Imulti));
        Result(Iun).ColFun     = TT.(Args.FunColName)(Imulti(1));

        % if numel(Imulti)>1
        %     Iun
        %     UnCol(Iun)
        %     TT.(Args.TableColName)(Imulti)
        % end
    end



end
