function [Result] = read_xls2tableFormat(FileName, Args)
    % Read xls file containing DB table definitions and prepare structre
    %   which is the input for: imProc.db.insertImages
    % Input  : - xls file name to read.
    %            Default is 'Design-Database-Pipeline-ClickHouse.xlsx'.
    %          * ...,key,val,... 
    %            See code for options.
    % Output : - A structure array with the information regarding the 
    %            columns in the table.
    % Author : Eran Ofek (2024 Oct) 
    % Example: R=db.util.read_xls2tableFormat

    arguments
        FileName                = 'Design-Database-Pipeline-ClickHouse.xlsx';
        Args.Sheet              = 'Images';
        Args.VariableNamesRange = 'A3';

        Args.TableName          = 'visit_images';
        Args.TableColName       = 'column_name';
        Args.HeadColName        = 'header_col_name';
        Args.TypeColName        = 'Type';
        Args.FunColName         = 'Fun';

        Args.Convert2Upper logical = true;   % convert ColName and ColNameOut to upper case.

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

        if Args.Convert2Upper
            Result(Iun).ColName    = upper(UnCol{Iun});
            Result(Iun).ColNameOut = upper(string(TT.(Args.TableColName)(Imulti)));
        else
            Result(Iun).ColName    = UnCol{Iun};
            Result(Iun).ColNameOut = string(TT.(Args.TableColName)(Imulti));
        end
        
        if isempty(TT.(Args.FunColName){Imulti(1)})
            Result(Iun).ColFun = [];
        else
            Result(Iun).ColFun     = str2func(TT.(Args.FunColName){Imulti(1)});
        end

        % if numel(Imulti)>1
        %     Iun
        %     UnCol(Iun)
        %     TT.(Args.TableColName)(Imulti)
        % end
    end



end
