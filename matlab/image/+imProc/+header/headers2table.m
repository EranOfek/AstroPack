function [Result] = headers2table(Obj, Args)
    % Export headers in AstroHeader/AstroImage into a table.
    % Input  : - AstroHeader or AstroImage.
    %          * ...,key,val,... 
    %            'ColNameDic' - Either a cell array of header keywords to
    %                   extract from headers and insert to output table, or
    %                   a structure array with element per column to
    %                   extract and the following fields:
    %                   .ColName - Column name to extract.
    %                           If the col name does not exist in input
    %                           header, then will populate it with NaN in
    %                           the output table.
    %                   .ColFun - A function to apply to the extracted
    %                           value. If empty, do not apply function.
    %                           This may also include column types
    %                           functions (e.g., @int16).
    %                           Default is empty.
    %                   .ColNameOut - The column name in the output table.
    %                           If empty, use input column name.
    %                           Default is empty.
    %                           If the ColFun returns more then one input,
    %                           then this should be a cell array of output
    %                           column names, per each one of the outputs.
    %            'OutType' - Output type: 'table' | 'AstroCatalog'.
    %                   Default is 'table'.
    % Output : - A table or AstroCatalog with a table.
    % Author : Eran Ofek (2024 Sep) 
    % Example: imProc.header.headers2table(AI)
    %          cd /marvin/LAST.01.01.01/2024/08/13/proc/000126v0/
    %          AI=AstroImage.readFileNamesObj('LAST.01.01.01_20240814.000116.450_clear_1325_000_001_001_sci_coadd_Image_1.fits');
    %          AI(2)=AstroImage.readFileNamesObj('LAST.01.01.01_20240814.000116.450_clear_1325_000_001_002_sci_coadd_Image_1.fits');
    %          R=imProc.header.headers2table(AI,'ColNameDic',{'EXPTIME','CCDSEC','PH_MAGSY'})
    %          St(1).ColName = 'EXPTIME';
    %          St(2).ColName = 'CCDSEC'; St(2).ColFun = @imUtil.ccdsec.ccdsecStr2num; St(2).ColNameOut = {'XMIN','XMAX','YMIN','YMAX'};
    %          St(3).ColName = 'PH_MAGSY'; St(3).ColNameOut = 'PH';
    %          R=imProc.header.headers2table(AI,'ColNameDic',St)


    arguments
        Obj
        Args.ColNameDic        = {};
       
        Args.OutType           = 'table';   % 'AstroCatalog' with table
    end
    
    if iscell(Args.ColNameDic) || isstring(Args.ColNameDic)
        ColName = Args.ColNameDic;
        Ncol    = numel(ColName);
        ColFun  = cell(Ncol,1);
        ColNameOut = ColName;
    elseif isstruct(Args.ColNameDic)
        ColName = {Args.ColNameDic.ColName};
        Ncol    = numel(ColName);
        
        if isfield(Args.ColNameDic, 'ColNameOut')
            ColNameOut = {Args.ColNameDic.ColNameOut};
            IcE = tools.cell.isempty_cell(ColNameOut);
            ColNameOut(IcE) = ColName(IcE);

        else
            ColNameOut = ColName;
        end
        if isfield(Args.ColNameDic, 'ColFun')
            ColFun = {Args.ColNameDic.ColFun};
        else
            ColFun = cell(Ncol,1);
        end
    else
       
    end
    
    Ncol = numel(ColName);
    %FunList
    %OutClass
    
    Nobj = numel(Obj);
   
    OutCell = cell(Nobj, Ncol);
    for Iobj=1:1:Nobj
        if isa(Obj, 'AstroImage')
            AH = Obj(Iobj).HeaderData;
        else
            % assume Obj is AstroHeader
            AH = Obj(Iobj);
        end
        
        for Icol=1:1:Ncol
            Tmp = AH.getVal(ColName{Icol});
            if ~isempty(ColFun{Icol})
                Tmp = ColFun{Icol}(Tmp);     
            end
            
            OutCell{Iobj, Icol} = Tmp; 
        end
    end
    
    TmpTbl = cell2table(OutCell, 'VariableNames',ColName);
    
    % execute functions on specific columns
    if all(tools.cell.isempty_cell(ColFun))
        OutTbl = TmpTbl;
        OutTbl.Properties.VariableNames = ColNameOut;
    else
        OutTbl = table;
        IcolOut = 0;
        for Icol=1:1:Ncol
            [~,NcolCol] = size(TmpTbl.(ColName{Icol}));
            if NcolCol==1
                OutTbl.(ColNameOut{Icol}) = TmpTbl.(ColName{Icol});
            else
                % multiple columns - break
                for IcolCol=1:1:NcolCol
                    OutTbl.(ColNameOut{Icol}{IcolCol}) = TmpTbl.(ColName{Icol})(:,IcolCol);
                end
            end
        end            
        
    end  

    % convert columns to specific types


    switch Args.OutType
        case 'table'
            % do notning
            Result = OutTbl;
        case 'AstroCatalog'
            Result = AstroCatalog;
            Result.Catalog = OutTbl;
        otherwise
            error('Unknown OutType option');
    end

    
end
