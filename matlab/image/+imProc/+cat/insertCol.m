function [Obj] = insertCol(Obj, Args)
    % Insert columns to an AstroCatalogs in AstroImage
    % Input  : - An AstroImage or AstroCatalog object.
    %          * ...,key,val,... 
    %            'InsertJD' - A logical indicating if to insert a JD
    %                   column. Default is true.
    %            'ColNameJD' - A JD column name. Default is 'JD'.
    %            'InsertId' - A logical indicating if to insert an Id taken
    %                   from the obkect element index. Default is true.
    %            'ColNameId' - A Id column name. Default is 'CropID'.
    %            'Val' - A scalar or vector of values to insert in a
    %                   column. Default is [].
    %            'ColNameVal' - Column name for the 'Val' data.
    %                   Default is ''.
    %            'ColNameUnits' - Coluumn units for the 'Val' data.
    %                   Default is ''.
    % Output : - The input AstroImage/AstroCatalog updated with the new
    %            columns.
    % Author : Eran Ofek (2024 Feb) 
    % Example: imProc.cat.insertCol(SI)

    arguments
        Obj    % AstroImage or AstroCatalog
        
        Args.InsertJD logical     = true;
        Args.ColNameJD            = 'JD';
        Args.InsertId logical     = true;
        Args.ColNameId            = 'CropID';

        Args.Val                  = [];
        Args.ColNameVal           = '';
        Args.ColNameUnits         = '';


        Args.CreateNewObj logical   = false;
    end


    Nobj = numel(Obj);
    for Iobj=1:1:Nobj
        Cat = imProc.cat.getCat(Obj(Iobj), 'CreateNewObj',Args.CreateNewObj);

        Nsrc = Cat.sizeCatalog;
        if Args.InsertJD
            Cat.insertCol(repmat(Cat.JD, Nsrc,1), Inf, Args.ColNameJD, 'day');
        end
        if Args.InsertId
            Cat.insertCol(repmat(Iobj, Nsrc,1), Inf, Args.ColNameId, '');
        end
        if ~isempty(Args.ColNameVal) && ~isempty(Args.Val)
            if numel(Args.Val)==1
                Args.Val = repmat(Args.Val, Nsrc, 1);
            end
            Cat.insertCol(Args.Val, Inf, Args.ColNameVal, Args.ColNameUnits);
        end
        if isa(Obj, 'AstroImage')
            Obj(Iobj).CatData = Cat;
        else
            Obj(Iobj) = Cat;
        end
    end

end
