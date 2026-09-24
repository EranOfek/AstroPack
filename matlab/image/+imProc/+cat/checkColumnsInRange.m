function [Result] = checkColumnsInRange(Obj, Args)
    % Check that columns in AstroTable or AstroCatalog in AstroImage are pre-defined in range. 
    % Input  : - An AstroCatalog/AstroTable, or AstroImage/AstroZOGY
    %            (containing AstroCatalog) object.
    %          * ...,key,val,... 
    %            'ColTest' - A three column cell array containing:
    %                   Column name,
    %                   Allowed [Min Max],
    %                   NaN_Allowd (logical).
    %                   Default is {}.
    % Output : - A matrix of size [Nobj, Ncol] containing logicals, for
    %            each input object element, and coloumns to test.
    %            True if all values in columns are within range or NaN.
    % Author : Eran Ofek (2026 Mar) 
    % Example: AC=AstroCatalog({rand(5,10), rand(14,10)})
    %          R=imProc.cat.checkColumnsInRange(AC, {'Var2',[0 1],1; 'Var4',[0 0.5],1});

    arguments
        Obj
        Args.ColTest      = {};  % {'Name', [Min Max], NaN_Allowed}
    end

    Ncol   = size(Args.ColTest, 1);

    Nobj   = numel(Obj);
    Result = false(Nobj, Ncol);
    for Iobj=1:1:Nobj
        Cat = Obj(Iobj).getCatData;
       
        for Icol=1:1:Ncol
            %
            ColData = Cat.getCol(Args.ColTest{Icol,1});

            InRange = ColData>=Args.ColTest{Icol,2}(1) & ColData<=Args.ColTest{Icol,2}(2);

            if Args.ColTest{Icol,3}
                % NaN are allowed
                InRange = InRange | isnan(ColData);
            end

            Result(Iobj,Icol) = all(InRange);
        end

        Obj(Iobj).setCatData(Cat);
    end

end
