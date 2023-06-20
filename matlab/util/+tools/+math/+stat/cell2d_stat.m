function Result=cell2d_stat(X,Y,Val, Args)
    % Given [X,Y,Val], calculate statistics of Val's in X/Y bins
    % Input  : - Vector of X.
    %          - Vector of Y.
    %          - Vector of values for each X,Y.
    %          * ...,key,val,...
    %            'Funs' - A cell array of functions to calculate on each
    %                   bin.
    %                   Default is {@numel, @median, @mean, @tools.math.stat.rstd}
    %            'FunsArgs' - A cell array of cell array of additional
    %                   arguments to pass to each one of the functions in the
    %                   'Funs' argument.
    %                   Default is {{}, {'all','omitnan'}, {'all','omitnan'}, {'all'} }
    %            'FunsName' - A cell array of function names that will be
    %                   used as the field of the output structure.
    %                   If empty, will use the actual function name.
    %                   Default is {}.
    %            'EdgesX' - Vector of X edges.
    %                   Default is [].
    %            'EdgesY' - Vector of Y edges.
    %            'NbinX' - If EdgesX is empty, then this is the number of
    %                   bins in X between the Min-Eps and Max+Eps.
    %                   Default is 5.
    %            'NbinY' - Like 'NbinX', but for the Y axis.
    %                   Default is 5.
    %            'Eps' - A small value to subtract/add from the min(X) and
    %                   max(X) - see 'NbinX/Y'
    %                   Default is 1e-8.
    % Output : - A structure with fields according to the provided
    %            functions.
    % Author : Eran Ofek (Jun 2023)
    % Example: X = rand(1000,1); Y=rand(1000,1); Val=rand(1000,1);
    %          Result = tools.math.stat.cell2d_stat(X,Y,Val)


    arguments
        X
        Y
        Val
        Args.Funs       = {@numel, @median, @mean, @tools.math.stat.rstd};
        Args.FunsArgs   = {{}, {'all','omitnan'}, {'all','omitnan'}, {'all'} };
        Args.FunsName   = {};
        Args.EdgesX     = [];
        Args.EdgesY     = [];
        Args.NbinX      = 5;
        Args.NbinY      = 5;
        Args.Eps        = 1e-8;
    end


    if isempty(Args.EdgesX)
        MinX   = min(X) - Args.Eps;
        MaxX   = max(X) + Args.Eps;
        StepX  = (MaxX - MinX)./Args.NbinX;
        EdgesX = (MinX:StepX:MaxX);
    else
        EdgesX = Args.EdgesX;
    end

    if isempty(Args.EdgesY)
        MinY   = min(Y) - Args.Eps;
        MaxY   = max(Y) + Args.Eps;
        StepY  = (MaxY - MinY)./Args.NbinY;
        EdgesY = (MinY:StepY:MaxY);
    else
        EdgesY = Args.EdgesY;
    end

    Nfun      = numel(Args.Funs);
    if isempty(Args.FunsName)
        FieldName = cell(Nfun,1);
        for Ifun=1:1:Nfun
            FieldName{Ifun} = func2str(Args.Funs{Ifun});
            if contains(FieldName{Ifun},'.')
                Tmp = split(FieldName{Ifun}, '.');
                FieldName{Ifun} = Tmp{end};
            end
        end
    else
        FieldNames = Args.FunsName;
    end

    NX = numel(EdgesX) - 1;
    NY = numel(EdgesY) - 1;
    for Ix=1:1:NX
        for Iy=1:1:NY
            Flag = X>EdgesX(Ix) & X<=EdgesX(Ix+1) & Y>EdgesY(Iy) & Y<=EdgesY(Iy+1);

            for Ifun=1:1:Nfun
                Result.(FieldName{Ifun})(Iy,Ix) = Args.Funs{Ifun}(Val(Flag), Args.FunsArgs{Ifun}{:});
            end
        end
    end


end
