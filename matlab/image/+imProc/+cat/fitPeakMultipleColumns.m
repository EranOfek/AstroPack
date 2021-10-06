function [FitRes, Result] = fitPeakMultipleColumns(Obj, Args)
    %
   
    arguments
        Obj                                       % matrix | AstroImage | AstroCatalog
        Args.Cols                            = {'SN_1','SN_2','SN_3'};
        Args.Pos                             = [];                           % must have the same length as Cols
        
        Args.InsertBestVal logical           = true;
        Args.InsertBestInd logical           = true;
        Args.InsertMaxFitVal logical         = true;
        Args.InsertMaxFitPos logical         = true;
        Args.CreateNewObj logical            = false;
    end
    
    if numel(Args.Col)~=numel(Args.Pos)
        error('Number of Cols must be equal to number of Pos');
    end
    
    % construct design matrix
    Args.Deg  = [0 1 2];   % degrees are hard coded - for min/max calculation
    X = Args.Pos(:);
    H = X.^(Args.Deg(:).');
    
    if isnumeric(Obj)
        Nobj = 1;
    else
        Nobj = numel(Obj);
    end
        
    for Iobj=1:1:Nobj
        % for each image/catalog
        if isa(Obj, 'AstroCatalog')
            if Args.CreateNewObj
                Cat  = Obj(Iobj).copy;
            else
                Cat  = Obj(Iobj);
            end
            Data = getCol(Cat, Args.Cols);
        elseif isa(Obj, 'AstroImage')
            if Args.CreateNewObj
                Cat  = Obj(Iobj).CatData.copy;
            else
                Cat  = Obj(Iobj).CatData;
            end
            Data = getCol(Cat, Args.Cols);
        else
            % assuming input is an array in which the columns corresponding
            % to Args.Cols
            Data = Obj;
        end
        Data = Data.';  % column per source
        
        Par = H\Data;
        X_ext        = -Par(2)./(2.*Par(3));
        SecDerivSign = sign(Par(3)); 
        if X_ext
        
    
end
