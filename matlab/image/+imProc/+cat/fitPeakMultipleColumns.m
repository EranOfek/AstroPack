function [FitRes, Result] = fitPeakMultipleColumns(Obj, Args)
    %
    % Example: X=rand(100,3);
    %          [FitRes, Result] = imProc.cat.fitPeakMultipleColumns(X, 'Pos',[1 2 3])
   
    arguments
        Obj                                       % matrix | AstroImage | AstroCatalog
        Args.Cols                            = {'SN_1','SN_2','SN_3'};
        Args.Pos                             = [];                           % must have the same length as Cols
        Args.FlagBad logical                 = true; % modify MaxFitPos in cases there is no max solution, or solution is out of range
        Args.InsertBestVal logical           = true;
        Args.InsertBestInd logical           = true;
        Args.InsertMaxFitVal logical         = true;
        Args.InsertMaxFitPos logical         = true;
        Args.CreateNewObj logical            = false;
    end
    
    if numel(Args.Cols)~=numel(Args.Pos)
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
        
    Result = Obj;
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
        X_ext           = (-Par(2,:)./(2.*Par(3,:))).';
        SecDerivSign    = sign(Par(3,:)).'; 
        Flag.IsMax      = SecDerivSign<0;
        Flag.IsBelowMin = X_ext<Args.Pos(1);
        Flag.IsAboveMax = X_ext>Args.Pos(end);
        
        MaxFitPos          = X_ext;
        MaxFitVal          = Par(1) + X_ext.*(Par(2) + X_ext.*Par(3));   % second order polynomial
        [BestVal, BestInd] = max(Data,[],1);
        
        FitRes.MaxFitPos = MaxFitPos;
        FitRes.MaxFitVal = MaxFitVal;
        FitRes.BestVal   = BestVal.';
        FitRes.BestInd   = BestInd.';
        FitRes.Flag      = Flag;
        
        if Args.FlagBad
            % modify MaxFitPos in cases there is no max solution, or solution
            % is out of range
            FitRes.MaxFitPos(Flag.IsBelowMin) = Args.Pos(1);
            FitRes.MaxFitPos(Flag.IsAboveMax) = Args.Pos(end);
            FitRes.MaxFitPos(~Flag.IsMax)     = NaN;
        end
        
        % insert columns to output table
        % only if not a matrix
        if ~isnumeric(Obj)
            if  Args.InsertBestVal
                
            end
            if Args.InsertBestInd
                
            end
            if Args.InsertMaxFitVal
                
            end
            if Args.InsertMaxFitPos
                
            end
        end
    end
        
end
