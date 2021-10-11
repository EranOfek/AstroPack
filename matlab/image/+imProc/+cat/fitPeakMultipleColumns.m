function [FitRes, Result] = fitPeakMultipleColumns(Obj, Args)
    % Given N columns with some property (e.g., S/N) fit a parabola
    %   to the S/N values as a function of some parameter (e.g., FWHM), and
    %   return the peak S/N value and position.
    %   The best fitted S/N and position, as well as the best S/N column
    %   value and index are optionally wrotten to the AstroCatalog.
    %   If the S/N is outside of bounderies or have minimum instead of
    %   maximum, return NaN (default).
    % Input  : - Either an AstroCatalog, AstroImage (with AstroCatalog), or
    %            a matrix.
    %          * ...,key,val,...
    %            'Cols' - Column names from which to get the values.
    %                   Default is {'SN_1','SN_2','SN_3'}.
    %            'Pos' - A vector of positions, correspinding to 'Cols'.
    %                   This paramaeter must be provided and length must
    %                   equal to the numbre of 'Cols'. Default is [].
    %            'FlagBad' - A logical indicating if to put NaNs in
    %                   MaxFitPos which is minima instaed of maxima, and to 
    %                   replace the MaxFitPos by the nearest bound if the position
    %                   is out of 'Pos' bounds. Default is true.
    %            'InsertBestVal' - Insert Best (max) value. Default is true.
    %            'InsertBestInd' - Insert the index of the column
    %                   corresponding to the best (max) value.
    %                   Default is true.
    %            'InsertMaxFitVal' - Inset fitted max value.
    %                   Default is true.
    %            'InsertMaxFitPos' -  Inset fitted max pos.
    %                   Default is true.
    %            'InsertErrMaxFitPos' - Insert error in fitted pos.
    %                   Default is true.
    %            'ColNameBestVal' - Best val column name.
    %                   Default is 'BestSN'.
    %            'ColNameBestInd' - Best column index.
    %                   Default is 'IndBestSN'.
    %            'ColNameMaxVal' - Fitted max value column name.
    %                   Default is 'FitBestSN'.
    %            'ColNameMaxPos' - Fitted max pos column name.
    %                   Default is 'FotPosBestSN'.
    %            'ColNameMaxPosErr' - Fitted max error pos column name.
    %                   Default is 'FitPosErrBestSN'.
    %            'Pos' - Position in which to insert the new columns.
    %                   Default is Inf.
    %            'CreateNewObj' - A logical indicating if to create a new
    %                   copy of the AstroCatalog (only) object.
    %                   Default is false.
    % Output : - A structure array with the following fields:
    %            'MaxFitPos' - Fitted pos.
    %            'MaxFitVal' - Fitted val.
    %            'ErrFitPos' - Error in fitted pos. Nan for maxima.
    %            'BestVal' - Best val.
    %            'BestInd' - Best ind.
    %            'Flag' - A structure with out of bound/min/max flags.
    %          - The same type as the input (but only AstroCatalog |
    %            AstroImage are supported). These contains the new columns.
    %            If CreateNewObj=true, only the Astrocatalog content is
    %            copied.
    % Author : Eran Ofek (Oct 2021)
    % Example: X=rand(100,3);
    %          [FitRes, Result] = imProc.cat.fitPeakMultipleColumns(X, 'Pos',[1 2 3])
    %          AC = AstroCatalog({X}, 'ColNames',{'SN_1','SN_2','SN_3'});
    %          [FitRes, Result] = imProc.cat.fitPeakMultipleColumns(AC, 'Pos',[1 2 3])
    %          AI=AstroImage('PTF_Cropped.fits');
    %          imProc.sources.findMeasureSources(AI, 'PsfFunPar',{[0.1; 1.2; 3]});
    %          [FitRes, Result] = imProc.cat.fitPeakMultipleColumns(AI, 'Pos',[0.1 1.2 3])

   
    arguments
        Obj                                       % matrix | AstroImage | AstroCatalog
        Args.Cols                            = {'SN_1','SN_2','SN_3'};
        Args.Pos                             = [];                           % must have the same length as Cols
        Args.FlagBad logical                 = true; % modify MaxFitPos in cases there is no max solution, or solution is out of range
        Args.InsertBestVal logical           = true;
        Args.InsertBestInd logical           = true;
        Args.InsertMaxFitVal logical         = true;
        Args.InsertMaxFitPos logical         = true;
        Args.InsertErrMaxFitPos logical      = true;
        Args.ColNameBestVal                  = 'BestSN';
        Args.ColNameBestInd                  = 'IndBestSN';
        Args.ColNameMaxVal                   = 'FitBestSN';
        Args.ColNameMaxPos                   = 'FitPosBestSN';
        Args.ColNameMaxPosErr                = 'FitPosErrBestSN';
        Args.ColPos                          = Inf;
        
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
        Nlines = size(Data,1);
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
        
        FitRes(Iobj).MaxFitPos = MaxFitPos;
        FitRes(Iobj).MaxFitVal = MaxFitVal;
        FitRes(Iobj).BestVal   = BestVal.';
        FitRes(Iobj).BestInd   = BestInd.';
        FitRes(Iobj).ErrFitPos = sqrt(abs(Par(3,:)));
        FitRes(Iobj).ErrFitPos(~Flag.IsMax) = NaN;
        
        FitRes(Iobj).Flag      = Flag;
        
        if Args.FlagBad
            % modify MaxFitPos in cases there is no max solution, or solution
            % is out of range
            FitRes(Iobj).MaxFitPos(Flag.IsBelowMin) = Args.Pos(1);
            FitRes(Iobj).MaxFitPos(Flag.IsAboveMax) = Args.Pos(end);
            FitRes(Iobj).MaxFitPos(~Flag.IsMax)     = NaN;
        end
        
        % insert columns to output table
        % only if not a matrix
        NnewCols = Args.InsertBestVal + Args.InsertBestInd + Args.InsertMaxFitVal + Args.InsertMaxFitPos + Args.InsertErrMaxFitPos;
        NewCols  = zeros(Nlines, NnewCols);
        ColNames = cell(1, NnewCols);
        ColUnits = cell(1, NnewCols);
        K = 0;
        if ~isnumeric(Obj)
            if  Args.InsertBestVal
                K = K + 1;
                NewCols(:,K) = FitRes(Iobj).BestVal;
                ColNames{K}  = Args.ColNameBestVal;
                ColUnuts{K}  = '';
            end
            if Args.InsertBestInd
                K = K + 1;
                NewCols(:,K) = FitRes(Iobj).BestInd;
                ColNames{K}  = Args.ColNameBestInd;
                ColUnuts{K}  = '';
            end
            if Args.InsertMaxFitVal
                K = K + 1;
                NewCols(:,K) = FitRes(Iobj).MaxFitVal;
                ColNames{K}  = Args.ColNameMaxVal;
                ColUnuts{K}  = '';
            end
            if Args.InsertMaxFitPos
                K = K + 1;
                NewCols(:,K) = FitRes(Iobj).MaxFitPos;
                ColNames{K}  = Args.ColNameMaxPos;
                ColUnuts{K}  = '';
            end
            if Args.InsertErrMaxFitPos
                K = K + 1;
                NewCols(:,K) = FitRes(Iobj).ErrFitPos;
                ColNames{K}  = Args.ColNameMaxPosErr;
                ColUnuts{K}  = '';
            end
                                
            if nargout>1
                Cat.insertCol(NewCols, Args.ColPos, ColNames, ColUnuts);
                if isa(Obj, 'AstroCatalog')
                    Result(Iobj) = Cat;
                elseif isa(Obj, 'AstroImage')
                    Result(Iobj).CatData = Cat;
                else
                    error('Matrix input does not support second output');
                end
            end
        end
    end
        
end
