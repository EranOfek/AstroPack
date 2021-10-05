function [FitRes, Result] = fitPeakMultipleColumns(Obj, Args)
    %
   
    arguments
        Obj
        Args.Cols = {'SN_1','SN_2','SN_3'};
        Args.Pos  = [];                           % must have the same length as Cols
        Args.InsertBestVal logical           = true;
        Args.InsertBestInd logical           = true;
        Args.InsertMaxFitVal logical         = true;
        Args.InsertMaxFitPos logical         = true;
    end
    
    
    
    
    
end
