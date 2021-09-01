function Result = mergeCatalogs(Obj, Args)
    %
    
    arguments
        Obj
        Args.MatchByXY(1,1) logical    = false;
    end
    
    % Match catalogs by RA/Dec or X/Y
    [MatchedObj, UnMatchedObj, TruelyUnMatchedObj] = imProc.match.match(Obj1, Obj2, Args)
    
    % Generate a matched sources matrix
    
    % fit PM
    
    % flag PM
    
    % relative photometry
    
    % photometric variability properties
    
    % Photometric template matching
    
    % treat unmatched sources
    
    % match to external catalogs
    
    
end