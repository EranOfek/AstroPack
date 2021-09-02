function Result = mergeCatalogs(Obj, Args)
    %
    
    arguments
        Obj
        Args.CooType                 = 'sphere';
        Args.unifiedSourcesCatalogArgs cell     = {};
        Args.matchArgs cell          = {};
        Args.MatchedColums           = {'RA','Dec','PSF_MAG','PSF_MAGERR','APER_MAG_1_','APER_MAGERR_1_','APER_MAG_2_','APER_MAGERR_2_'});
    end
    
    % find all unique sources
    AllSources = imProc.match.unifiedSourcesCatalog(Obj, 'CooType',Args.CooType, Args.unifiedSourcesCatalogArgs{:});
    
    % Match catalogs by RA/Dec or X/Y
    [MatchedObj, UnMatchedObj, TruelyUnMatchedObj] = imProc.match.match(Obj, AllSources, 'CooType',Args.CooType, Args.matchArgs{:});
    
    % Generate a matched sources matrix
    MatchedS = MatchedSources;
    MatchedS.addMatrix(MatchedObj, Args.MatchedColums);
    
    % fit PM
    
    
    % flag PM
    
    % relative photometry
    
    % photometric variability properties
    
    % Photometric template matching
    
    % treat unmatched sources
    
    % match to external catalogs
    
    
end